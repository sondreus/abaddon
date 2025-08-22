# 0) Install & load necessary packages
if (!requireNamespace("rstan", quietly=TRUE)) {
  install.packages("rstan", dependencies=TRUE)
}
library(rstan); rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())
library(dplyr)
library(stringr)
library(readr)
library(anytime)
library(pROC)
library(scales)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1) Raw data: a data.frame `df` with these columns:
#    id, match_id, win (0/1), ml_prob (in [0,1])
#    and ANY id with >=2 underscores is a composition ID.
# ------------------------------------------------------------------------------
# Load splits
source('scripts/aux_update_and_load_splits.R')

# Load data
df <- read_csv("output-data/df_feat_and_ml.csv", show_col_types = FALSE)

# Get latent skill split:
df <- df[df$match_id %in% as.numeric(splits %>% filter(split == 'latent_skill_model') %>% select(match_id) %>% unlist()), ]

# Exclude matches which only include players or compositions which too few observations to be modeled individually:
not_collapsed <- unique(df$match_id[!as.logical(df$collapsed)])
df <- df %>% filter(match_id %in% not_collapsed)

# Cut-off for betting simulation (now set to middle of sample)
# df <- df[anytime(df$start_time, calcUnique = T) > as.Date('2025-07-22'), ]

# First, collapse patches with insufficient matches
min_matches <- 400
patches <- rev(sort(unique(df$patch)))  # newest to oldest

# Collapse insufficient patches into newer ones
for (i in 2:length(patches)) {
  n_matches <- sum(df$patch == patches[i]) / 12
  if (n_matches < min_matches) {
    cat(sprintf("Collapsing patch %s into %s (%.0f matches)\n", patches[i], patches[i-1], n_matches))
    df$patch[df$patch == patches[i]] <- patches[i-1]
  }
}

# Check first (newest) patch separately
patches <- rev(sort(unique(df$patch)))  # refresh after collapsing
if (length(patches) > 1) {
  n_matches_first <- sum(df$patch == patches[1]) / 12
  if (n_matches_first < min_matches) {
    cat(sprintf("Collapsing patch %s into %s (%.0f matches)\n", patches[i], patches[i-1], n_matches))
    df$patch[df$patch == patches[1]] <- patches[2]
  }
}

# NOW collapse old patches, but only if we have more than 3 patches remaining
patches_after_collapse <- rev(sort(unique(df$patch)))
num_recent_to_keep <- 3

if (length(patches_after_collapse) > num_recent_to_keep) {
  # The patch to collapse older ones into (3rd most recent)
  collapse_target <- patches_after_collapse[num_recent_to_keep]
  
  # Find patches older than the 3rd most recent
  patches_to_collapse <- patches_after_collapse[(num_recent_to_keep + 1):length(patches_after_collapse)]
    
  # Collapse them
  df$patch[df$patch %in% patches_to_collapse] <- collapse_target
  message(sprintf('Patches older than the %d most recent collapsed into patch %s to speed up convergence.', 
                  num_recent_to_keep, collapse_target))
}

# Ensure patches are integers to avoid lexicographic issues
df$patch <- as.integer(df$patch)
stopifnot(all(!is.na(df$patch)))

# 2) Flag compositions vs individual players
df <- df %>%
  mutate(is_comp = str_count(id, "_") >= 2)

# Note: we use model_id as preferred ID. This has collapsed non-pro players and those with very few games into groups (and same for compositions).

# 3) Build numeric mappings
ids <- sort(unique(df$model_id[!df$is_comp]))
comp_ids   <- sort(unique(df$model_id[df$is_comp ]))
player_index <- setNames(seq_along(ids), ids)
comp_index   <- setNames(seq_along(comp_ids),   comp_ids)

# Generate a "comp" description vector
is_lineup_id <- function(x) grepl("^\\d+_\\d+_\\d+_\\d+_\\d+$", x)
comp_is_lineup <- is_lineup_id(comp_ids)
names(comp_is_lineup) <- comp_ids

# choose a default player bucket to stand in for bucketed compositions
default_player_bucket <- if ("pro_lowdata" %in% ids) {
  "pro_lowdata"
} else if ("nonpro_lowdata" %in% ids) {
  "nonpro_lowdata"
} else {
  ids[1]  # safe fallback
}
default_idx <- unname(player_index[[default_player_bucket]])

# 4) Dimensions
match_ids <- sort(unique(df$match_id))
M <- length(match_ids)
P <- length(ids)
C <- length(comp_ids)

# 4b.1) get the unique patch values (could be integers, strings, whatever)
patch_values <- sort(unique(as.integer(df$patch)))
K            <- length(patch_values)

# 4b.2) for each match, pull its patch value (take the first row for that match)
patch_per_match <- sapply(match_ids, function(mid) {
  df$patch[df$match_id==mid][1]
})

# 4b.3) integer‐code them 1…K
patch_id <- match(patch_per_match, patch_values)

# sanity check: no NAs
stopifnot(length(patch_id)==M, all(!is.na(patch_id)))

# 5) Allocate Stan inputs
home_players <- matrix(NA_integer_, nrow=M, ncol=5)
away_players <- matrix(NA_integer_, nrow=M, ncol=5)
home_comp    <- integer(M)
away_comp    <- integer(M)
win_vec      <- integer(M)
ml_prob_vec  <- numeric(M)

# randomly (but reproducibly) set side A and B.
set.seed(123)                            # reproducible "random"
flip_home_is_winner <- sample(c(TRUE, FALSE), M, replace = TRUE)

# 6) Fill them match by match
for (i in seq_along(match_ids)) {
  mid <- match_ids[i]
  sub <- df %>% filter(match_id == mid)
  comp_sub   <- sub %>% filter(is_comp)
  player_sub <- sub %>% filter(!is_comp)
  
  win_flags <- comp_sub$win
  stopifnot(all(win_flags %in% c(0,1)), length(win_flags)==2)
  winner_i <- which(win_flags == 1)
  loser_i  <- which(win_flags == 0)
  
  # Random orientation per match, but reproducible:
  home_is_winner <- flip_home_is_winner[i]
  
  if (home_is_winner) {
    # home = winner, away = loser
    home_comp[i]   <- unname(comp_index[ comp_sub$model_id[winner_i] ])
    away_comp[i]   <- unname(comp_index[ comp_sub$model_id[loser_i]  ])
    win_vec[i]     <- 1L
    ml_prob_vec[i] <- as.numeric(comp_sub$ml_prob[winner_i])
    
    p_idx  <- unname(player_index[player_sub$model_id])
    home_p <- p_idx[player_sub$win == 1]
    away_p <- p_idx[player_sub$win == 0]
  } else {
    # home = loser, away = winner
    home_comp[i]   <- unname(comp_index[ comp_sub$model_id[loser_i]  ])
    away_comp[i]   <- unname(comp_index[ comp_sub$model_id[winner_i] ])
    win_vec[i]     <- 0L
    ml_prob_vec[i] <- as.numeric(comp_sub$ml_prob[loser_i])
    
    p_idx  <- unname(player_index[player_sub$model_id])
    home_p <- p_idx[player_sub$win == 0]
    away_p <- p_idx[player_sub$win == 1]
  }
  
  stopifnot(length(home_p) == 5, length(away_p) == 5)
  home_players[i,] <- sort(home_p)
  away_players[i,] <- sort(away_p)
}

# 6b) Build player-specific ML‐prob vectors
player_rows    <- df %>% filter(!is_comp) %>% arrange(match_id, model_id)
Np             <- nrow(player_rows)                   # should be 10 * M
player_id_vec  <- player_index[player_rows$model_id]        # length Np
ml_prob_player <- player_rows$ml_prob                  # length Np

# Add clamp:
eps <- 1e-6
ml_prob_vec  <- pmin(pmax(ml_prob_vec,  eps), 1 - eps)
ml_prob_player <- pmin(pmax(ml_prob_player, eps), 1 - eps)

# 7) Package for Stan (add the new entries)
stan_data <- list(
  M               = M,
  P               = P,
  C               = C,
  Np              = Np,
  home_players    = home_players,
  away_players    = away_players,
  home_comp       = home_comp,
  away_comp       = away_comp,
  win             = win_vec,
  ml_prob         = ml_prob_vec,
  player_id      = player_id_vec,
  ml_prob_player = ml_prob_player,
  K        = K,
  patch_id = patch_id
)

#-------------------------------------------------------------------------------
# 8) Stan model: non-centered player & comp effects + Beta on ml_prob
#-------------------------------------------------------------------------------
stan_code <- "
data {
  int<lower=1> M;                          // matches
  int<lower=1> P;                          // players
  int<lower=1> C;                          // compositions
  int<lower=1> K;                          // patches
  int<lower=1,upper=K> patch_id[M];        // patch index per match
  int<lower=1> Np;                         // player‐rows
  int<lower=1,upper=P> home_players[M,5];
  int<lower=1,upper=P> away_players[M,5];
  int<lower=1,upper=C> home_comp[M];
  int<lower=1,upper=C> away_comp[M];
  int<lower=0,upper=1> win[M];
  real<lower=0,upper=1> ml_prob[M];
  int<lower=1,upper=P> player_id[Np];
  real<lower=0,upper=1> ml_prob_player[Np];
}

parameters {
  // static skills
  vector[P]     z_player;
  real<lower=0> sigma_player;
  vector[C]     z_comp_raw;
  real<lower=0> sigma_comp;

  // patch×comp innovations for anchored random walk (patch 1 = 0)
  matrix[K-1, C] patch_comp_innov;   // innovations for k = 2..K
  real<lower=0> sigma_patch_comp;

  // Global outcome calibration
  real alpha_win;
  real<lower=0> beta_win;

  // Decoupled ML-prob calibration (global)
  real alpha_ml;
  real<lower=0> beta_ml;

  // Beta precisions
  real<lower=0> phi_ml;       // for ml_prob
  real<lower=0> phi_player;   // for ml_prob_player
}

transformed parameters {
  vector[P] player_skill_raw = z_player * sigma_player;
  vector[P] player_skill = player_skill_raw - mean(player_skill_raw); // sum-to-zero
  // mean-zero constraint on comp effects
  vector[C] comp_eff = sigma_comp * (z_comp_raw - mean(z_comp_raw));
  
  // patch×comp deviations: anchored RW with patch 1 = 0 by construction
  matrix[K, C] patch_comp_eff;
  for (c in 1:C) {
    patch_comp_eff[1, c] = 0; // anchor
    for (k in 2:K)
      patch_comp_eff[k, c] = patch_comp_eff[k-1, c]
                           + sigma_patch_comp * patch_comp_innov[k-1, c];
  }
}

model {
  // Priors
  z_player     ~ normal(0,1);
  z_comp_raw   ~ normal(0,1);
  sigma_player ~ normal(0,2);
  sigma_comp   ~ normal(0,3);

  sigma_patch_comp ~ normal(0,1.5);
  to_vector(patch_comp_innov) ~ normal(0,1);  // RW innovations

  // Global priors for outcome calibration
  alpha_win ~ normal(0,1);
  beta_win  ~ lognormal(0,0.5);

  // Priors for ML-prob calibration
  alpha_ml  ~ normal(0,1);
  beta_ml   ~ lognormal(0,0.5);

  // Precisions
  phi_ml       ~ gamma(5,1);
  phi_player   ~ gamma(5,1);

  // Likelihoods
  for (m in 1:M) {
    real S_home = mean(player_skill[home_players[m]]) +
                  comp_eff[home_comp[m]] +
                  patch_comp_eff[patch_id[m], home_comp[m]];
    real S_away = mean(player_skill[away_players[m]]) +
                  comp_eff[away_comp[m]] +
                  patch_comp_eff[patch_id[m], away_comp[m]];
    real d      = S_home - S_away;

    // (1) Outcome with global calibration
    win[m] ~ bernoulli_logit(alpha_win + beta_win * d);

    // (2) ML-prob channel with its own calibration
    real mu_ml = inv_logit(alpha_ml + beta_ml * d);
    ml_prob[m] ~ beta(mu_ml * phi_ml, (1 - mu_ml) * phi_ml);
  }

  // Player-level ML-probs
  for (n in 1:Np) {
    ml_prob_player[n]
      ~ beta(inv_logit(player_skill[player_id[n]]) * phi_player,
             (1 - inv_logit(player_skill[player_id[n]])) * phi_player);
  }
}
"

#-------------------------------------------------------------------------------
# 9) Compile & fit
#-------------------------------------------------------------------------------
# Parameter counting for your Stan model
# Based on your stan_data structure
cat("=== STAN MODEL PARAMETER COUNT ===\n\n")

# Extract dimensions
M <- stan_data$M
P <- stan_data$P
C <- stan_data$C
K <- stan_data$K

cat("Data dimensions:\n")
cat(sprintf("  Matches (M): %d\n", M))
cat(sprintf("  Players (P): %d\n", P))
cat(sprintf("  Compositions (C): %d\n", C))
cat(sprintf("  Patches (K): %d\n\n", K))

cat("Parameter counts:\n")

z_player_params <- P
z_comp_params   <- C  # raw comps
patch_comp_eff_params <- K * C

# scalar hyperparameters: list explicitly
scalar_params <- c(
  sigma_player     = 1,
  sigma_comp       = 1,
  sigma_patch_comp = 1,
  phi_ml           = 1,
  phi_player       = 1,
  alpha_win        = 1,
  beta_win         = 1,
  alpha_ml         = 1,
  beta_ml          = 1
)

cat("Parameter counts:\n")
cat(sprintf("  z_player: %d\n", z_player_params))
cat(sprintf("  z_comp_raw: %d\n", z_comp_params))
cat(sprintf("  patch_comp_eff (K × C): %d × %d = %d\n", K, C, patch_comp_eff_params))
cat(sprintf("  Scalars: %d\n", sum(scalar_params)))

player_skill_params <- P
comp_eff_params     <- C

total_params <- z_player_params + z_comp_params + patch_comp_eff_params + sum(scalar_params) + player_skill_params + comp_eff_params
cat(sprintf("\nTOTAL PARAMETERS: %d\n", total_params))

# Comparison to uncollapsed
cat("\n=== COMPARISON TO UNCOLLAPSED ===\n")
original_players <- length(unique(df$id[!df$is_comp]))
original_comps   <- length(unique(df$id[df$is_comp]))
original_z_player    <- original_players
original_z_comp      <- original_comps
original_patch_comp  <- K * original_comps
original_total <- original_z_player + original_z_comp + original_patch_comp + sum(scalar_params) +comp_eff_params+player_skill_params

cat(sprintf("Original players: %d\n", original_players))
cat(sprintf("Original compositions: %d\n", original_comps))
cat(sprintf("Original total parameters: %d\n", original_total))

reduction <- 1 - (total_params / original_total)
cat(sprintf("Parameter reduction: %.1f%%\n", reduction * 100))

# Crude timing bucket
est_time <- if (total_params < 1000) "Fast (minutes)" else
  if (total_params < 5000) "Moderate (tens of minutes to hours)" else
    if (total_params < 20000) "Slow (hours)" else "Very slow (many hours+)"
cat(sprintf("\nTOTAL PARAMETERS: %d\n", total_params))
cat(sprintf("Estimated sampling time: %s\n", est_time))

# Breakdown table (include patch calibration)
cat("\n=== PARAMETER BREAKDOWN ===\n")
breakdown <- data.frame(
    Category = c("z_player (static)", "z_comp (static)",
                                 "patch_comp_eff (K×C)",
                                 "Scalar hyperparams"),
    Count = c(z_player_params, z_comp_params,
                           patch_comp_eff_params,
                           scalar_params)
)
breakdown$Percentage <- 100 * breakdown$Count / total_params
print(breakdown)

cat("\n\n=== STAN MODEL BEGINS TRAINING ===\n\n")
print(start_time <- Sys.time())
fit <- stan(
  model_code = stan_code,
  data       = stan_data,
  iter       = 4000,
  warmup     = 2000,
  chains     = 4,
  cores      = 4,
  refresh    = 0,
  control    = list(adapt_delta = 0.95)
)
print(end_time <- Sys.time())
cat("\n\n=== STAN MODEL TRAINING ENDED ===\n\n")
saveRDS(list(fit, start_time, end_time, "total_params" = total_params, "match_ids" = unique(df$match_id)), paste0('Fit_timing_param_count_n_matches_', Sys.time(), '.RDS'))

# Save posterior draws & metadata for later win‐prob function
post <- rstan::extract(fit)

library(anytime)
posterior_filename <- paste0('Posteriors_fit_', Sys.time(), '.RDS')
saveRDS(
  list(
    post          = post,
    stan_data     = stan_data,
    comp_ids      = comp_ids,
    ids            = ids,     
    patch_values  = patch_values,
    most_recent_match = max(anytime(df$start_time)),
    comp_is_lineup       = comp_is_lineup,          
    default_idx          = default_idx,              
    default_player_bucket= default_player_bucket,
    calibration="global"
  ),
  file = posterior_filename
)

#-------------------------------------------------------------------------------
# 10) Extract & post-process
#-------------------------------------------------------------------------------
# Example output:
if(F){
  # Calculate mean skills for ranking
  mean_player_skills <- colMeans(post$player_skill)
  mean_comp_skills <- colMeans(post$comp_eff)
  
  # Get top 5 players by skill
  top_players_idx <- order(mean_player_skills, decreasing = TRUE)[1:5]
  top_players_skills <- mean_player_skills[top_players_idx]
  
  cat("=== TOP 5 PLAYERS BY SKILL ===\n")
  for(i in 1:5) {
    player_idx <- top_players_idx[i]
    player_id <- ids[player_idx]
    skill <- top_players_skills[i]
    # Get 90% credible interval
    skill_samples <- post$player_skill[, player_idx]
    ci_low <- quantile(skill_samples, 0.05)
    ci_high <- quantile(skill_samples, 0.95)
    
    cat(sprintf("%d. Player ID: %s | Skill: %.3f (90%% CI: %.3f, %.3f)\n", 
                i, player_id, skill, ci_low, ci_high))
  }
  
  # Get top 5 compositions by skill  
  top_comps_idx <- order(mean_comp_skills, decreasing = TRUE)[1:5]
  top_comps_skills <- mean_comp_skills[top_comps_idx]
  
  cat("\n=== TOP 5 COMPOSITIONS BY SKILL ===\n")
  for(i in 1:5) {
    comp_idx <- top_comps_idx[i]
    comp_id <- comp_ids[comp_idx]
    skill <- top_comps_skills[i]
    # Get 90% credible interval
    skill_samples <- post$comp_eff[, comp_idx]
    ci_low <- quantile(skill_samples, 0.05)
    ci_high <- quantile(skill_samples, 0.95)
    
    cat(sprintf("%d. Composition ID: %s | Skill: %.3f (90%% CI: %.3f, %.3f)\n", 
                i, comp_id, skill, ci_low, ci_high))
  }
  
  # Find match with highest skilled player
  highest_skilled_player_idx <- which.max(mean_player_skills)
  highest_skilled_player_id <- ids[highest_skilled_player_idx]
  
  # Find matches containing this player
  matches_with_top_player <- which(apply(home_players, 1, function(row) highest_skilled_player_idx %in% row) |
                                     apply(away_players, 1, function(row) highest_skilled_player_idx %in% row))
  
  if(length(matches_with_top_player) > 0) {
    # Pick the first match with the highest skilled player
    i_match <- matches_with_top_player[1]
    
    cat(sprintf("\n=== MATCH WITH HIGHEST SKILLED PLAYER ===\n"))
    cat(sprintf("Match #%d contains highest skilled player: %s (skill: %.3f)\n", 
                i_match, highest_skilled_player_id, mean_player_skills[highest_skilled_player_idx]))
  } else {
    # Fallback to random match if somehow no matches found
    set.seed(42)
    i_match <- sample(seq_len(M), 1)
    cat(sprintf("\n=== RANDOM MATCH (no matches found with top player) ===\n"))
  }
  
  # 12) Get the two lineups and comps for that match
  home_pl <- home_players[i_match, ]    # 5‐vector of player‐indices
  away_pl <- away_players[i_match, ]
  home_co <- home_comp[i_match]         # single comp‐index
  away_co <- away_comp[i_match]
  
  # 13) Compute posterior draws of latent skill
  S_home <- rowMeans(post$player_skill[, home_pl]) + post$comp_eff[, home_co] +  post$patch_comp_eff[, patch_id[i_match], home_co]
  
  S_away <- rowMeans(post$player_skill[, away_pl]) + post$comp_eff[, away_co] +  post$patch_comp_eff[, patch_id[i_match], away_co]
  
  # Show match details
  cat(sprintf("\nMatch #%d details:\n", i_match))
  cat("Home players: ", paste(ids[home_pl], collapse = ", "), "\n")
  cat("Away players: ", paste(ids[away_pl], collapse = ", "), "\n")
  cat("Home composition: ", comp_ids[home_co], "\n")
  cat("Away composition: ", comp_ids[away_co], "\n")
  
  # Calculate and show predicted probability
  prob_home_wins <- mean(plogis(post$alpha_win + post$beta_win * (S_home - S_away)))
  
  cat(sprintf("Predicted P(Home wins): %.3f\n", prob_home_wins))
  
  # 14) Put into a data.frame for ggplot
  plot_df <- data.frame(
    Skill = c(S_home, S_away),
    Team  = rep(c("Home","Away"), each = length(S_home))
  )
  
  # 15) Plot densities
  library(ggplot2)
  ggplot(plot_df, aes(x = Skill, fill = Team)) +
    geom_density(alpha = 0.5) +
    theme_minimal() +
    labs(
      title = paste0(
        "Match #", i_match, ": Home vs Away\n",
        "Home players = ", paste(ids[home_pl], collapse = ","),
        " | Away players = ", paste(ids[away_pl], collapse = ",")
      ),
      x = "Latent Skill",
      y = "Density"
    )
  
  # Bonus: Show which team has the highest skilled player if it's in this match
  if(highest_skilled_player_idx %in% home_pl) {
    cat(sprintf("\n*** Highest skilled player (%s) is on HOME team! ***\n", highest_skilled_player_id))
  } else if(highest_skilled_player_idx %in% away_pl) {
    cat(sprintf("\n*** Highest skilled player (%s) is on AWAY team! ***\n", highest_skilled_player_id))
  }
  
  # 16) Plot player‐skill densities for the 10 players in the selected match
  library(tidyr)
  library(pROC)
  library(scales)
  
  # Get the 10 player indices and their names
  players       <- c(home_players[i_match, ], away_players[i_match, ])
  player_names  <- ids[players]
  
  # Extract posterior draws for those players
  # post$player_skill is [draws × P]
  draws_mat     <- post$player_skill[, players]
  colnames(draws_mat) <- player_names
  
  # Reshape to long for ggplot
  player_df <- as.data.frame(draws_mat) %>%
    pivot_longer(cols      = everything(),
                 names_to  = "Player",
                 values_to = "Skill")
  
  # Plot density for each player
  ggplot(player_df, aes(x = Skill, fill = Player)) +
    geom_density(alpha = 0.5) +
    theme_minimal() +
    labs(
      title = paste0("Posterior Skill Distributions for Players in Match #", match_ids[i_match]),
      x     = "Latent Skill",
      y     = "Density"
    ) +
    theme(legend.position = "right")
}

# End) Calibration: bin predicted probabilities vs actual outcomes
n_draws <- nrow(post$player_skill)
p_draws <- matrix(NA_real_, nrow = n_draws, ncol = M)

for (m in seq_len(M)) {
  # per‐draw home/away skill difference
  S_home <- rowMeans(post$player_skill[, home_players[m, ]]) +
    post$comp_eff[, home_comp[m]] +
    post$patch_comp_eff[, patch_id[m], home_comp[m]]
  S_away <- rowMeans(post$player_skill[, away_players[m, ]]) +
    post$comp_eff[, away_comp[m]] +
    post$patch_comp_eff[, patch_id[m], away_comp[m]]
  p_draws[, m] <- plogis(post$alpha_win + post$beta_win * (S_home - S_away))
  }

# 2) Now either
#  a) Compute the usual point‐estimate with means:
p_hat    <- colMeans(p_draws)

#  b) Or sample one draw *per* match to build a single “posterior‐realization”:
set.seed(2025)
which_draw <- sample(seq_len(n_draws), size = M, replace = TRUE)
p_sampled  <- p_draws[cbind(which_draw, seq_len(M))]

# 3) Build two calibration data‐sets:
calib_mean <- tibble(match_id = match_ids,
                     win       = win_vec,
                     win_prob  = p_hat)

calib_samp <- tibble(match_id = match_ids,
                     win       = win_vec,
                     win_prob  = p_sampled)

# 4) Compare AUC/Brier on both:
for (dat in list(mean = calib_mean, samp = calib_samp)) {
  cat("----", names(dat), "----\n")
  auc_v   <- auc(roc(dat$win, dat$win_prob))
  brier_v <- mean((dat$win - dat$win_prob)^2)
  cat("AUC   =", round(auc_v,3),
      "  Brier =", round(brier_v,3), "\n\n")
}

# 5) Make a reliability diagram on the sampled version:
bins <- 10
calib_samp %>%
  mutate(bin = cut_number(win_prob, n = bins)) %>%
  group_by(bin) %>%
  summarise(mean_pred = mean(win_prob),
            obs_rate  = mean(win),
            n         = n()) %>%
  ggplot(aes(mean_pred, obs_rate, size = n)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal() +
  scale_x_continuous("Predicted P(home win)", labels = percent_format()) +
  scale_y_continuous("Observed win rate",   labels = percent_format()) +
  scale_size_area("Matches\nper bin") +
  theme_minimal() +
  labs(title = "Reliability (one random posterior draw per match)")+geom_smooth(se = F, method = 'lm', size = 1, col='blue')
 
# # ALT: 6) Make a reliability diagram on the mean-win-prob version:
# bins <- 10
# calib_mean %>%
#   mutate(bin = cut_number(win_prob, n = bins)) %>%
#   group_by(bin) %>%
#   summarise(mean_pred = mean(win_prob),
#             obs_rate  = mean(win),
#             n         = n()) %>%
#   ggplot(aes(mean_pred, obs_rate, size = n)) +
#   geom_point(alpha = 0.7) +
#   geom_abline(slope = 1, intercept = 0, linetype = "das hed") +
#   coord_equal() +
#   scale_x_continuous("Predicted P(home win)", labels = percent_format()) +
#   scale_y_continuous("Observed win rate",   labels = percent_format()) +
#   scale_size_area("Matches\nper bin") +
#   theme_minimal() +
#   labs(title = "Reliability (one random posterior draw per match)")+geom_smooth(se = F, method = 'lm', size = 1, col='blue')
# 

source('scripts/aux-save-diagnostics.R')
save_diagnostics()

source('scripts/aux-future-matches-validation.R')
results <- validate_on_future_matches(posterior_filename, max_weeks_ahead = 4)
# 
# # Platt scaling on OoS set
# m <- glm(win ~ qlogis(win_prob), data = results$calib_mean, family = binomial())
# p_platt <- plogis(predict(m, newdata = data.frame(win_prob = results$calib_mean$win_prob), type = "link"))
# auc_platt   <- pROC::auc(pROC::roc(results$calib_mean$win, p_platt, quiet=TRUE))
# brier_platt <- mean((results$calib_mean$win - p_platt)^2)
