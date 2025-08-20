#!/usr/bin/env Rscript
# win_probability.R
library(readr)
library(tidyverse)
library(anytime)   # used below for date filters
library(pROC)
library(boot)

# Flexible Kelly betting implementation
# Set betting strategy
one_sided_betting <- F  # Set to FALSE for two-sided betting
team_to_bet <- 1          # Only used if one_sided_betting = TRUE (1 or 2)

# 0) (Optional) change this to where you actually saved the posteriors
# rds_file <- sample(dir()[grep('Posteriors_', dir())], 1)
rds_file <- posterior_filename

# 1) Read in your saved posterior & data
res          <- readRDS(rds_file)
post         <- res$post
ids          <- res$ids
comp_ids     <- res$comp_ids
patch_values <- res$patch_values
most_recent_match_in_data <- res$most_recent_match

# Helper for NULL-coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

# Load bucket-aware helpers if present; otherwise set safe fallbacks
comp_is_lineup <- res$comp_is_lineup %||% setNames(grepl("^\\d+_\\d+_\\d+_\\d+_\\d+$", comp_ids), comp_ids)

default_idx <- res$default_idx
if (is.null(default_idx) || is.na(default_idx)) {
  di <- match("pro_lowdata", ids)
  if (is.na(di)) di <- match("nonpro_lowdata", ids)
  if (is.na(di)) di <- 1L
  default_idx <- di
}

# 2) The main function (bucket-aware)
win_prob <- function(comp1, comp2, patch, best_of = 1) {
  # 1) sanity checks
  if (!comp1 %in% comp_ids) stop("'", comp1, "' not found in comp_ids.")
  if (!comp2 %in% comp_ids) stop("'", comp2, "' not found in comp_ids.")
  if (!(best_of %in% c(1,3,5,7))) stop("best_of must be one of 1, 3, 5 or 7.")
  if (patch > max(patch_values)) {
    patch <- patch - 1
    message('Most recent patch not in training data, predicting as-if previous patch')
  }
  pidx <- match(patch, patch_values)
  if (is.na(pidx)) stop("Patch '", patch, "' not in patch_values.")
  
  # 3) composition & patch effects
  c1 <- which(comp_ids == comp1)
  c2 <- which(comp_ids == comp2)
  comp1_eff  <- post$comp_eff[, c1]
  comp2_eff  <- post$comp_eff[, c2]
  patch1_eff <- post$patch_comp_eff[, pidx, c1]
  patch2_eff <- post$patch_comp_eff[, pidx, c2]
  
  # 4) player skills, guarding for bucketed comps
  is_lineup <- function(comp) {
    if (comp %in% names(comp_is_lineup)) return(comp_is_lineup[[comp]])
    grepl("^\\d+_\\d+_\\d+_\\d+_\\d+$", comp)
  }
  mk_idx <- function(comp) {
    if (is_lineup(comp)) {
      p <- strsplit(comp, "_", fixed = TRUE)[[1]]
      idx <- match(p, ids)
      idx[is.na(idx)] <- default_idx
      if (length(idx) != 5L) rep(default_idx, 5L) else idx
    } else {
      rep(default_idx, 5L)  # bucketed comp → neutral proxy
    }
  }
  idx1 <- mk_idx(comp1)
  idx2 <- mk_idx(comp2)
  
  # post$player_skill is [draws × P]
  home_player_skill <- rowMeans(post$player_skill[, idx1, drop = FALSE])
  away_player_skill <- rowMeans(post$player_skill[, idx2, drop = FALSE])
  
  # 5) per‐draw one-game win prob
  d_eff   <- (home_player_skill + comp1_eff + patch1_eff) -
    (away_player_skill + comp2_eff + patch2_eff)

  # Outcome calibration: handle both global and hierarchical
  if ("alpha_win" %in% names(post) && "beta_win" %in% names(post)) {
    calib_type <- res$calibration %||% "hierarchical"
    if (identical(calib_type, "global")) {
      p_draws <- plogis(post$alpha_win + post$beta_win * d_eff)
      } else {
        k_patch <- pidx  # from earlier match(patch, patch_values)
        p_draws <- plogis(post$alpha_win[, k_patch] +
                            post$beta_win[, k_patch] * d_eff)
        }
    } else {
      p_draws <- plogis(d_eff)
      }
  
  if (best_of == 1) return(mean(p_draws))
  
  # 6) series win prob via binomial tail
  n            <- best_of
  wins_needed  <- floor(n/2) + 1
  series_prob  <- function(p) sum(dbinom(wins_needed:n, size = n, prob = p))
  p_series     <- vapply(p_draws, series_prob, numeric(1))
  mean(p_series)
}

# 3) Example usage:
# cat("P(compA beats compB in BO3 on patch 58): ",
#     win_prob("99796146_173842160_180775030_252520776_421515107",
#              "97658618_100594231_241884166_324277900_1150772339", 
#              "58", best_of = 3), "\n")

# 4) Get probabilities for a bunch of matches
bet_data <- dir('/Users/sondresolstad/Github/scrapeman/')
bet_data <- sort(bet_data[grepl('betting_set', bet_data)], decreasing = TRUE)[1]
betting_set <- read_csv(paste0('/Users/sondresolstad/Github/scrapeman/', bet_data))

betting_set <- betting_set %>% 
  filter(match_id != '') %>%                                   # exclude matches with no data 
  filter(anytime(first_game_start) >  most_recent_match_in_data) %>% 
  filter(anytime(first_game_start) <=  most_recent_match_in_data + days(14)) %>% 
  mutate(
    betting_implied_prob = (1 / odds_team1_decimal) /
      ((1 / odds_team1_decimal) + (1 / odds_team2_decimal)),
    team_1_won = ifelse(winner == "1", 1, 0)
  )

# Add estimated probability:
betting_set$estimated_probability <- NA_real_

for (i in seq_len(nrow(betting_set))) {
  best_of_type <- NA_integer_
  if (betting_set$result[i] %in% c('0 - 2', '2 - 0', '2 - 1', '1 - 2')) {
    best_of_type <- 3L
  } 
  if (betting_set$result[i] %in% c('3 - 0', '0 - 3', '1 - 3', '3 - 1', '3 - 2', '2 - 3')) {
    best_of_type <- 5L
  }
  betting_set$estimated_probability[i] <- tryCatch(
    win_prob(betting_set$composition_team1[i],
             betting_set$composition_team2[i],
             betting_set$patch[i],
             best_of = best_of_type),
    error = function(e) NA_real_
  )
}

betting_set <- betting_set %>% 
  filter(!is.na(betting_implied_prob), !is.na(estimated_probability))

# Quick scatter & correlation
print(ggplot(betting_set, aes(y = estimated_probability, x = betting_implied_prob)) + 
        geom_point())
print(cor(betting_set[, c('estimated_probability','betting_implied_prob','team_1_won')]))

# AUC / Brier
auc_model   <- auc(betting_set$team_1_won, betting_set$estimated_probability)
auc_implied <- auc(betting_set$team_1_won, betting_set$betting_implied_prob)
brier_model   <- mean((betting_set$team_1_won - betting_set$estimated_probability)^2)
brier_implied <- mean((betting_set$team_1_won - betting_set$betting_implied_prob)^2)

print(data.frame(
  Method      = c("Your model", "Implied odds"),
  AUC         = c(as.numeric(auc_model), as.numeric(auc_implied)),
  Brier_Score = c(brier_model,           brier_implied)
))

# --- Single, two-sided Kelly (fixes “double Kelly” and one-sided betting) -----
kelly_frac <- function(p, o) pmax((p * (o - 1) - (1 - p)) / (o - 1), 0)

betting_set <- betting_set %>%
  arrange(match_date) %>%
  mutate(
    kelly_team1 = kelly_frac(estimated_probability, odds_team1_decimal),
    kelly_team2 = kelly_frac(1 - estimated_probability, odds_team2_decimal),
    
    # Choose betting side based on strategy
    bet_side = case_when(
      one_sided_betting & team_to_bet == 1 ~ ifelse(kelly_team1 > 0, 1L, 0L),
      one_sided_betting & team_to_bet == 2 ~ ifelse(kelly_team2 > 0, 2L, 0L),
      
      # Two-sided logic (original)
      !one_sided_betting & kelly_team1 <= 0 & kelly_team2 <= 0 ~ 0L,
      !one_sided_betting & kelly_team1 >= kelly_team2 ~ 1L,
      !one_sided_betting ~ 2L,
      
      # Fallback
      TRUE ~ 0L
    ),
    
    kelly = case_when(
      bet_side == 1L ~ kelly_team1,
      bet_side == 2L ~ kelly_team2,
      TRUE ~ 0
    ),
    
    bet_odds = case_when(
      bet_side == 1L ~ odds_team1_decimal,
      bet_side == 2L ~ odds_team2_decimal,
      TRUE ~ 1
    ),
    
    bet_win = case_when(
      bet_side == 0L ~ NA,
      bet_side == 1L ~ team_1_won == 1,
      bet_side == 2L ~ team_1_won == 0
    )
  )

# Bankroll simulation (unchanged)
initial_bankroll <- 100
bankroll <- numeric(nrow(betting_set) + 1L)
bankroll[1] <- initial_bankroll

for (i in seq_len(nrow(betting_set))) {
  f   <- betting_set$kelly[i]
  o   <- betting_set$bet_odds[i]
  win <- betting_set$bet_win[i]
  bankroll[i + 1L] <- bankroll[i] * (1 + ifelse(isTRUE(win), f * (o - 1), ifelse(isFALSE(win), -f, 0)))
}

betting_set$bankroll <- bankroll[-1L]

# Print summary of betting strategy
if (one_sided_betting) {
  cat("Using one-sided betting on team", team_to_bet, "\n")
} else {
  cat("Using two-sided betting (bet on side with better edge)\n")
}

# Inspect
print(head(betting_set %>% 
             select(match_date, estimated_probability, odds_team1_decimal, odds_team2_decimal,
                    kelly_team1, kelly_team2, bet_side, kelly, bet_odds, team_1_won, bankroll)))

# Plot bankroll
print(
  ggplot(betting_set, aes(x = match_date, y = bankroll / initial_bankroll)) +
    geom_line() +
    geom_point(aes(size = kelly)) +
    ggtitle('Multiple of initial money, dota2 bets, simulated') +
    xlab('') + ylab('')+geom_hline(aes(yintercept = 1), linetype = 2)
)

# Tests
betting_set <- betting_set %>% 
  arrange(match_date) %>%
  mutate(
    prev_bankroll = lag(bankroll, default = initial_bankroll),
    ret           = (bankroll - prev_bankroll) / prev_bankroll,
    logret        = log(bankroll / prev_bankroll)
  )

t1 <- t.test(betting_set$ret,    mu = 0, alternative = "greater")
t2 <- t.test(betting_set$logret, mu = 0, alternative = "greater")
w  <- wilcox.test(betting_set$ret, mu = 0, alternative = "greater", exact = FALSE)

boot_mean <- function(dat, idx) mean(dat[idx], na.rm = TRUE)
b <- boot(betting_set$ret, boot_mean, R = 5000)
ci <- boot.ci(b, type = "perc")$percent[4:5]

print(list(
  t_test_ret    = t1,
  t_test_logret = t2,
  wilcox_asymp  = w,
  boot_mean_CI  = ci
))

# Bets only
bets_only <- betting_set %>% filter(kelly > 0)
print(t.test(bets_only$ret, mu = 0, alternative = "greater"))
