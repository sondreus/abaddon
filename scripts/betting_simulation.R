#!/usr/bin/env Rscript
# win_probability.R
library(readr)
library(tidyverse)

# 0) (Optional) change this to where you actually saved the posteriors
# rds_file <- "Posteriors_fit_2025-08-08 14:25:55.267445.RDS"
rds_file <- posterior_filename

# 1) Read in your saved posterior & data
res          <- readRDS(rds_file)
post         <- res$post
ids          <- res$ids
comp_ids     <- res$comp_ids
patch_values <- res$patch_values
most_recent_match_in_data <- res$most_recent_match

# 2) The main function
win_prob <- function(comp1, comp2, patch, best_of = 1) {
  # 1) sanity checks
  if (!comp1 %in% comp_ids) stop("'", comp1, "' not found in comp_ids.")
  if (!comp2 %in% comp_ids) stop("'", comp2, "' not found in comp_ids.")
  if (!(best_of %in% c(1,3,5,7))) stop("best_of must be one of 1, 3, 5 or 7.")
  if(patch > max(patch_values)){
    patch <- patch - 1
    message('Most recent patch not in training data, predicting as-if previous patch')
  }
  pidx <- match(patch, patch_values)
  if (is.na(pidx)) stop("Patch '", patch, "' not in patch_values.")
  
  # 3) find the two comps’ static effects
  c1 <- which(comp_ids == comp1)
  c2 <- which(comp_ids == comp2)
  comp1_eff  <- post$comp_eff[, c1]
  comp2_eff  <- post$comp_eff[, c2]
  patch1_eff <- post$patch_comp_eff[, pidx, c1]
  patch2_eff <- post$patch_comp_eff[, pidx, c2]
  
  # 4) decode each comp’s 5 players and look up their skill‐draws
  ply1 <- strsplit(comp1, "_", fixed=TRUE)[[1]]
  ply2 <- strsplit(comp2, "_", fixed=TRUE)[[1]]
  idx1 <- match(ply1, ids)
  idx2 <- match(ply2, ids)
  
  default_idx <- match("pro_lowdata", ids)
  idx1[is.na(idx1)] <- default_idx
  idx2[is.na(idx2)] <- default_idx
  
  if (any(is.na(idx1))) stop("Couldn’t match all players in ", comp1, " to ids.")
  if (any(is.na(idx2))) stop("Couldn’t match all players in ", comp2, " to ids.")
  # post$player_skill is [draws × P]
  home_player_skill <- rowMeans(post$player_skill[, idx1, drop=FALSE])
  away_player_skill <- rowMeans(post$player_skill[, idx2, drop=FALSE])
  
  # 5) per‐draw win‐prob for a single game
  d_eff   <- (home_player_skill + comp1_eff + patch1_eff) -
    (away_player_skill + comp2_eff + patch2_eff)
  p_draws <- plogis(d_eff)
  
  if (best_of == 1) {
    return(mean(p_draws))
  }
  
  # 6) otherwise compute series‐win by binomial tail
  n            <- best_of
  wins_needed  <- floor(n/2) + 1
  series_prob  <- function(p) sum(dbinom(wins_needed:n, size=n, prob=p))
  p_series     <- vapply(p_draws, series_prob, numeric(1))
  mean(p_series)
}

# 3) Example usage:
# cat("P(compA beats compB in BO3 on patch 58): ",
# win_prob("99796146_173842160_180775030_252520776_421515107",
#          "97658618_100594231_241884166_324277900_1150772339", 
#          "58", best_of = 3), "\n")

# 4) Get probabilities for a bunch of matches:
bet_data <- dir('/Users/sondresolstad/Github/scrapeman/')
bet_data <- sort(bet_data[grepl('betting_set', bet_data)], decreasing = T)[1]
betting_set <- read_csv(paste0('/Users/sondresolstad/Github/scrapeman/', bet_data))
betting_set <- betting_set %>% filter(match_id != '') %>% # exclude matches with no data 
  filter(anytime(first_game_start) > most_recent_match_in_data) %>% # Remove those played after fit
  filter(anytime(first_game_start) < most_recent_match_in_data + days(14)) %>% # Remove those a long time after the model fit 
  mutate(betting_implied_prob = 
           (1 / odds_team1_decimal) /
           ((1 / odds_team1_decimal) + (1 / odds_team2_decimal)),
         team_1_won = ifelse(winner == "1", 1, 0))

# Add estimated probability:
betting_set$estimated_probability <- NA

for(i in 1:nrow(betting_set)){
  best_of_type <-NA
  if(betting_set$result[i] %in% c('0 - 2', '2 - 0', '2 - 1', '1 - 2')){
    best_of_type <- 3
  } 
  if(betting_set$result[i] %in% c('3 - 0', '0 - 3', '1 - 3', '3 - 1', '3 - 2', '2 - 3')){
    best_of_type <- 5
  }
  
  betting_set$estimated_probability[i] <- tryCatch(
    win_prob(betting_set$composition_team1[i],
             betting_set$composition_team2[i],
             betting_set$patch[i],
             best_of = best_of_type), error = function(e) NA)
}

betting_set <- betting_set %>% filter(!is.na(betting_implied_prob) &
                                        !is.na(estimated_probability))

print(ggplot(betting_set, aes(y=estimated_probability, x=betting_implied_prob))+geom_point())

print(cor(betting_set[, c('estimated_probability', 'betting_implied_prob', 'team_1_won')]))

library(pROC)
# AUC
auc_model   <- auc(betting_set$team_1_won, betting_set$estimated_probability)
auc_implied <- auc(betting_set$team_1_won, betting_set$betting_implied_prob)
# Brier score
brier_model   <- mean((betting_set$team_1_won - betting_set$estimated_probability)^2)
brier_implied <- mean((betting_set$team_1_won - betting_set$betting_implied_prob)^2)

print(data.frame(
  Method            = c("Your model", "Implied odds"),
  AUC               = c(as.numeric(auc_model),   as.numeric(auc_implied)),
  Brier_Score       = c(brier_model,             brier_implied)
))

# Estimate bankroll

betting_set <- betting_set %>%
  arrange(match_date) %>%
  mutate(
    kelly = pmax(
      (estimated_probability * (odds_team1_decimal - 1) - (1 - estimated_probability)) /
        (odds_team1_decimal - 1),
      0
    )
  )


# 2) Simulate bankroll over time
initial_bankroll <- 100
bankroll <- numeric(nrow(betting_set) + 1)
bankroll[1] <- initial_bankroll

for (i in seq_len(nrow(betting_set))) {
  f <- betting_set$kelly[i]
  o <- betting_set$odds_team1_decimal[i]
  win <- betting_set$team_1_won[i]
  # if win: gain f * (o−1); if lose: lose f
  bankroll[i+1] <- bankroll[i] * (1 + f * (if (win == 1) (o - 1) else -1))
}

# drop the initial slot and attach to betting_set
betting_set$bankroll <- bankroll[-1]

# 3) Inspect
head(betting_set %>% select(match_date, estimated_probability, odds_team1_decimal,
                            kelly, team_1_won, bankroll))

# Plot bankroll over time:
print(ggplot(betting_set, aes(x=match_date, y=bankroll/100))+geom_line()+geom_point(aes(size=kelly))+ggtitle('Multiple of initial money, dota2 bets, simulated')+
  xlab('')+ylab(''))

# Check if statistically significant wins
betting_set <- betting_set %>% arrange(match_date) %>%
  mutate(
    prev_bankroll = lag(bankroll, default = 100),
    ret          = (bankroll - prev_bankroll) / prev_bankroll,
    logret        = log(bankroll / prev_bankroll)
  )

# 2) Parametric tests
t1 <- t.test(betting_set$ret,    mu = 0, alternative = "greater")
t2 <- t.test(betting_set$logret, mu = 0, alternative = "greater")

# 3) Non-parametric Wilcoxon (force asymptotic)
w  <- wilcox.test(betting_set$ret, mu = 0, alternative = "greater", exact = FALSE)

# 4) Bootstrap 95% CI for mean(ret)
library(boot)
boot_mean <- function(dat, idx) mean(dat[idx], na.rm=TRUE)
b <- boot(betting_set$ret, boot_mean, R = 5000)
ci <- boot.ci(b, type = "perc")$percent[4:5]

print(list(
  t_test_ret      = t1,
  t_test_logret   = t2,
  wilcox_asymp    = w,
  boot_mean_CI    = ci
))

# Looking only at actual bets
bets_only <- betting_set %>%
  filter(kelly > 0)

# simple t‐test on conditional returns
print(t.test(bets_only$ret, mu = 0, alternative = "greater"))

