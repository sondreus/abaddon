# NOTE: DOES NOT WORK

# Install & load necessary packages
if (!requireNamespace("rstan", quietly = TRUE)) {
  install.packages("rstan", dependencies = TRUE)
}
library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# 1) Define dummy data inline
#-------------------------------------------------------------------------------
df <- data.frame(
  team    = c("Team_1", "Team_2", "Team_1", "Team_3", "Team_2",
              "Team_4", "Team_5", "Team_6", "Team_3", "Team_6"),
  opp     = c("Team_2", "Team_1", "Team_3", "Team_4", "Team_4",
              "Team_5", "Team_1", "Team_2", "Team_5", "Team_3"),
  win     = c(1, 0, 1, 0, 1, 1, 0, 1, 1, 0),
  ml_prob = c(0.65, 0.35, 0.72, 0.40, 0.80, 0.61, 0.30, 0.56, 0.73, 0.45)
)

# Create numeric team IDs
teams <- unique(c(df$team, df$opp))
team_index <- setNames(seq_along(teams), teams)
df$team_id <- team_index[df$team]
df$opp_id  <- team_index[df$opp]

#-------------------------------------------------------------------------------
# 2) Prepare data list for Stan
#-------------------------------------------------------------------------------
stan_data <- list(
  N       = nrow(df),
  T       = length(teams),
  team_id = df$team_id,
  opp_id  = df$opp_id,
  win     = df$win,
  ml_prob = df$ml_prob
)

#-------------------------------------------------------------------------------
# 3) Stan model code (as a string)
#-------------------------------------------------------------------------------
stan_code <- "
data {
  int<lower=1> N;                         // # of matches
  int<lower=1> T;                         // # of teams
  int<lower=1,upper=T> team_id[N];        // challenger index
  int<lower=1,upper=T> opp_id[N];         // opponent index
  int<lower=0,upper=1> win[N];            // win indicator
  real<lower=0,upper=1> ml_prob[N];       // ML model’s predicted P(win)
}

parameters {
  vector[T] z;                            // raw, unit-normal skills
  real<lower=0> sigma_theta;              // learned skill scale
  real<lower=0> phi;                      // Beta “precision” for ml_prob
}

transformed parameters {
  vector[T] theta = z * sigma_theta;      // actual team skills
}

model {
  // — Priors on skills and Beta precision —
  z           ~ normal(0, 1);
  sigma_theta ~ normal(0, 1);             // you could also use a half-normal here
  phi         ~ gamma(2, 0.1);            // mean ≈20; tune as you see fit

  // — Likelihood —
  for (n in 1:N) {
    real d = theta[team_id[n]] - theta[opp_id[n]];
    real p = inv_logit(d);

    // match outcome
    win[n]     ~ bernoulli_logit(d);

    // ML-model prediction
    ml_prob[n] ~ beta(p * phi,
                      (1 - p) * phi);
  }
}
"

#-------------------------------------------------------------------------------
# 4) Compile & fit with rstan
#-------------------------------------------------------------------------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit <- stan(
  model_code = stan_code,
  data       = stan_data,
  iter       = 1000,
  warmup     = 500,
  chains     = 2,
  refresh    = 0
)

#-------------------------------------------------------------------------------
# 5) Extract posterior samples and reshape for plotting
#-------------------------------------------------------------------------------
post <- extract(fit, pars = "theta")$theta
# post is an array [iterations, teams]; convert to data.frame
post_df <- as.data.frame(post)
colnames(post_df) <- teams

# Select two teams for comparison
plot_df <- post_df %>%
  select(Team_1, Team_2) %>%
  pivot_longer(cols = everything(),
               names_to  = "Team",
               values_to = "Skill")

#-------------------------------------------------------------------------------
# 6) Plot density of posterior skill for Team_1 vs Team_2
#-------------------------------------------------------------------------------
ggplot(plot_df, aes(x = Skill, fill = Team)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Posterior Skill Distributions (RStan)",
    x     = "Latent Skill",
    y     = "Density"
  )
