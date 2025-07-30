# Master script

# 0. Set parameters
source('scripts/set-parameters.R')

# 1. Collect match data
source('scripts/combine-jsons-to-data-frame.R')

# 2. Feature engineering
source('scripts/feature-engineering.R')

# 3. Train ML model
source('scripts/fit_ml_model_on_match_data.R')

# 4. Fit latent skill model
source('scripts/fit_bayesian_model_of_latent_skill.R')
