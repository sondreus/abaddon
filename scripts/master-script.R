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

# 5. Betting simulation
source('scripts/betting_simulation.R')

# Full betting simulation loop, using at most 14-day-old models:
full_betting_sim <- T
if(full_betting_sim){
  library(lubridate)

  # Load odds data:
  bet_data <- dir('/Users/sondresolstad/Github/scrapeman/')
  bet_data <- sort(bet_data[grepl('betting_set', bet_data)], decreasing = T)[1]
  bet_dates <- read_csv(paste0('/Users/sondresolstad/Github/scrapeman/', bet_data)) %>%  select(match_date)
  
  # Define initial cutoff
  cutoff <- min(bet_dates[[1]])-days(1)
  
  res <- data.frame()

  while(cutoff < max(bet_dates[[1]])-days(14)){
    
    cat('\n')
    cat('************************\n')
    cat('************************\n')
    cat('\n')
    message(paste0('Calculating optimal model and bets for ', cutoff, ' and following two weeks'))
    message(paste0('Start: ', Sys.time()))
    cat('\n')
    cat('************************\n')
    cat('************************\n')
    cat('\n')
    
    tryCatch({
      source('scripts/fit_ml_model_on_match_data.R')
      source('scripts/fit_bayesian_model_of_latent_skill.R')
      source('scripts/betting_simulation.R')
      betting_set$cutoff <- cutoff
      res <- rbind(res, betting_set)
      betting_set_bc <- betting_set
      rm(betting_set)
      
      write_csv(res, paste0('output-data/betting_sim_checkpoint_', 
                            format(Sys.time(), "%Y%m%d_%H%M"), '.csv'))
    }, error = function(e) {
      cat("Error at cutoff", as.character(cutoff), ":", e$message, "\n")
      # Log the error but continue with next iteration
    })
    
  }
  write_csv(res, 'output-data/full_betting_sim_results.csv')

  # Move cutoff forward by two weeks:
  cutoff <- cutoff+days(14)
  
}