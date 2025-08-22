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
  
  my_res <- data.frame()

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
      my_res <- rbind(my_res, betting_set)
      betting_set_bc <- betting_set
      rm(betting_set)
      
      write_csv(my_res, paste0('output-data/betting_sim_checkpoint_', 
                            format(Sys.time(), "%Y%m%d_%H%M"), '.csv'))
    }, error = function(e) {
      cat("Error at cutoff", as.character(cutoff), ":", e$message, "\n")
      # Log the error but continue with next iteration
    })
    
    # Move cutoff forward by two weeks:
    cutoff <- cutoff+days(14)
  }
  write_csv(my_res, 'output-data/full_betting_sim_results.csv')
}

limited_betting_sim <- T
if(limited_betting_sim){
  models_to_test <- c("Posteriors_fit_2025-08-19 12:57:26.235651.RDS",
                      "Posteriors_fit_2025-08-19 14:53:02.047383.RDS",
                      "Posteriors_fit_2025-08-19 16:56:12.932225.RDS",
                      "Posteriors_fit_2025-08-20 00:02:24.456407.RDS",
                      "Posteriors_fit_2025-08-20 10:11:08.306107.RDS",
                      "Posteriors_fit_2025-08-20 11:13:54.527635.RDS",
                      "Posteriors_fit_2025-08-20 23:32:47.179562.RDS",
                      "Posteriors_fit_2025-08-21 20:01:32.475927.RDS")
  
  models_to_test <- dir()[grep('Posteriors_f', dir())]
  
  lim_sim <- data.frame()
  for(posterior_filename in models_to_test){
    tryCatch({source('scripts/betting_simulation.R')
    betting_set$cutoff <- most_recent_match_in_data
    betting_set$model <- posterior_filename
    lim_sim <- rbind(lim_sim, betting_set)}, error = function(e) print(e))
  }
  lim_sim <- unique(lim_sim)
  lim_sim <- lim_sim[order(lim_sim$first_game_start), ]
}

bankroll <- data.frame(bank = c(1, rep(NA, nrow(lim_sim))),
                       match_date = c(min(lim_sim$cutoff), 
                                      lim_sim$match_date),
                       return = c(0, lim_sim$ret),
                       bet = c(0, lim_sim$kelly),
                       cutoff = c('start', as.character(lim_sim$cutoff)),
                       model = c('start', as.character(lim_sim$model)))
for(i in 2:nrow(bankroll)){
  bankroll$bank[i] <- bankroll$bank[i-1]*(1+bankroll$return[i])
}

library(anytime)
bankroll$cutoff <- anydate(bankroll$cutoff)

ggplot(bankroll, aes(x=match_date, 
                     y=bank,
                     group=1))+
  geom_hline(aes(yintercept=1), linetype = 2)+
  geom_line()+
  geom_point(aes(size=bet, col=model))+
  geom_smooth(method='lm')+
  labs(title='Multiple of starting money, by date',
       x='', y='')+theme_minimal()

ggplot(bankroll, aes(x=1:nrow(bankroll), 
                     y=bank,
                     group=1))+
  geom_hline(aes(yintercept=1), linetype = 2)+
  geom_line()+
  geom_point(aes(size=bet, col=model))+
  geom_smooth(method='lm')+
  labs(title='Multiple of starting money, by bet',
       x='', y='')+theme_minimal()

pdat <- bankroll %>% filter(bankroll$cutoff != as.Date('2025-06-20'))
ggplot(data = pdat, aes(x=1:nrow(pdat), 
                     y=bank,
                     group=1))+
  geom_hline(aes(yintercept=1), linetype = 2)+
  geom_line()+
  geom_point(aes(size=bet, col=model))+
  geom_smooth(method='lm')+
  labs(title='Multiple of starting money, by bet',
       x='', y='')+theme_minimal()+facet_wrap(.~cutoff, scales = 'free')

sum(bankroll$bet)

summary(lm(return ~ model, data=bankroll, weights = bet))


ggplot(bankroll, aes(x=match_date, 
                     y=bet*return,
                     group=1))+
  geom_hline(aes(yintercept=1), linetype = 2)+
  geom_line()+
  geom_point(aes(size=bet, col=model))+
  geom_smooth(method='lm')+
  labs(title='Multiple of starting money, by date',
       x='', y='')+theme_minimal()+theme(legend.position='none')
