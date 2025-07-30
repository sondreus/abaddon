# Train ML model to predict match outcome based on covariates
library(agtboost)
library(tidyverse)

# Load data
raw <- read_csv('output-data/df_feat.csv')

# Load splits
source('scripts/aux_update_and_load_splits.R')

# Transform all character to factor-numeric
cat("Converting the following columns to factor-numeric: ")
raw <- data.frame(raw)
for(i in 1:ncol(raw)){
  if(class(raw[, i])[1] == 'character'){
    cat(colnames(raw)[i]); cat(', ')
    raw[, i] <- as.numeric(as.factor(raw[, i]))
  }
}

# Get ML split:
ml_subset <- raw[raw$match_id %in% as.numeric(splits %>% filter(split == 'ml_match_model') %>% select(match_id) %>% unlist()), ]

# Get X-matrix
X <- ml_subset %>%
  # Unselect columns where win/loss encoded, or team/match/accounts involved.
  select(-c(lose, win, loss, comeback, throw, stomp, match_id, series_id, series_type, match_seq_num, radiant_team_id, radiant_name, radiant_logo, region, last_login, is_pro, cluster, radiant_team_complete, dire_team_id, dire_name, dire_logo, dire_team_complete, radiant_captain, dire_captain, account_id, replay_url, party_id, team_number, team_slot, id, match_team, composition, leaguename, rank_tier, personaname, personaname_isNA, leaver_status, replay_salt, leagueid, name, throw_isNA, comeback_isNA, loss_isNA, stomp_isNA))

# Get Y-vector
Y <- ml_subset %>% select(win) %>% unlist()

print('Training GBT: start')
print(Sys.time())
gbt_fit <- gbt.train(y=Y,
                     x=as.matrix(X),
                     loss_function = 'logloss',
                     verbose = 10,
                     learning_rate = 0.01)
print('Training GBT: complete')
print(Sys.time())

gbt.save(gbt_fit, 'output-data/gbt_model.gbt')
gbt.importance(feature_names = colnames(X), gbt_fit)

# Check how this performs on reserve set:
X_val <- raw[raw$match_id %in% as.numeric(splits %>% filter(!split == 'latent_skill_model') %>% select(match_id) %>% unlist()), ]

preds <- predict(newdata = as.matrix(X_val[, colnames(X)]),
                 gbt_fit)

cor(X_val$win, preds)

# Export predictions to file
all_preds <- predict(newdata = as.matrix(raw[, colnames(X)]),
                     gbt_fit)

df_feat <- read_csv('output-data/df_feat.csv')
df_feat$ml_prob <- all_preds
write_csv(df_feat, 'output-data/df_feat_and_ml.csv')
