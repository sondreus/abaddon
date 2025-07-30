# This script assigns and maintains data splits.

# Load all match_ids
df_raw <- read_csv('output-data/df_feat.csv')
match_ids <- df_raw %>% select(match_id) %>% unique() %>% unlist()

# Load assignments so far
splits <- read_csv('output-data/splits.csv')

# Remove those already assigned:
match_ids <- match_ids[!match_ids %in% splits$match_id]

if(pro_n_threshold > 0){
  exclude <- df_raw %>% filter(n_pros < pro_n_threshold) %>% select(match_id) %>% unique() %>% unlist()
  splits$split[splits$match_id %in% exclude] <- 'excluded by pro_n_threshold'
}

if(length(match_ids) > 0){
  # Assign 1/2 of remaining to latent skill model
  latent_skill_model <- data.frame(match_id = sample(match_ids, floor(length(match_ids)/2)), split = 'latent_skill_model')
  
  # Assign 1/4 of remaining to ML match model
  ml_match_model <- data.frame(match_id = 
                                 sample(setdiff(match_ids, latent_skill_model$match_id), floor(length(match_ids)/4)), 
                               split = 'ml_match_model')
  
  # Assign remaining to reserve (possibly momentum model or validation)
  reserve <- data.frame(match_id = match_ids[!match_ids %in%
                                               c(latent_skill_model$match_id,
                                                 ml_match_model$match_id)],
                        split = 'reserve')
  
  colnames(latent_skill_model) <- colnames(ml_match_model) <- colnames(reserve) <- colnames(splits)
  
  splits <- rbind(splits, latent_skill_model, ml_match_model, reserve)
  rownames(splits) <- 1:nrow(splits)
  
  # Exclude again:
  if(pro_n_threshold > 0){
    exclude <- df_raw %>% filter(n_pros < pro_n_threshold) %>% select(match_id) %>% unique() %>% unlist()
    splits$split[splits$match_id %in% exclude] <- 'excluded by pro_n_threshold'
  }
  
  # Save updated splits
  write_csv(splits, 'output-data/splits.csv')
}