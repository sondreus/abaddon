# ─────────────────────────────────────────────────────────────────────────────
# 3. FEATURE ENGINEERING AND IMPUTE MISSING -----------------------------------
# ─────────────────────────────────────────────────────────────────────────────
library(tidyverse)
library(anytime)

df <- read_csv('output-data/df.csv', show_col_types = FALSE)

df_feat <- df %>%
  # Radiant dummy
  mutate(radiant    = isRadiant,
         # pick your “team score”
         # score      = if_else(radiant, radiant_score, dire_score),
         # opp_score  = if_else(radiant, dire_score, radiant_score),
         # net_score  = score - opp_score,
         # net_score_pct = net_score / (score + opp_score),
         
         # standard per-match stats
         KDA             = kills / (deaths + 1),
         duration_min    = duration / 60,
         kills_per_min   = kills / duration_min,
         deaths_per_min  = deaths / duration_min,
         assists_per_min = assists / duration_min,
         
         # total team xp/gold
         total_xp   = xp_per_min  * duration_min,
         total_gold = gold_per_min * duration_min
  ) 

# We next append a new ID column that can serve as an alternative to both ID and composition. 

# Load data on pro players
pros <- read_csv('source-data/pro_players.csv', show_col_types = FALSE)

# We then classify player-match observations in terms of whether it has any pro player in them:
# df_feat$is_pro <- df_feat$id %in% pros$account_id

# We also flag all ids as "is_pro" if they have a pro in them (multiple ids are present as a string separted by underscores, e.g. "123406722_179986197_357873937_451153940_1675393568")\
df_feat$is_pro <- NULL
is_pro <- data.frame(id = unique(df_feat$id),
                     is_pro = unlist(lapply(unique(df_feat$id), 
                                            FUN = function(i){
  cat(paste0('\r\r\r\r\r\r\r\r\r  Categorizing IDs:     ', i))
  if(grepl('_', i)){
      return(any(unlist(strsplit(i, "_")) %in% pros$account_id))
    } else {
      return(i %in% pros$account_id)
  }})))
df_feat <- merge(df_feat, is_pro, by = 'id', all.x = T)

# Calculate match counts based on recent matches only (last 6 months)
# Assuming you have a date/time column - adjust the column name as needed
six_months_ago <- Sys.Date() - months(6)

# Filter to recent matches for counting purposes
# Replace 'start_time' with your actual date column name
recent_df <- df_feat %>%
  filter(as.Date(anytime(start_time, calcUnique = T)) >= six_months_ago)  # or however your date column is formatted

# Calculate match counts from recent data only
player_match_counts <- table(recent_df$id)

# Define ID category
df_feat$id_category <- case_when(
  df_feat$is_pro & player_match_counts[df_feat$id] >= collapse_n ~ "pro_individual",
  df_feat$is_pro & player_match_counts[df_feat$id] < collapse_n ~ "pro_lowdata",
  !df_feat$is_pro & player_match_counts[df_feat$id] >= collapse_n ~ "nonpro_highdata",
  !df_feat$is_pro & player_match_counts[df_feat$id] < collapse_n ~ "nonpro_lowdata"
)
df_feat$id_category[df_feat$is_pro & is.na(df_feat$id_category)] <- 'pro_lowdata'
df_feat$id_category[!df_feat$is_pro & is.na(df_feat$id_category)] <- 'nonpro_lowdata'
df_feat$id_category[is.na(df_feat$is_pro) & is.na(df_feat$id_category)] <- 'nonpro_lowdata'
df_feat$id_category[is.na(df_feat$is_pro) & !is.na(df_feat$id_category)] <- 'nonpro_lowdata'

# Get number of pros per match:
df_feat <- df_feat %>%
  group_by(match_id) %>%
  mutate(n_pros = ifelse(sum(is_pro) == 0, 0, sum(is_pro) - 2)) %>%
  ungroup()

df_feat$model_id <- ifelse(df_feat$id_category == 'pro_individual', df_feat$id, df_feat$id_category)

df_feat$collapsed <- ifelse(df_feat$model_id != df_feat$id, 1, 0)

# Impute missing
source('scripts/aux_fill_NA_encode_missing.R')
df_feat <- fill_NA_encode_missing(df_feat)

write_csv(df_feat, 'output-data/df_feat.csv')
write_csv(df_feat, paste0('output-data/df_feat_', as.numeric(Sys.time()), '.csv'))

# BELOW IS NOT CURRENTLY USED

# Generate per-team and per-minute advantage features
compute_advantages <- function(df, minutes) {
  # df must contain list-columns radiant_gold_adv, radiant_xp_adv
  for (j in c("gold","xp")) {
    adv_col <- paste0("radiant_", j, "_adv")
    for (m in minutes) {
      out_col <- if (m == "end")
        paste0(j, "_adv_end")
      else
        paste0(j, "_adv_min", m)
      df[[out_col]] <- sapply(df[[adv_col]], function(x) {
        if (!length(x)) NA
        else if (m == "end") tail(x,1)
        else x[m]
      })
    }
    df[[paste0(j, "_adv_mean")]] <- sapply(df[[adv_col]], mean, na.rm=TRUE)
    df[[adv_col]] <- NULL
  }
  df
}

# Basic ELO/Glicko/Steph rating fusion
add_ratings <- function(df) {
  # Prepare match-level winner/loser pairs
  df2 <- unique(df[, .(match_id, composition, win, start_time)])
  winners <- df2[win==TRUE,  .(match_id, start_time, side1=composition)]
  losers  <- df2[win==FALSE, .(match_id,         side2=composition)]
  matches <- merge(winners, losers, by="match_id")
  ratings <- list(
    elo    = elo(matches[,.(winner=1, side1, side2)], history=TRUE),
    glicko = glicko(matches[,.(winner=1, side1, side2)], history=TRUE),
    steph  = steph(matches[,.(winner=1, side1, side2)], history=TRUE)
  )
  # Merge back to df (omitted full code for brevity)
  df
}