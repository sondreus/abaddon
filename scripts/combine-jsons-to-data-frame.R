# ─────────────────────────────────────────────────────────────────────────────
# 1. SETUP ---------------------------------------------------------------------
# ─────────────────────────────────────────────────────────────────────────────

library(jsonlite)
library(tidyverse)
library(anytime)

# where your raw match JSONs live:
match_dir <- "source-data/match-data/"

# at which minute-marks you’d like to pull radiant_{gold,xp}_adv values:
minutes_to_check <- c(1, 5, 10, 15, 20, "end")

# ─────────────────────────────────────────────────────────────────────────────
# 2. READ + FLATTEN ALL MATCH JSONs --------------------------------------------
# ─────────────────────────────────────────────────────────────────────────────

library(jsonlite)
library(tidyverse)
library(anytime)
library(pbapply)  # for pblapply()


read_all_matches <- function(dir = "source-data/match-data/", n = NULL) {
  files <- list.files(dir, "\\.json$", full.names = TRUE)
  if (is.finite(n)) files <- files[seq_len(min(c(n, length(files))))]
  
  df_list <- pblapply(files, function(path) {
    tryCatch({
    m <- fromJSON(path, flatten = FALSE)

    # 1) Build the players table: each element of m$players becomes one row
    players <- bind_rows(m$players)
    
    # Tests for broken replay data. If broken, skip game.
    if(!sum(unlist(players$gold_t)) > 0){
      return(NULL)
    }
    
    # 2) Pick out only the scalar (length-1, atomic) match-level fields
    scalar_names <- names(m)[
      map_lgl(m, ~ is.atomic(.x) && length(.x) == 1)
    ]
    match_info <- m[scalar_names] %>%
      as_tibble() %>%
      # replicate each scalar row to line up with players
      slice(rep(1, nrow(players)))
    
    # 3) Combine into a clean DF
    df <- bind_cols(match_info, players[, setdiff(colnames(players), colnames(match_info))])
    
    # 4) Now all the columns (df$win, df$radiant_win, df$account_id, etc.)
    #    really are vectors of length nrow(players), so your ave()/ifelse()
    #    and loops will work without size errors.
    
    # Engineer some features
    df <- df %>%
      mutate(
        composition = ave(
          account_id,
          paste0(match_id, "_", win),
          FUN = function(x) paste0(sort(x), collapse = "_")
        ),
        radiant = win == radiant_win,
        score = if_else(radiant, radiant_score, dire_score),
        opponent_score = if_else(radiant, dire_score, radiant_score),
        net_score = score - opponent_score,
        KDA = kills / (deaths + 1),
        duration_min = duration / 60,
        kills_per_min = kills / duration_min,
        assists_per_min = assists / duration_min
      ) %>%
      select(-dire_score, -radiant_score)
    
    # Efficiently calculate radiant gold/xp advantage
    calculate_advantage <- function(players_df, col = "gold_t") {
      radiant <- players_df %>% filter(isRadiant) %>% pull({{col}}) %>% reduce(`+`)
      dire <- players_df %>% filter(!isRadiant) %>% pull({{col}})  %>% reduce(`+`)
      radiant - dire
    }
    
    radiant_gold_adv <- calculate_advantage(players, gold_t)
    radiant_xp_adv <- calculate_advantage(players, xp_t)
    
    # Store directly in df
    df$radiant_gold_adv <- list(radiant_gold_adv)
    df$radiant_xp_adv <- list(radiant_xp_adv)
    
    for(j in c("gold", "xp")){
      for(i in minutes_to_check){
        if(i == "end"){
          df[, paste0("radiant_", j, 
                      "_adv_", i)] <- unlist(lapply(df[, paste0("radiant_", j, "_adv")], function(x) if(length(x) == 0){return(NA)} else {unlist(x)[length(x[[1]])][1]}))
        } else {
          df[, paste0("radiant_", j, 
                      "_adv_min", i)] <- unlist(lapply(df[, paste0("radiant_", j, "_adv")], function(x) if(length(x) == 0){return(NA)} else 
                      {unlist(c(NA, x))[as.numeric(i)][1]}))
          
        }
      }
    }
      
      df$radiant_xp_adv_mean <- unlist(lapply(df$radiant_xp_adv, function(x) if(length(x) == 0){return(NA)} else {unlist(mean(x))[1]}))
      df$radiant_gold_adv_mean <- unlist(lapply(df$radiant_gold_adv, function(x) if(length(x) == 0){return(NA)} else { unlist(mean(x))[1]}))
      
      
      # Total xp / gold:
      df$total_xp <- ave(df$xp_per_min, df$match_id, FUN = sum)*(df$duration/60)
      df$total_gold <- ave(df$gold_per_min, df$match_id, FUN = sum)*(df$duration/60)
      
      df$radiant_gold_adv_end_percent <- df$radiant_gold_adv_end / df$total_gold
      df$radiant_xp_adv_end_percent <- df$radiant_xp_adv_end / df$total_xp
      
      # Remove the list columns:
      df$radiant_gold_adv <- NULL
      df$radiant_xp_adv <- NULL
      
      # Add information on tournaments from liquipedia:
      # "https://liquipedia.net/dota2/Tier_1_Tournaments"
      # "https://liquipedia.net/dota2/Tier_2_Tournaments"
      
      # tournaments <- read.csv("dota_tournaments.csv")
      
      # df$liquipedia_tier_1 <-        df$leagueid %in% tournaments$id[tournaments$tier == 1]
      # df$liquipedia_tier_2 <-        df$leagueid %in% tournaments$id[tournaments$tier == 2]
      
      
      
      # Restrict to post 2013 matches:
      # df <- df[df$start_time > as.numeric(anytime("2013-01-01")), ]
      # 
      # if(restrict){
      #   # Restrict to either liquipedia_tier_1 or liquipedia_tier_2 or in-client "premium" matches 
      #   df <- df[df$tier == "premium" | df$liquipedia_tier_1 | df$liquipedia_tier_2, ]
      # }
      # 
      # df$tier <- NULL
      # df$liquipedia_tier_1 <- NULL
      # df$liquipedia_tier_2 <- NULL
      # 
      # Dropping hero_id (unknown before game), player_name + team_id + team_name (both too inconsistently used, will rely on compositions instead), leaguename, leagueid
      df$hero_id <- NULL
      df$player_name <- NULL
      df$team_name <- NULL
      df$radiant_win <- NULL
      df$team_id <- NULL
      # df$leaguename <- NULL
      # df$leagueid <- NULL
      df$league_tier <- m$league$tier
      df$leaguename <- m$league$name
      
      # Get relative gold priority in game
      df$position <- ave(df$gold_per_min, paste0(df$match_id, df$win), FUN = function(x) order(x))
      
      # Generate composition variable
      df$match_team <- paste0(df$match_id, "_", ifelse(df$radiant, "radiant", "dire"))
      df$composition <- ave(df$account_id, df$match_team, FUN = function(x){
        paste0(sort(x), collapse = "_")
      })
      
      # Create composition-based dataset
      comp <- df
      comp <- comp[ , setdiff(colnames(comp), names(comp)[sapply(comp, is.list)])]%>%
        group_by(match_team) %>%
        mutate(account_id = paste(account_id, collapse = '_')) %>%
        summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), 
                  across(!where(is.numeric), first),
                  .groups = "drop")
      comp$position <- 6 # manually assigned role "6"
      
      # Generate ID column:
      df$id <- df$account_id
      comp$id <- comp$composition
      
      # Remove list columns
      df <- df[ , setdiff(colnames(df), names(df)[sapply(df, is.list)])]
     
      # Combine the two
      df <- rbind(df, comp)
      df <- df[order(df$start_time), ]
      return(df)}, 
    error = function(e) {
      message(paste0('ERROR: ', e)) 
      return(NULL)})
    })
    
  bind_rows(df_list)
}

# Usage:
setwd('/Users/sondresolstad/Github/scrapeman')
df <- read_all_matches("match_data", n = 1000000)
setwd('/Users/sondresolstad/Github/abaddon')

write_csv(df, 'output-data/df.csv')
write_csv(df, paste0('output-data/df_', as.numeric(Sys.time()), '.csv'))

# We then get a data frame of pro player ids
get_pro_players_df <- function(api_key = NULL) {
  library(httr)
  library(jsonlite)
  
  url <- "https://api.opendota.com/api/proPlayers"
  if (!is.null(api_key)) {
    url <- modify_url(url, query = list(api_key = api_key))
  }
  
  resp <- GET(url)
  stop_for_status(resp)
  
  # parse into a data.frame
  df <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  # ensure it’s a proper data.frame (not a list)
  return(as.data.frame(df, stringsAsFactors = FALSE))
}
write_csv(get_pro_players_df(), 'source-data/pro_players.csv')
