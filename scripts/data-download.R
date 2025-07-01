### Prediction Pipeline Cleaned Script
#-----------------------------------------
# Author: (Your Name)
# Date:   (YYYY-MM-DD)
# Purpose: Fetch, preprocess, feature-engineer, and model pro Dota2 match data

#---- 1. Setup & Configuration ----
# Load core libraries
library(parallel)
library(data.table)
library(dplyr)
library(ggplot2)
library(anytime)
library(jsonlite)
library(PlayerRatings)
library(progress)
library(reshape2)
library(googlesheets4)

# Define paths
data_dir             <- 'source-data/match-data'
recent_matches_file  <- "source-data/recent_matches.json"

# Ensure data directory exists
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

#---- 2. Utility Functions ----
source('scripts/aux_utility_functions.R')

#---- 3. Data Fetching ----
# OpenDota endpoints
BASE_URL <- "https://api.opendota.com/api"

# Fetch list of recent pro match IDs
fetch_pro_matches <- function(limit = 200) {
  url <- paste0(BASE_URL, "/proMatches")
  matches_list <- list()
  
  while (length(matches_list) < limit) {
    res <- httr::GET(url)
    if (httr::status_code(res) != 200) {
      Sys.sleep(30)
      next
    }
    batch <- httr::content(res, as = "parsed")
    if (length(batch) == 0) break
    matches_list <- c(matches_list, batch)
    last_id <- batch[[length(batch)]]$match_id
    url <- paste0(BASE_URL, "/proMatches?less_than_match_id=", last_id)
    Sys.sleep(1)
  }

  match_ids <- unlist(lapply(matches_list, function(x) {return(x$match_id)}))
  unique(head(match_ids, limit))
}

# Fetch detailed data for a single match ID
fetch_match_details <- function(match_id) {
  url <- sprintf("%s/matches/%s", BASE_URL, match_id)
  res <- httr::GET(url)
  if (httr::status_code(res) != 200) return(NULL)
  httr::content(res, as = "parsed")
}

source('scripts/feature-engineering.R')

#---- Download Pipeline ----
run_pipeline <- function(initial_fetch = 100, daily_fetch = 200, daily_cap = 1000) {
  # 1) Load or initialize stored match IDs
  if (file.exists(recent_matches_file)) {
    matches_list <- jsonlite::read_json(recent_matches_file, simplifyVector = TRUE)
  } else {
    matches_list <- integer(0)
  }
  
  # 2) Fetch today's new match IDs
  new_ids <- fetch_pro_matches(daily_fetch)
  updated <- unique(c(matches_list, new_ids))
  jsonlite::write_json(updated, recent_matches_file, pretty = TRUE)
  
  # 3) Identify which IDs haven't been fetched yet
  existing_files <- list.files(data_dir, "\\.json$", full.names = FALSE)
  existing_ids <- as.integer(sub("\\.json$", "", existing_files))
  to_fetch       <- setdiff(updated, existing_ids)
  to_fetch       <- head(to_fetch, daily_cap)
  
  # 4) Download details for missing IDs
  for (mid in to_fetch) {
    detail <- fetch_match_details(mid)
    if (!is.null(detail)) {
      jsonlite::write_json(detail, file.path(data_dir, paste0(mid, ".json")), auto_unbox = TRUE)
    }
    Sys.sleep(1)
  }
  message("Data fetch complete.")

  # 6) Save final dataset
  saveRDS(df_features, file.path(dropbox_dir, "df_features.RDS"))
  message("Pipeline complete.")
}

#---- 6. Execute ----
if (interactive()) {
  run_pipeline()
} else {
  run_pipeline()
}

