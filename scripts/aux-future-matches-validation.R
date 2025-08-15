# scripts/out_of_sample_validation.R
# Out-of-sample calibration test on future matches
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(anytime)
})

validate_on_future_matches <- function(posterior_file, max_weeks_ahead = 4) {
  
  # Load the saved posteriors and metadata
  if (!file.exists(posterior_file)) {
    stop("Posterior file not found: ", posterior_file)
  }
  
  saved_objects <- readRDS(posterior_file)
  post <- saved_objects$post
  stan_data <- saved_objects$stan_data
  comp_ids <- saved_objects$comp_ids
  ids <- saved_objects$ids
  patch_values <- saved_objects$patch_values
  most_recent_training_match <- saved_objects$most_recent_match
  
  cat("Most recent training match:", format(most_recent_training_match), "\n")
  
  # Define validation window
  validation_start <- most_recent_training_match
  validation_end <- most_recent_training_match + (max_weeks_ahead * 7 * 24 * 3600)
  
  cat("Validation window:", format(validation_start), "to", format(validation_end), "\n")
  
  # Load future data - use all matches, not just those in training split
  df_future <- read_csv("output-data/df_feat_and_ml.csv", show_col_types = FALSE)
  
  # Filter to validation window (matches after training data)
  df_future <- df_future[anytime(df_future$start_time) > validation_start & 
                           anytime(df_future$start_time) <= validation_end, ]
  
  # Exclude matches which only include players or compositions which too few observations to be modelled individually:
  not_collapsed <- unique(df_future$match_id[!as.logical(df_future$collapsed)])
  df_future <- df_future %>% filter(match_id %in% not_collapsed)

  if (nrow(df_future) == 0) {
    cat("No future matches found in validation window\n")
    return(NULL)
  }
  
  cat("Found", length(unique(df_future$match_id)), "future matches for validation\n")
  
  # Apply same data processing as training
  df_future <- df_future %>%
    mutate(is_comp = str_count(id, "_") >= 2)
  
  # Only keep matches with known players/compositions
  future_match_ids <- unique(df_future$match_id)
  valid_matches <- c()
  
  for (mid in future_match_ids) {
    sub <- df_future %>% filter(match_id == mid)
    
    # Check if all players and compositions are in our training set
    comp_sub <- sub %>% filter(is_comp)
    player_sub <- sub %>% filter(!is_comp)
    
    if (nrow(comp_sub) == 2 && nrow(player_sub) == 10) {
      # Check if we know all these entities
      known_comps <- all(comp_sub$model_id %in% comp_ids)
      known_players <- all(player_sub$model_id %in% ids)
      
      if (known_comps && known_players) {
        valid_matches <- c(valid_matches, mid)
      }
    }
  }
  
  if (length(valid_matches) == 0) {
    cat("No valid future matches with known players/compositions\n")
    return(NULL)
  }
  
  cat("Using", length(valid_matches), "matches with known entities\n")
  
  # Filter to valid matches
  df_future <- df_future %>% filter(match_id %in% valid_matches)
  
  # Process future matches same as training data
  M_future <- length(valid_matches)
  home_players_future <- matrix(NA_integer_, nrow = M_future, ncol = 5)
  away_players_future <- matrix(NA_integer_, nrow = M_future, ncol = 5)
  home_comp_future <- integer(M_future)
  away_comp_future <- integer(M_future)
  win_vec_future <- integer(M_future)
  patch_id_future <- integer(M_future)
  
  # Create index mappings
  player_index <- setNames(seq_along(ids), ids)
  comp_index <- setNames(seq_along(comp_ids), comp_ids)
  
  for (i in seq_along(valid_matches)) {
    mid <- valid_matches[i]
    sub <- df_future %>% filter(match_id == mid)
    comp_sub <- sub %>% filter(is_comp)
    player_sub <- sub %>% filter(!is_comp)
    
    # Process compositions
    home_comp_future[i] <- comp_index[comp_sub$model_id[1]]
    away_comp_future[i] <- comp_index[comp_sub$model_id[2]]
    win_vec_future[i] <- comp_sub$win[1]
    
    # Get patch (use most recent if patch not seen before)
    match_patch <- sub$patch[1]
    if (match_patch %in% patch_values) {
      patch_id_future[i] <- which(patch_values == match_patch)
    } else {
      # Use most recent patch as fallback
      patch_id_future[i] <- length(patch_values)
    }
    
    # Process players
    p_idx <- player_index[player_sub$model_id]
    home_p <- p_idx[player_sub$win == win_vec_future[i]]
    away_p <- p_idx[player_sub$win != win_vec_future[i]]
    
    home_players_future[i,] <- sort(home_p)
    away_players_future[i,] <- sort(away_p)
  }
  
  # Generate predictions using posterior samples
  n_draws <- nrow(post$player_skill)
  p_draws_future <- matrix(NA_real_, nrow = n_draws, ncol = M_future)
  
  cat("Generating", n_draws, "posterior predictions for", M_future, "matches...\n")
  
  # Diagnostic: track skill components
  home_skills <- rep(0, M_future)
  away_skills <- rep(0, M_future)
  skill_differences <- rep(0, M_future)
  
  for (m in seq_len(M_future)) {
    # Calculate skill differences for each posterior draw
    S_home <- rowMeans(post$player_skill[, home_players_future[m, ]]) +
      post$comp_eff[, home_comp_future[m]] +
      post$patch_comp_eff[, patch_id_future[m], home_comp_future[m]]
    
    S_away <- rowMeans(post$player_skill[, away_players_future[m, ]]) +
      post$comp_eff[, away_comp_future[m]] +
      post$patch_comp_eff[, patch_id_future[m], away_comp_future[m]]
    
    if ("alpha_win" %in% names(post) && "beta_win" %in% names(post)) {
      # New calibrated models
      k_match <- patch_id_future[m]
      p_draws_future[, m] <- plogis(post$alpha_win[, k_match] +
                                      post$beta_win[, k_match] * (S_home - S_away))
    } else {
      # Older models
      p_draws_future[, m] <- plogis(S_home - S_away)
    }
    
    # Store mean values for diagnostics
    home_skills[m] <- mean(S_home)
    away_skills[m] <- mean(S_away)
    skill_differences[m] <- mean(S_home - S_away)
  }
  
  # Diagnostic output
  cat("\n=== PREDICTION DIAGNOSTICS ===\n")
  cat("Matches with exactly 0.5 prediction:", sum(abs(colMeans(p_draws_future) - 0.5) < 1e-10), "\n")
  cat("Matches with prediction within 0.001 of 0.5:", sum(abs(colMeans(p_draws_future) - 0.5) < 0.001), "\n")
  cat("Range of skill differences:", round(range(skill_differences), 4), "\n")
  cat("Range of home skills:", round(range(home_skills), 4), "\n")
  cat("Range of away skills:", round(range(away_skills), 4), "\n")
  cat("Std dev of skill differences:", round(sd(skill_differences), 4), "\n")
  
  # Check for problematic matches
  exact_half <- which(abs(colMeans(p_draws_future) - 0.5) < 1e-10)
  if (length(exact_half) > 0) {
    cat("\nFirst few matches with exactly 0.5 prediction:\n")
    for (i in head(exact_half, 3)) {
      cat("Match", i, "- Home players:", paste(ids[home_players_future[i,]], collapse=", "), "\n")
      cat("         - Away players:", paste(ids[away_players_future[i,]], collapse=", "), "\n")
      cat("         - Home comp:", comp_ids[home_comp_future[i]], "\n")
      cat("         - Away comp:", comp_ids[away_comp_future[i]], "\n")
      cat("         - Patch:", patch_values[patch_id_future[i]], "\n")
      cat("         - Skill diff:", round(skill_differences[i], 6), "\n\n")
    }
  }
  
  # Create calibration datasets
  p_hat_future <- colMeans(p_draws_future)
  
  # Sample one draw per match for alternative calibration
  set.seed(2025)
  which_draw <- sample(seq_len(n_draws), size = M_future, replace = TRUE)
  p_sampled_future <- p_draws_future[cbind(which_draw, seq_len(M_future))]
  
  calib_future_mean <- tibble(
    match_id = valid_matches,
    win = win_vec_future,
    win_prob = p_hat_future
  )
  
  calib_future_samp <- tibble(
    match_id = valid_matches,
    win = win_vec_future,
    win_prob = p_sampled_future
  )
  
  # Calculate metrics
  auc_mean_future <- pROC::auc(pROC::roc(calib_future_mean$win, calib_future_mean$win_prob, quiet = TRUE))
  brier_mean_future <- mean((calib_future_mean$win - calib_future_mean$win_prob)^2)
  
  auc_samp_future <- pROC::auc(pROC::roc(calib_future_samp$win, calib_future_samp$win_prob, quiet = TRUE))
  brier_samp_future <- mean((calib_future_samp$win - calib_future_samp$win_prob)^2)
  
  # Print results
  cat("\n=== OUT-OF-SAMPLE VALIDATION RESULTS ===\n")
  cat("Validation period:", format(validation_start), "to", format(validation_end), "\n")
  cat("Matches validated:", M_future, "\n")
  cat("AUC (mean predictions):", round(auc_mean_future, 3), "\n")
  cat("Brier (mean predictions):", round(brier_mean_future, 3), "\n")
  cat("AUC (sampled predictions):", round(auc_samp_future, 3), "\n")
  cat("Brier (sampled predictions):", round(brier_samp_future, 3), "\n")
  
  # Create reliability plot with robust binning
  library(ggplot2)
  library(scales)
  
  # Use more robust binning strategy
  bins <- min(10, M_future %/% 20)  # Ensure at least 20 matches per bin on average
  bins <- max(bins, 3)  # But at least 3 bins
  
  # Check if we can create the desired number of bins
  unique_probs <- length(unique(calib_future_samp$win_prob))
  if (unique_probs < bins) {
    bins <- max(unique_probs, 2)
    cat("Reducing bins to", bins, "due to limited unique probabilities\n")
  }
  
  # Try cut_number, fall back to cut_interval if it fails
  reliability_data <- tryCatch({
    calib_future_samp %>%
      mutate(bin = cut_number(win_prob, n = bins)) %>%
      group_by(bin) %>%
      summarise(
        mean_pred = mean(win_prob),
        obs_rate = mean(win),
        n = n(),
        .groups = 'drop'
      )
  }, error = function(e) {
    cat("cut_number failed, using cut_interval instead\n")
    calib_future_samp %>%
      mutate(bin = cut_interval(win_prob, n = bins)) %>%
      group_by(bin) %>%
      summarise(
        mean_pred = mean(win_prob),
        obs_rate = mean(win),
        n = n(),
        .groups = 'drop'
      ) %>%
      filter(n > 0)  # Remove empty bins
  })
  
  reliability_plot <- reliability_data %>%
    ggplot(aes(mean_pred, obs_rate, size = n)) +
    geom_point(alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    coord_equal() +
    scale_x_continuous("Predicted P(home win)", labels = percent_format()) +
    scale_y_continuous("Observed win rate", labels = percent_format()) +
    scale_size_area("Matches\nper bin") +
    theme_minimal() +
    labs(title = paste0("Out-of-Sample Reliability (", M_future, " matches)")) +
    geom_smooth(se = TRUE, method = 'lm', size = 1, col = 'blue')
  
  print(reliability_plot)
  
  # Save validation results
  validation_results <- list(
    validation_window = c(validation_start, validation_end),
    n_matches = M_future,
    auc_mean = as.numeric(auc_mean_future),
    brier_mean = brier_mean_future,
    auc_samp = as.numeric(auc_samp_future),
    brier_samp = brier_samp_future,
    calib_mean = calib_future_mean,
    calib_samp = calib_future_samp,
    reliability_plot = reliability_plot
  )
  
  # Append to diagnostics file
  save_validation_diagnostics(validation_results, posterior_file)
  
  return(validation_results)
}

# Function to save validation results to diagnostics
save_validation_diagnostics <- function(validation_results, posterior_file) {
  
  validation_diag <- data.frame(
    timestamp = Sys.time(),
    validation_type = "out_of_sample",
    posterior_file = basename(posterior_file),
    validation_start = validation_results$validation_window[1],
    validation_end = validation_results$validation_window[2],
    n_matches_validated = validation_results$n_matches,
    auc_mean_oos = validation_results$auc_mean,
    brier_mean_oos = validation_results$brier_mean,
    auc_samp_oos = validation_results$auc_samp,
    brier_samp_oos = validation_results$brier_samp,
    stringsAsFactors = FALSE
  )
  
  # Define output file
  output_file <- "validation_diagnostics.csv"
  
  if (file.exists(output_file)) {
    existing_data <- read.csv(output_file, stringsAsFactors = FALSE)
    if ("timestamp" %in% names(existing_data)) {
      existing_data$timestamp <- as.POSIXct(existing_data$timestamp)
    }
    if ("validation_start" %in% names(existing_data)) {
      existing_data$validation_start <- as.POSIXct(existing_data$validation_start)
    }
    if ("validation_end" %in% names(existing_data)) {
      existing_data$validation_end <- as.POSIXct(existing_data$validation_end)
    }
    combined_data <- rbind(existing_data, validation_diag)
    write.csv(combined_data, output_file, row.names = FALSE)
  } else {
    write.csv(validation_diag, output_file, row.names = FALSE)
  }
  
  cat("Validation diagnostics saved to", output_file, "\n")
}

# Example usage:
# Assuming you have a posterior file from your training run
# posterior_file <- "Posteriors_fit_2025-07-31 21:26:38.385922.RDS"  # adjust to your actual file
# results <- validate_on_future_matches(posterior_file, max_weeks_ahead = 4)
