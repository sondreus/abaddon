# scripts/save_diagnostics.R
# Script to save model diagnostics to a CSV file

# Function to save diagnostics
save_diagnostics <- function(script_name = "scripts/fit_bayesian_model_of_latent_skill.R") {

  # Check if required objects exist in the global environment
  required_objects <- c("calib_mean", "calib_samp", "M", "match_ids", "fit")
  missing_objects <- required_objects[!sapply(required_objects, exists)]

  if (length(missing_objects) > 0) {
    stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
  }

  # Calculate diagnostics from calib_mean (point estimates)
  auc_mean <- pROC::auc(pROC::roc(calib_mean$win, calib_mean$win_prob, quiet = TRUE))
  brier_mean <- mean((calib_mean$win - calib_mean$win_prob)^2)

  # Calculate diagnostics from calib_samp (sampled posterior)
  auc_samp <- pROC::auc(pROC::roc(calib_samp$win, calib_samp$win_prob, quiet = TRUE))
  brier_samp <- mean((calib_samp$win - calib_samp$win_prob)^2)

  # Get model diagnostics
  n_matches <- M
  n_unique_matches <- length(unique(match_ids))

  # Get Stan diagnostics
  stan_summary <- rstan::summary(fit)
  max_rhat <- max(stan_summary$summary[, "Rhat"], na.rm = TRUE)
  min_n_eff <- min(stan_summary$summary[, "n_eff"], na.rm = TRUE)
  n_divergent <- rstan::get_num_divergent(fit)
  n_max_treedepth <- rstan::get_num_max_treedepth(fit)

  # Get additional model info
  n_parameters <- nrow(stan_summary$summary)
  n_chains <- fit@sim$chains
  n_iter <- fit@sim$iter
  n_warmup <- fit@sim$warmup

  # Create diagnostics data frame
  diagnostics <- data.frame(
    timestamp = Sys.time(),
    training_time = end_time - start_time,
    script_name = script_name,
    n_matches = n_matches,
    n_unique_matches = n_unique_matches,
    auc_mean = as.numeric(auc_mean),
    brier_mean = brier_mean,
    auc_samp = as.numeric(auc_samp),
    brier_samp = brier_samp,
    max_rhat = max_rhat,
    min_n_eff = min_n_eff,
    n_divergent = n_divergent,
    n_max_treedepth = n_max_treedepth,
    n_parameters = n_parameters,
    n_chains = n_chains,
    n_iter = n_iter,
    n_warmup = n_warmup,
    stringsAsFactors = FALSE
  )

  # Define output file path
  output_file <- "model_diagnostics.csv"

  # Check if file exists, if so append, otherwise create new
  if (file.exists(output_file)) {
    # Read existing data
    existing_data <- read.csv(output_file, stringsAsFactors = FALSE)

    # Convert timestamp back to POSIXct if it exists
    if ("timestamp" %in% names(existing_data)) {
      existing_data$timestamp <- as.POSIXct(existing_data$timestamp)
    }

    # Combine with new data
    combined_data <- rbind(existing_data, diagnostics)

    # Write back to file
    write.csv(combined_data, output_file, row.names = FALSE)

    cat("Diagnostics appended to", output_file, "\n")
  } else {
    # Create new file
    write.csv(diagnostics, output_file, row.names = FALSE)
    cat("Diagnostics saved to", output_file, "\n")
  }

  # Print summary to console
  cat("\n--- Model Diagnostics Summary ---\n")
  cat("Timestamp:", format(diagnostics$timestamp), "\n")
  cat("Timestamp:", format(diagnostics$training_time), "\n")
  cat("Script:", diagnostics$script_name, "\n")
  cat("Matches:", diagnostics$n_matches, "(", diagnostics$n_unique_matches, "unique )\n")
  cat("AUC (mean):", round(diagnostics$auc_mean, 3), "\n")
  cat("Brier (mean):", round(diagnostics$brier_mean, 3), "\n")
  cat("AUC (sampled):", round(diagnostics$auc_samp, 3), "\n")
  cat("Brier (sampled):", round(diagnostics$brier_samp, 3), "\n")
  cat("Max R-hat:", round(diagnostics$max_rhat, 3), "\n")
  cat("Min N_eff:", round(diagnostics$min_n_eff, 0), "\n")
  cat("Divergent transitions:", diagnostics$n_divergent, "\n")
  cat("Max treedepth hits:", diagnostics$n_max_treedepth, "\n")
  cat("Parameters:", diagnostics$n_parameters, "\n")
  cat("Chains:", diagnostics$n_chains, "\n")
  cat("Iterations:", diagnostics$n_iter, "(", diagnostics$n_warmup, "warmup )\n")

  # Return diagnostics invisibly
  invisible(diagnostics)
}

# Run the function when script is sourced
save_diagnostics()
