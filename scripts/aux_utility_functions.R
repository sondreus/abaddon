# Check columns for high NA proportion
dim_na <- function(df, threshold = 100) {
  na_counts <- colSums(is.na(df))
  vars     <- names(na_counts[na_counts > threshold])
  message(sprintf("Columns with >%d NAs: %s", threshold, paste(vars, collapse=", ")))
}

# Log-loss for binary predictions
log_loss <- function(preds, truths) {
  eps <- 1e-7
  # Clip predictions to avoid log(0)
  p <- pmax(pmin(preds, 1 - eps), eps)
  loss <- -mean(truths * log(p) + (1 - truths) * log(1 - p))
  return(loss)
}

# Kelly criterion for betting stakes
kelly <- function(odd1, odd2, p1) {
  # odd1, odd2: numeric odds for team1 and team2
  # p1: probability estimate for team1 win
  b1 <- ((odd1 - 1) * p1 - (1 - p1)) / (odd1 - 1)
  b2 <- ((odd2 - 1) * (1 - p1) - p1) / (odd2 - 1)
  # No negative bets
  bet1 <- pmax(0, b1)
  bet2 <- pmax(0, b2)
  return(c(bet1 = bet1, bet2 = bet2))
}
