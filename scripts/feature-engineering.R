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