# Set parameters:

# Key feature variables
minutes_to_check <- c(5, 10, 15, 20, 25, 30, "end")

# A threshold for collapsing IDs (number of games in which they appear at minimum)
collapse_n <- 20

# A threshold for minimum number of known pro players in a match:
pro_n_threshold <- 4

# Time intervals (seconds)
# time_intervals <- c(
#   all           = 1e10,
#   last_7_days   = 60*60*24*7,
#   last_3_months = 60*60*24*90,
#   last_12_months= 60*60*24*365,
#   last_24_months= 60*60*24*365*2
# )

# base_variables   <- c(
#   "position","duration","win","gold_per_min","xp_per_min",
#   "kills","deaths","assists","kills_per_min","deaths_per_min","assists_per_min","KDA",
#   paste0("gold_adv_min", minutes_to_check[minutes_to_check != "end"]),
#   paste0("xp_adv_min",   minutes_to_check[minutes_to_check != "end"]),
#   "gold_adv_end","gold_adv_mean","xp_adv_end","xp_adv_mean",
#   "gold_adv_end_percent","xp_adv_end_percent",
#   "comp_glicko","comp_steph","total_gold","total_xp"
# )

# Detect and set cores for parallel operations
# max_cores <- ifelse(detectCores() > 4, 7, 3)
