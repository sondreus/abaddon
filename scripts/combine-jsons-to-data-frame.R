#---- Load jsons and make data frame ---- ----

# Convert raw JSON list to a unified data.table, filling missing fields with NA
json_to_dt <- function(json_list) {
  # Determine superset of all field names across all records
  all_names <- unique(unlist(lapply(json_list, names)))
  
  # Convert each record into a data.table with all columns
  dt_list <- lapply(json_list, function(x) {
    # Fill missing fields with NA
    x[setdiff(all_names, names(x))] <- NA
    as.data.table(x)[, all_names, with = FALSE]
  })
  
  # Bind all together
  rbindlist(dt_list, fill = TRUE)
}
