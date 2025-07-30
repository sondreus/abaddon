# This function takes a data frame and fills in NAs with min()-1 by column, and adds a column saying whether or not NA

fill_NA_encode_missing <- function(df){
  df <- data.frame(df)
  has_missing <- c()
  for(i in 1:ncol(df)){
    if(sum(is.na(df[, i]) > 0)){
      has_missing <- c(has_missing, colnames(df)[i])
    }
  }
  for(i in has_missing){
    df[, paste0(i, '_isNA')] <- is.na(df[, i])
    if(class(df[, i])[1] %in% c('numeric', "POSIXct", "POSIXt")){
      df[, i] <- ifelse(is.na(df[, i]), min(df[, i], na.rm = T) - 1, df[, i])
    } else {
      df[, i] <- ifelse(is.na(df[, i]), 'is_NA', df[, i])
    }
  }
  return(df)
}
