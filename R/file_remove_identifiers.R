###################################################################
#' Detects presence of identifying information from column names
#' in a data frame then returns data frame without those columns.
#'
#' @param df
#' @return Data frame with identifiers removed.
file_remove_identifiers <- function(df) {
  require(stringr)
  stopifnot(is.data.frame(df))
  
  contains_name <- stringr::str_detect(names(df), 'name')
  contains_address <- stringr::str_detect(names(df), 'address')
  contains_phone <- stringr::str_detect(names(df), 'phone')
  contains_email <- stringr::str_detect(names(df), 'email')
  contains_birthdate <- stringr::str_detect(names(df), 'birthdate')
  contains_first <- stringr::str_detect(names(df), 'first[12]?')
  contains_last <- stringr::str_detect(names(df), 'last[12]?')
  contains_city <- stringr::str_detect(names(df), 'city')
  contains_year <- stringr::str_detect(names(df), 'year[12]?')
  contains_month <- stringr::str_detect(names(df), 'month[12]?')
  contains_day <- stringr::str_detect(names(df), '/day[12]?$')
  
  identifiable_data <- contains_name | contains_address |
    contains_phone |
    contains_email | contains_birthdate | contains_first |
    contains_last |
    contains_city | contains_year | contains_month | contains_day
  
  identifiable_cols <- (1:length(names(df)))[identifiable_data]
  
  df_deidentified <- df %>%
    dplyr::select(.,-all_of(identifiable_cols))
  
  df_deidentified
}
