screen_remove_identifiers <- function(df, vb = TRUE) {
  stopifnot(is.data.frame(df))
  
  box::use(stringr[str_detect])
  box::use(dplyr[select])
  
  var_names <- basename(names(df))
  
  contains_name <- stringr::str_detect(var_names, 'name')
  contains_address <- stringr::str_detect(var_names, 'addr')
  contains_phone <- stringr::str_detect(var_names, 'phone')
  contains_email <- stringr::str_detect(var_names, 'email')
  contains_birthdate <- stringr::str_detect(var_names, 'birthdate')
  contains_first <- stringr::str_detect(var_names, 'first[12]?')
  contains_last <- stringr::str_detect(var_names, 'last[12]?')
  contains_city <- stringr::str_detect(var_names, 'city')
  contains_year <- stringr::str_detect(var_names, 'year[12]?')
  contains_month <- stringr::str_detect(var_names, 'month[12]?')
  contains_day <- stringr::str_detect(var_names, '/day[12]?$')
  
  identifiable_data <- contains_name | contains_address |
    contains_phone |
    contains_email | contains_birthdate | contains_first |
    contains_last |
    contains_city | contains_year | contains_month | contains_day
  
  if (vb) {
    message("Removed n = ",
            sum(identifiable_data),
            " of ",
            length(var_names),
            " columns.")
  }
  
  identifiable_cols <- (1:length(names(df)))[identifiable_data]
  
  df |>
    dplyr::select(-dplyr::all_of(identifiable_cols))
  
}
