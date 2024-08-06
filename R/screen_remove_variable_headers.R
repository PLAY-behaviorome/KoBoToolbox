screen_remove_variable_headers <- function(df) {
  stopifnot(is.data.frame(df))
  
  require(stringr)
  
  old_names <- names(df)
  
  new_names <- old_names |>
    # 'Newer' data
    stringr::str_remove_all("play_demo_questionnaire/") |>
    # 'Older' data
    stringr::str_remove_all("play_phone_questionnaire/") |>
    stringr::str_remove_all("group_siteinfo/") |>
    stringr::str_remove_all("group_contact_info/") |>
    stringr::str_remove_all("group_address/") |>
    stringr::str_remove_all("group_ut0kj94/") |>
    stringr::str_remove_all("group_lh0wi25/")
  
  names(df) <- new_names
  df
}
