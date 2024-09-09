screen_clean_childcare_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(dplyr[rename_with, select])
  
  df |>
    dplyr::rename_with(~ gsub("group_child_care_arrangements/", "", .x, fixed = TRUE)) |>
    dplyr::select(-contains(c("/nanny", "/relative", "/childcare", "/none")))
}
