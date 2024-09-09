screen_clean_lang_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(tidyr[unite])
  box::use(dplyr[rename, select])
  
  df |>
    tidyr::unite(col = "language_spoken_home", 
                 c("language_spoken_house", "language_spoken_home"), 
                 na.rm = TRUE) |>
    tidyr::unite(col = "language_spoken_home_comments", c(22, 127), na.rm = TRUE) |>
    dplyr::rename(language_spoken_child_comments = language_spoken_child_other) |>
    dplyr::rename(language_spoken_mom_comments = language_spoken_mom_other) |>
    dplyr::select(-contains(c("/english", "/spanish", "/other")))
}
