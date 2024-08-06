screen_clean_biodad_father_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(tidyr[unite])
  
  df |>
    tidyr::unite(col = biodad_childbirth_age, 
                 c("group_biodad/biodad_childbirth_age", 
                   "parent_information/father_information/father_home")) |>
    tidyr::unite(col = biodad_race,
                 c("group_biodad/biodad_race", "parent_information/father_information/father_race")) |>
    dplyr::select(-`group_biodad/biodad_childbirth_age_over21`)
}
