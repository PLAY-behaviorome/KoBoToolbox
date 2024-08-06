screen_clean_child_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  require(tidyr)
  require(dplyr)
  
  df |>
    tidyr::unite(col = "child_illnesses_injuries_specify", c("child_information/specify_illnesses_injuries",
                                                             "child_information/indicate_injuries_illnesses"),
                 na.rm = TRUE) |>
    dplyr::rename_with(~ gsub("child_information/", "", .x, fixed = TRUE)) |>
    dplyr::rename(child_birth_complications_specify = specify_birth_complications) |>
    dplyr::rename(child_hearing_disabilities = hearing_disabilities) |>
    dplyr::rename(child_hearing_disabilities_specify = specify_hearing) |>
    dplyr::rename(child_vision_disabilities = vision_disabilities) |>
    dplyr::rename(child_vision_disabilities_specify = specify_vision) |>
    dplyr::rename(child_major_illnesses_injuries = major_illnesses_injuries) |>
    dplyr::rename(child_developmentaldelays = other_developmentaldelays) |>
    dplyr::rename(child_developmentaldelays_specify = specify_developmentaldelays) |>
    dplyr::rename(child_sleep_location_specify = specify_child_sleep_location) |>
    dplyr::rename(child_age_mos = check_childage) |>
    dplyr::mutate(subject_number = stringr::str_pad(subject_number, 3, "left", "0"))
}
