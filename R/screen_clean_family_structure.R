screen_clean_family_structure <- function(df) {
  stopifnot(is.data.frame(df))
  
  df |>
    dplyr::rename(child_onlychild = `group_family_structure/only_child`) |>
    dplyr::rename(child_onlychild_specify = `group_family_structure/specify_onlychild`) |>
    dplyr::rename(household_members = `group_family_structure/household_members`)
}
