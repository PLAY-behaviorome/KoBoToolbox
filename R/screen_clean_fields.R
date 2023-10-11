screen_clean_fields <- function(df) {
  stopifnot(is.data.frame(df))
  
  df |> 
    screen_clean_child_info() |>
    screen_clean_lang_info() |>
    screen_clean_mom_info() |>
    screen_clean_biodad_father_info() |>
    screen_clean_childcare_info() |>
    screen_clean_family_structure() |>
    screen_remove_selected_cols() |>
    screen_select_reorder_cols() |>
    screen_recode_site_id() |>
    dplyr::rename("participant_ID" = "subject_number")
}