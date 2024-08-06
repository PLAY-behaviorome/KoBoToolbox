screen_remove_selected_cols <- function(df) {
  df |>
    dplyr::select(-end,
                  -c_today,
                  -update_date,
                  -day,
                  -day2,
                  -day1,
                  -contains("group_family_structure"),
                  -contains("parent_information"),
                  -concat1,
                  -Participant_ID_concat2,
                  -contains("NOTA_El_"),
                  -state)
}