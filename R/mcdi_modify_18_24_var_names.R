mcdi_modify_18_24_var_names <- function(df) {
  assertthat::assert_that(is.data.frame(df))
  
  names(df) <- names(df) |>
    stringr::str_replace("participant_id", "play_id")
  
  df |>
    mcdi_modify_dupes("candy") |>
    mcdi_modify_dupes("leg") |>
    mcdi_modify_dupes("rain") |>
    mcdi_modify_dupes("wet")
}
