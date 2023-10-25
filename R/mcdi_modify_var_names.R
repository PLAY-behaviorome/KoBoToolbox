mcdi_modify_var_names <- function(df) {
  assertthat::assert_that(is.data.frame(df))
  
  names(df) <- names(df) |>
    stringr::str_replace("participant_id", "play_id")
  
  df |>
    mcdi_modify_dupes("outside") |>
    mcdi_modify_dupes("mommy") |>
    mcdi_modify_dupes("please") |>
    mcdi_modify_dupes("bath")
}
