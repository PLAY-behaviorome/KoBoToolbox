home_remove_metadata_fields <- function(df) {
  stopifnot(is.data.frame(df))
  rows_before <- dim(df)[1]
  df <- df |>
    dplyr::select(
      -dplyr::contains('note'),
      -dplyr::contains('instructions'),
      -dplyr::contains('acknowledge')
    )
  df
}
