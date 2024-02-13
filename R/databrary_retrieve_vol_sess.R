databrary_retrieve_vol_sess <- function(vol_id = 1) {
  assertthat::is.number(vol_id)
  assertthat::assert_that(vol_id >= 1)
  
  df <- databraryr::download_session_csv(vol_id, as_df = TRUE, rq = lrq)
  if (assertthat::not_empty(df)) {
    df |>
      # Eliminate materials folders
      dplyr::filter(`session-date` != "materials") |>
      dplyr::mutate(vol_id = vol_id)
  } else {
    NULL
  }
}