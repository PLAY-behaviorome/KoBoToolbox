#-------------------------------------------------------------------------------
screen_add_db_vol_id <- function(df) {
  PLAY_VOLS <-
    readr::read_csv(
      "../data/csv/play_site_vols.csv",
      col_types = readr::cols(.default = 'c'),
      show_col_types = FALSE
    )
  
  PLAY_VOLS <- PLAY_VOLS |>
    dplyr::rename("vol_id" = "play_vol_id")

  dplyr::left_join(df, PLAY_VOLS, by = 'site_id') |>
    dplyr::select(-c('play_site_id', 'site_name'))
}
