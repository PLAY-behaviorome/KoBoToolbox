home_visit_merge_screen_datab <- function(home_df,
                                          scr_df) {
  assertthat::assert_that(is.data.frame(home_df))
  assertthat::not_empty(home_df)
  # assertthat::is.string(scr_fn)
  # assertthat::is.readable(scr_fn)
  
  scr_df <- readr::read_csv(scr_fn,
                            col_types = readr::cols(.default = 'c'),
                            show_col_types = FALSE)
  
  scr_df <- scr_df |>
    dplyr::filter(!is.na(site_id),
                  !is.na(participant_ID),
                  !is.null(participant_ID))
  message("There are n=",
          dim(scr_df)[1],
          " records with non-NA site_id and participant_ID values")
  
  home_df <- home_df |>
    dplyr::rename("play_id" = "participant_id") |>
    dplyr::rename("participant_ID" = "subject_number") |>
    dplyr::filter(!is.na(participant_ID),
                  !is.na(site_id),
                  !is.null(participant_ID))
  message("There are n=",
          dim(home_df)[1],
          " records with non-NA site_id and participant_ID values")
  
  dplyr::left_join(scr_df, home_df, by = c("site_id", "participant_ID", "play_"))
}

clean_home_visit_df <- function(df) {
  df |>
    dplyr::rename("play_id" = "participant_id") |>
    dplyr::rename("participant_ID" = "subject_number") |>
    dplyr::filter(
      !is.na(participant_ID),
      !is.null(participant_ID),
      !is.na(site_id),
      !is.null(site_id)
    )
}

clean_screen_df <- function(df) {
  df |>
    dplyr::filter(
      !is.na(participant_ID),
      !is.null(participant_ID),
      !is.na(site_id),
      !is.null(site_id)
    )
}