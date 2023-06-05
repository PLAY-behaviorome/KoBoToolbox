#' Create aggregate (across PLAY forms) locomotion data and export a data frame. 
#'
#' @param in_dir Directory where the input CSV can be found. Default is 
#' 'data/csv/home_visit/non_mbcdi/deid'.
#' @param out_dir Directory where the output CSV should be written. 
#' Default is `in_dir`.
#' @param vb Do or do not print verbose output. Default is TRUE.
loco_clean_make_agg_df <-
  function(in_dir = "data/csv/home_visit/non_mbcdi/deid",
           out_dir = in_dir,
           lang_admin = "english",
           vb = TRUE) {
    stopifnot(is.character(in_dir))
    stopifnot(dir.exists(in_dir))
    stopifnot(is.character(out_dir))
    stopifnot(dir.exists(out_dir))
    stopifnot(is.logical(vb))
    
    require(tidyverse)
    
    source("R/loco_clean_make_df.R")
    
    fl <- list.files(in_dir, "english")
    
    if (vb)
      message("Making aggregate data frame from individual files...")
    out_df <- purrr::map_dfr(fl, loco_clean_make_df, in_dir, vb) |>
      # Filter NAs in participant_id
      dplyr::filter(!is.na(participant_id)) |>
      dplyr::arrange(age_group, crawl_onset_mo)
    
    out_df
  }
