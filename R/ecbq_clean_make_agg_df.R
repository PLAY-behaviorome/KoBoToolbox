#' Create aggregate (across PLAY forms) Early Childhood Behavior Questionnaire
#' (ecbq) data and export a data frame. At the moment, this only works for ECBQ
#' data that were administered in English.
#'
#' @param in_dir Directory where the input CSV can be found. Default is 'data/csv/home_visit/non_mbcdi/deid'.
#' @param out_dir Directory where the output CSV should be written. Default is `in_dir`.
#' @param vb Do or do not print verbose output.
ecbq_clean_make_agg_df <-
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
    
    fl <- list.files(in_dir, "english")
    
    if (vb)
      message("Making aggregate data frame from individual files...")
    out_df <- purrr::map_dfr(fl, ecbq_clean_make_df, in_dir, vb, .progress = TRUE) |>
      # Filter NAs in participant_id
      dplyr::filter(!is.na(participant_id)) 
    
    out_df
  }
