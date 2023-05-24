#' Select Rothbart Early Childhood Behavior Questionnaire (EBQ) data, clean it,
#' and export a CSV.
#'
#' @param in_fn The input CSV file. Default is '740625_non_mbcdi_12_english_deidentified.csv'.
#' @param in_dir Directory where the input CSV can be found. Default is 'data/csv/home_visit/non_mbcdi/deid'.
#' @param out_dir Directory where the output CSV should be written. Default is `in_dir`.
#' @param vb Do or do not print verbose output.
ebq_clean_make_csv <-
  function(in_fn = "740625_non_mbcdi_12_english_deidentified.csv",
           in_dir = "data/csv/home_visit/non_mbcdi/deid",
           out_dir = in_dir,
           vb = TRUE) {
    stopifnot(is.character(in_fn))
    stopifnot(is.character(in_dir))
    stopifnot(is.logical(vb))
    stopifnot(dir.exists(in_dir))
    fn <- file.path(in_dir, in_fn)
    stopifnot(file.exists(fn))
    
    if (vb)
      message("Making data frame.")
    out_df <- ebq_clean_make_df(in_fn, in_dir, vb)
    
    out_fn <-
      paste0(stringr::str_extract(in_fn, "^[0-9]+"), "_ebq.csv")
    out_dir_fn <- file.path(out_dir, out_fn)
    
    readr::write_csv(out_df, file = out_dir_fn)
    if (vb)
      message("Saved: '", out_fn, "'.")
  }
