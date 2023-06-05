#' Select locomotor milestone data, clean it,
#' and export a data frame.
#'
#' @param in_fn The input CSV file. Default is '740625_non_mbcdi_12_english_deidentified.csv'.
#' @param in_dir Directory where the input CSV can be found. Default is 'data/csv/home_visit/non_mbcdi/deid'.
#' @param vb Do or do not print verbose output.
loco_clean_make_df <-
  function(in_fn = "740625_non_mbcdi_12_english_deidentified.csv",
           in_dir = "data/csv/home_visit/non_mbcdi/deid",
           vb = TRUE) {
    stopifnot(is.character(in_fn))
    stopifnot(is.character(in_dir))
    stopifnot(is.logical(vb))
    stopifnot(dir.exists(in_dir))
    fn <- file.path(in_dir, in_fn)
    stopifnot(file.exists(fn))
    
    require(readr)
    require(dplyr)
    
    if (vb)
      message("Processing file: '", in_fn, "'.")
    df <- readr::read_csv(fn, show_col_types = FALSE)
    if (dim(df)[1] <= 1) {
      if (vb)
        message(" No data in file. Skipping")
      return(NULL)
    }
    
    # Delete unneeded column name header info
    df_names <- names(df)
    no_homevisit <-
      gsub("group_homevisitquestionnaires/", replacement = "", df_names)
    no_combined <-
      gsub("group_combinedquestionnaires/",
           replacement = "",
           no_homevisit)
    names(df) <- no_combined
    
    # Select variables
    df <- df |>
      dplyr::select(
        participant_id,
        child_sex,
        age_group,
        dplyr::contains('locomotor'),
        -dplyr::contains('instructions'),
        -dplyr::contains('note'),
        -dplyr::contains('holiday'),
        -dplyr::contains('calc'),
        -dplyr::contains('check'),
        -dplyr::contains('date_format')
      )
    
    names(df) <- basename(names(df))
    
    df
  }
