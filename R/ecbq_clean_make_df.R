#' Select Rothbart Early Childhood Behavior Questionnaire (ECBQ) data, clean it,
#' and export a data frame.
#'
#' @param in_fn The input CSV file. Default is '740625_non_mbcdi_12_english_deidentified.csv'.
#' @param in_dir Directory where the input CSV can be found. Default is 'data/csv/home_visit/non_mbcdi/deid'.
#' @param vb Do or do not print verbose output.
ecbq_clean_make_df <-
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
    
    df_names <- names(df)
    
    # Delete unneeded column name header info
    no_homevisit <-
      gsub("group_homevisitquestionnaires/", replacement = "", df_names)
    no_combined <-
      gsub("group_combinedquestionnaires/",
           replacement = "",
           no_homevisit)
    
    basenames <- basename(no_combined)
    basenames_clean <- gsub("comments_rothbart",
                            replacement = "rothbart_comments", basenames)
    
    # Some of the group names differ
    #
    # no_group_rothbart <-
    #   gsub("group_rothbart/group_rothbartquestions/",
    #        replacement = "",
    #        no_combined)
    # rename_comments <- gsub("group_rothbart/comments_rothbart",
    #                         replacement = "rothbart_comments",
    #                         no_group_rothbart)
    # rename_comments <- gsub("comments_rothbart",
    #                         replacement = "rothbart_comments",
    #                         no_group_rothbart)
    
    names(df) <- basenames_clean
    
    # Select variables
    df <- df |>
      dplyr::select(
        participant_id,
        child_sex,
        age_group,
        dplyr::contains('rothbart'),-dplyr::contains('instructions'),-dplyr::contains('questions_header')
      )
    
    # Clean column values
    clean_ecbq_values <- function(this_col) {
      stringr::str_replace_all(this_col, "veryrarely", "very_rarely") |>
        stringr::str_replace_all("lessthanhalf", "less_than_half") |>
        stringr::str_replace_all("abouthalf", "about_half") |>
        stringr::str_replace_all("morethanhalf", "more_than_half") |>
        stringr::str_replace_all("almostalways", "almost_always") -> x
      
      gsub("na", replacement = NA, x)
    }
    
    # Create merged data frame
    cleaned_ecbq_vals <- df |>
      dplyr::select(contains('rothbart')) |>
      purrr::map_df(clean_ecbq_values)
    
    out_df <- tibble::tibble(
      participant_id = df$participant_id,
      child_sex = df$child_sex,
      age_group = df$age_group,
      cleaned_ecbq_vals
    )
    
    out_df
  }
