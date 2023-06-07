################################################################################
#' Create aggregate (across PLAY forms) health data and export a data frame. 
#'
#' @param in_dir Directory where the input CSV can be found. Default is 
#' 'data/csv/home_visit/non_mbcdi/deid'.
#' @param out_dir Directory where the output CSV should be written. 
#' Default is `in_dir`.
#' @param vb Do or do not print verbose output. Default is TRUE.
clean_make_agg_df <-
  function(in_dir = "data/csv/home_visit/non_mbcdi/deid",
           out_dir = in_dir,
           lang_admin = "english",
           vb = TRUE) {
    stopifnot(is.character(in_dir))
    stopifnot(dir.exists(in_dir))
    stopifnot(is.character(out_dir))
    stopifnot(dir.exists(out_dir))
    stopifnot(is.logical(vb))
    
    box::use(purrr)
    box::use(dplyr)
    
    fl <- list.files(in_dir, "english")
    
    if (vb)
      message("Making aggregate data frame from individual files...")
    out_df <- purrr::map_dfr(fl, clean_make_df, in_dir, vb) |>
      # Filter NAs in participant_id
      dplyr::filter(!is.na(participant_id))
    
    out_df
  }


################################################################################
#' Clean hearing check data from the screening survey
#' 
#' @param df Data frame of hearing screening data.
#' @returns A data frame with cleaned hearing data.
clean_hearing <- function(df) {
  
  box::use(dplyr)
  box::use(xfun)
  
  hearing_df <- df |>
    dplyr::select(contains("hearing_tested"),
                  -contains("birthhospital"),
                  -contains("afterhome"),
                  -contains("no"),
                  -contains("refused"),
                  -contains("donotknow"))
  names(hearing_df) <- gsub("group_general_health/",
                            "",
                            names(hearing_df))
  hearing_df
}

################################################################################
#' Select health-related data, clean it, and export a data frame.
#'
#' @param in_fn The input CSV file. Default is '740625_non_mbcdi_12_english_deidentified.csv'.
#' @param in_dir Directory where the input CSV can be found. Default is 'data/csv/home_visit/non_mbcdi/deid'.
#' @param vb Do or do not print verbose output.
#' @returns A data frame of cleaned health questionnaire data.
clean_make_df <-
  function(in_fn = "740625_non_mbcdi_12_english_deidentified.csv",
           in_dir = "data/csv/home_visit/non_mbcdi/deid",
           vb = TRUE) {
    stopifnot(is.character(in_fn))
    stopifnot(is.character(in_dir))
    stopifnot(is.logical(vb))
    stopifnot(dir.exists(in_dir))
    fn <- file.path(in_dir, in_fn)
    stopifnot(file.exists(fn))
    
    box::use(readr)
    box::use(dplyr)
    box::use(xfun)
    
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
        dplyr::contains('health'),
        -dplyr::contains('instructions'),
        -dplyr::contains('note'),
        -dplyr::contains('doctor_told_you')
      )
    names(df) <- gsub("group_health/",
                      replacement = "",
                      names(df))
    names(df) <- gsub("group_general_health/child_allergies_infections_ill/",
                      replacement = "illness_",
                      names(df))
    names(df) <- gsub("group_feeding_nutrition/",
                      replacement = "feeding_",
                      names(df))
    names(df) <- gsub("group_drinking/",
                      replacement = "",
                      names(df))
    names(df) <- gsub("group_smoking/",
                      replacement = "",
                      names(df))
    names(df) <- gsub("group_prenatal/",
                      replacement = "",
                      names(df))
    names(df) <- gsub("group_phq4/",
                      replacement = "",
                      names(df))
    names(df) <- gsub("group_general_health/",
                      replacement = "",
                      names(df))
    
    # Clean vision & hearing
    df <- dplyr::mutate(df, vision_tested = clean_vision(df)) 
    df <- dplyr::mutate(df, hearing_tested = clean_hearing(df))
    dplyr::select(df, -contains('child_vision_tested'),
                  -contains('child_hearing_tested'))
    
  }

################################################################################
#' Clean the vision screening data from the initial screen.
#' 
#' @param df A data frame of health data.
#' @returns A data frame with cleaned vision-related data.
clean_vision <- function(df) {
  
  box::use(dplyr)
  box::use(xfun)
  
  vision_df <- df |>
    dplyr::select(contains("vision_tested"),
                  -contains("birthhospital"),
                  -contains("afterhome"),
                  -contains("no"),
                  -contains("refused"),
                  -contains("donotknow"))
  names(vision_df) <- gsub("group_general_health/",
                           "",
                           names(vision_df))
  vision_df
}