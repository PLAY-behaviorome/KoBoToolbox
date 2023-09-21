#' Select health-related data, clean it, and export a data frame.
#'
#' @param in_fn The input CSV file. Default is '740625_non_mbcdi_12_english_deidentified.csv'.
#' @param in_dir Directory where the input CSV can be found. Default is 'data/csv/home_visit/non_mbcdi/deid'.
#' @param vb Do or do not print verbose output.
health_clean_make_df <-
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
    names(df) <- gsub("group_phq4_001/",
                      replacement = "",
                      names(df))
    names(df) <- gsub("group_general_health/",
                      replacement = "",
                      names(df))
    names(df) <- gsub("group_medicalprof/allergies",
                      replacement = "illness_allergies",
                      names(df))
    names(df) <- gsub("group_medicalprof/ear_infection",
                      replacement = "illness_ear_infection",
                      names(df))
    names(df) <- gsub("group_medicalprof/asthma",
                      replacement = "illness_asthma",
                      names(df))
    names(df) <- gsub("group_medicalprof/respiratory",
                      replacement = "illness_respiratory",
                      names(df))
    names(df) <- gsub("group_medicalprof/gastrointestinal",
                      replacement = "illness_gastrointestinal",
                      names(df))
    names(df) <- gsub("group_locomotor_milestones/",
                      replacement = "",
                      names(df))
    
    # Clean vision & hearing
    df <- dplyr::mutate(df, vision_tested = health_clean_vision(df)) 
    df <- dplyr::mutate(df, hearing_tested = health_clean_hearing(df))
    dplyr::select(df, -contains('child_vision_tested'),
                    -contains('child_hearing_tested'))
    
  }
