#-------------------------------------------------------------------------------
# ecbq.R
#
# Functions that are part of the play/ecbq module
#
# To load:    `box::use(play/ecbq)`.
#
# To unload:  `box::unload(ecbq)`

################################################################################
#' Create aggregate (across PLAY forms) Early Childhood Behavior Questionnaire
#' (ECBQ) data and export a data frame. At the moment, this only works for ECBQ
#' data that were administered in English.
#'
#' @param in_dir Directory where the input CSV can be found. Default is
#' 'data/csv/home_visit/non_mbcdi/deid'.
#' @param out_dir Directory where the output CSV should be written.
#' Default is `in_dir`.
#' @param vb Do or do not print verbose output.
#' @returns A data frame with the ECBQ across participants and separate
#' KoBoToolbox forms.
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
#' Select Rothbart Early Childhood Behavior Questionnaire (ECBQ) data, clean it,
#' and export a data frame.
#'
#' @param in_fn The input CSV file. Default is
#' '740625_non_mbcdi_12_english_deidentified.csv'.
#' @param in_dir Directory where the input CSV can be found.
#' Default is 'data/csv/home_visit/non_mbcdi/deid'.
#' @param vb Do or do not print verbose output.
#' @returns A data frame from a single KoBoToolbox form.
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
    box::use(xfun)
    box::use(dplyr)
    
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
    names(df) <- basenames_clean
    
    # Select variables
    df <- df |>
      dplyr::select(
        participant_id,
        child_sex,
        age_group,
        dplyr::contains('rothbart'),
        -dplyr::contains('instructions'),
        -dplyr::contains('questions_header')
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

################################################################################
#' Select Rothbart Early Childhood Behavior Questionnaire (ECBQ) data, clean it,
#' and export a CSV.
#'
#' @param in_fn The input CSV file. Default is
#' '740625_non_mbcdi_12_english_deidentified.csv'.
#' @param in_dir Directory where the input CSV can be found.
#' Default is 'data/csv/home_visit/non_mbcdi/deid'.
#' @param out_dir Directory where the output CSV should be written.
#' Default is `in_dir`.
#' @param vb Do or do not print verbose output.
#' @returns NULL
clean_make_csv <-
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
    
    box::use(readr)
    
    if (vb)
      message("Making data frame.")
    out_df <- clean_make_df(in_fn, in_dir, vb)
    
    out_fn <-
      paste0(stringr::str_extract(in_fn, "^[0-9]+"), "_ecbq.csv")
    out_dir_fn <- file.path(out_dir, out_fn)
    
    readr::write_csv(out_df, file = out_dir_fn)
    if (vb)
      message("Saved: '", out_fn, "'.")
  }


################################################################################
#' Make an ECBQ data frame 'longer' for comparative plotting across questions
#'
#' @param ecbq_df A 'wide' ECBQ data frame, e.g., from ecbq_clean_make_agg_df()
#' @returns A data frame with the ECBQ data in a "longer" form.
make_longer <- function(ecbq_df) {
  stopifnot(!is.null(ecbq_df))
  stopifnot(is.data.frame(ecbq_df))
  
  box::use(tidyr)
  box::use(dplyr)
  
  ecbq_df |>
    tidyr::pivot_longer(
      cols = contains('rothbart'),
      names_to = 'question',
      values_to = 'rating'
    ) |>
    dplyr::filter(!is.na('rating'),
                  !stringr::str_detect(question, 'comments')) |>
    dplyr::arrange(question)
}

################################################################################
#' Make an ECBQ data frame 'longer' for comparative plotting across questions
#'
#' @param df A 'long' ECBQ data frame, e.g., from ecbq_clean_make_agg_df()
#' @returns A data frame with the question variable transformed into an ordered
#' factor.
make_resps_ordinal <-
  function(df) {
    stopifnot(!is.null(df))
    stopifnot(is.data.frame(df))
    stopifnot("question" %in% names(df))
    
    box::use(dplyr)
    
    df |>
      dplyr::mutate(rating = factor(
        rating,
        c(
          "never",
          "very_rarely",
          "less_than_half",
          "about_half",
          "more_than_half",
          "almost_always",
          "always"
        ),
        ordered = TRUE
      ))
  }

################################################################################
#' Generate a barplot of all 36 questions from the
#' Rothbart Early Childhood Questionnaire (ECBQ), Very Short
#' https://research.bowdoin.edu/rothbart-temperament-questionnaires/instrument-descriptions/the-early-childhood-behavior-questionnaire/
#'
#' @param df A data frame with aggregate (across participants) ECBQ data
plot_all <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(ggplot2)
  
  if (!("question" %in% names(df))) {
    # Make longer so we can plot multiple questions
    df <- make_longer(df)
  }
  
  df |>
    # Clean question names and responses
    shorten_q_names() |>
    make_resps_ordinal() |>
    # Plot
    ggplot2::ggplot() +
    ggplot2::aes(rating, fill = age_group) +
    ggplot2::geom_bar() +
    ggplot2::facet_wrap(~ question, nrow = 12) +
    # https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
}

################################################################################
#' Generate a barplot of a single question from the
#' Rothbart Early Childhood Questionnaire (ECBQ)
#'
#' @param df A data frame with aggregate (across participants) ECBQ data
#' @returns A plot of a single variable from the ECBQ data.
plot <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(dplyr)
  box::use(ggplot2)
  
  df <- df |>
    dplyr::filter(!is.na(rothbart_unfamiliarperson)) |>
    dplyr::mutate(rothbart_unfamiliarperson = factor(
      rothbart_unfamiliarperson,
      c(
        "never",
        "very_rarely",
        "less_than_half",
        "about_half",
        "more_than_half",
        "almost_always",
        "always"
      ),
      ordered = TRUE
    ))
  
  ggplot(df) +
    aes(rothbart_unfamiliarperson) +
    geom_bar() +
    facet_grid(rows = ~ age_group) +
    # https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
    scale_x_discrete(guide = guide_axis(angle = 90))
}

################################################################################
#' Make an ECBQ data frame 'longer' for comparative plotting across questions
#'
#' @param df A 'long' ECBQ data frame, e.g., from ecbq_clean_make_agg_df()
#' @param q_pattern A string to replace in the values for the 'question' field.
#' Default is 'rothbart_'.
#' @param q_replace String to replace for values in `q_pattern`. Default is "",
#' but "ecbq_" is a logical alternative.
shorten_q_names <-
  function(df,
           q_pattern = "rothbart_",
           q_replace = "") {
    stopifnot(!is.null(df))
    stopifnot(is.data.frame(df))
    stopifnot("question" %in% names(df))
    
    box::use(dplyr)
    
    df |>
      dplyr::mutate(question = stringr::str_replace_all(question, q_pattern, q_replace))
  }
