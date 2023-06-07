
#-------------------------------------------------------------------------------
#' Create aggregate (across PLAY forms) locomotion data and export a data frame. 
#'
#' @param in_dir Directory where the input CSV can be found. Default is 
#' 'data/csv/home_visit/non_mbcdi/deid'.
#' @param out_dir Directory where the output CSV should be written. 
#' Default is `in_dir`.
#' @param vb Do or do not print verbose output. Default is TRUE.
#' @returns A data frame with all of the locomotion data.
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
    
    box::use(dplyr)
    box::use(purrr)
    
    fl <- list.files(in_dir, pattern = "_english_")
    
    if (vb)
      message("Making aggregate data frame from individual files...")
    out_df <- purrr::map(fl, clean_make_df, in_dir, vb) |>
      purrr::list_rbind()
    
    with(out_df, message("Omitting n=", sum(is.na(participant_id)),
                         " participants with NA in `participant_id`."))
    
    out_df|>
      # Filter NAs in participant_id
      dplyr::filter(!is.na(participant_id)) |>
      dplyr::arrange(age_group, crawl_onset_mo)
  }


#-------------------------------------------------------------------------------
#' Select locomotor milestone data, clean it,
#' and export a data frame.
#'
#' @param in_fn The input CSV file. Default is 
#' '740625_non_mbcdi_12_english_deidentified.csv'.
#' @param in_dir Directory where the input CSV can be found. 
#' Default is 'data/csv/home_visit/non_mbcdi/deid'.
#' @param vb Do or do not print verbose output.
#' @returns A data frame with locomotion data.
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
    no_group_locomotor_milestones <- gsub("group_locomotor_milestones/group_health/", replacement = "", no_combined)
    no_group_rothbart <- gsub("group_locomotor_milestones/group_rothbart/", replacement = "", no_group_locomotor_milestones)
    names(df) <- no_group_rothbart
    
    # Select variables
    df <- df |>
      dplyr::select(
        participant_id,
        child_sex,
        age_group,
        dplyr::contains('locomotor'),
        -dplyr::contains('mediause'),
        -dplyr::contains('division_labor'),
        -dplyr::contains('health'),
        -dplyr::contains('rothbart'),
        -dplyr::contains('pets'),
        -dplyr::contains('typical'),
        -dplyr::contains('acknowledge'),
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

#-------------------------------------------------------------------------------
#' Plot onset age histogram
#'
#'@param milestone Which milestone to plot. Default is 'crawl'.
#'
plot_onset_histogram <- function(milestone = 'crawl') {
  stopifnot(is.character(milestone))
  stopifnot(milestone %in% c('crawl', 'k_walk', 'who_walk'))
  
  box::use(ggplot2)
  box::use(dplyr)
  
  loco_df <- clean_make_agg_df()
  
  if (milestone == 'crawl') {
    this_df <- loco_df |>
      dplyr::select(child_sex, crawl_onset_mo) |>
      dplyr::mutate(crawl_onset_mos = as.numeric(crawl_onset_mo)) |>
      dplyr::filter(!is.na(child_sex), !is.na(crawl_onset_mos))
    
    ggplot2::ggplot(this_df) +
      ggplot2::aes(crawl_onset_mos, fill = child_sex) +
      ggplot2::geom_histogram()
  } else if (milestone == 'k_walk'){
    this_df <- loco_df |>
      dplyr::select(child_sex, k_walk_onset_mo) |>
      dplyr::mutate(k_walk_onset_mos = as.numeric(k_walk_onset_mo)) |>
      dplyr::filter(!is.na(child_sex), !is.na(k_walk_onset_mos))
    ggplot2::ggplot(this_df) +
      ggplot2::aes(k_walk_onset_mos, fill = child_sex) +
      ggplot2::geom_histogram()
  } else {
    this_df <- loco_df |>
      dplyr::select(child_sex, who_walk_onset_mo) |>
      dplyr::mutate(who_walk_onset_mos = as.numeric(who_walk_onset_mo)) |>
      dplyr::filter(!is.na(child_sex), !is.na(who_walk_onset_mos))
    
    ggplot2::ggplot(this_df) +
      ggplot2::aes(who_walk_onset_mos, fill = child_sex) +
      ggplot2::geom_histogram()
  }
}

