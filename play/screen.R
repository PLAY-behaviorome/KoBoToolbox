#-------------------------------------------------------------------------------
# screen.R
#
# A set of functions used to process PLAY project screening/demographic survey
# data.
# 
# These functions make use of the `box` package.
#
# To invoke the `screen` module, execute `box::use(./play/screen)` in the root
# directory.

#-------------------------------------------------------------------------------
#' Make data frame from screening/demographic survey.
#' 
#' @param kb_screen Data frame with file information about the screening/demog
#' survey data. This comes from the larger data frame of all KBT forms, but
#' filtered to include 'Demographic' in the file name.
#' @param xlsx_dir Directory where the XLSX format screening/demog surveys are
#' stored. Default is "data/xlsx/screening".
#' @param csv_dir Directory where the CSV file should be saved. Default is
#' "data/csv/screening".
#' @returns Data frame with the merged screening/demographic survey data.
make_df <-
  function(xlsx_dir = "data/xlsx/screening",
           csv_dir = "data/csv/screening") {
    stopifnot(is.character(xlsx_dir))
    stopifnot(dir.exists(xlsx_dir))
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    box::use(./kobo[list_data_filtered, retrieve_save_many_xlsx])
    box::use(./files[load_xlsx_save_many_csv])
    
    kb_screen <- list_data_filtered("[Dd]emographic")
    
    retrieve_save_many_xlsx(kb_screen, xlsx_dir)
    
    load_xlsx_save_many_csv(xlsx_dir, csv_dir, "Demographic")
    
    screen_csv_fns <-
      list.files(csv_dir, pattern = "[0-9]+.*\\.csv", full.names = TRUE)
    clean_merge(screen_csv_fns)
  }

###################################################################
#' Cleans set of screening/demographic data files and aggregates them into
#' a single data frame.
#'
#' @param fns A character vector of CSV file names.
#' @returns A single dataframe with the aggregated demographic data files.
clean_merge <- function(fns) {
  stopifnot(is.character(fns))
  
  box::use(dplyr[arrange])
  
  rbind(clean_1(fns[1]),
        clean_2(fns[2]),
        clean_2(fns[3])) |>
    dplyr::arrange(submit_date)
}

################################################################################
#' Takes an array of strings with language input responses and returns
#' a data fram about what language(s) are in the array.
#'
#' @param lang_str A an array of strings with language names, e.g., from the
#' Screening/Demographic survey.
#' @param contex_str A string indicating whether the language context was in the
#' home 'home' or to the child 'to_child'. These are appended to the output
#' variable names.
#' @returns A data frame with information about the language(s) are in the array.
clean_lang_data <-
  function(lang_str, context_str = "home") {
    stopifnot(is.character(lang_str))
    stopifnot(is.character(context_str))
    
    if (!(context_str %in% c('home', 'to_child'))) {
      message("`context_str` not `home` or `to_child`.")
    }
    
    box::use(stringr[str_detect])
    box::use(tibble[tibble])
    
    english_spoken <- str_detect(lang_str, "[Ee]nglish")
    spanish_spoken <- str_detect(lang_str, "[Ss]panish")
    other_spoken <- str_detect(lang_str, "[Oo]nglish")
    n_langs_spoken <-
      sum(english_spoken, spanish_spoken, other_spoken)
    
    df <-
      tibble::tibble(lang_spoken = lang_str,
                     english_spoken,
                     spanish_spoken,
                     other_spoken,
                     n_langs_spoken)
    names(df) <- paste0(names(df), "_", context_str)
    df
  }

###################################################################
#' Cleans new demographic/screening form by selecting
#' a subset of variables.
#'
#' @param csv_fn A filename for the CSV file associated with the original
#' screening form
#' @returns A data frome with cleaned field names suitable for merging with 
#' other screening/demographic files.
clean_2 <- function(csv_fn) {
  stopifnot(is.character(csv_fn))
  stopifnot("csv_fn not found" = file.exists(csv_fn))
  
  box::use(readr[read_csv])
  box::use(dplyr[select])
  
  df_2 <- readr::read_csv(csv_fn, show_col_types = FALSE)
  if (!is.data.frame(df_2)) {
    message("Failure to read file '", csv_fn, "'")
    return(NULL)
  }
  df_2 |>
    dplyr::select(
      submit_date = c_today,
      site_id = `play_demo_questionnaire/group_siteinfo/site_id`,
      sub_num = `play_demo_questionnaire/group_siteinfo/subject_number`,
      child_age_mos = `play_demo_questionnaire/check_childage`,
      child_sex = `play_demo_questionnaire/child_sex`,
      language_to_child = `play_demo_questionnaire/language_spoken_child`,
      language_spoken_home = `play_demo_questionnaire/language_spoken_house`,
      child_bornonduedate = `play_demo_questionnaire/child_information/child_onterm`,
      child_weight_pounds = `play_demo_questionnaire/child_information/child_weight_pounds`,
      child_weight_ounces = `play_demo_questionnaire/child_information/child_weight_ounces`,
      child_birth_complications = `play_demo_questionnaire/child_information/child_birth_complications`,
      major_illnesses_injuries = `play_demo_questionnaire/child_information/major_illnesses_injuries`,
      child_sleep_time = `play_demo_questionnaire/child_information/child_sleep_time`,
      child_wake_time = `play_demo_questionnaire/child_information/child_wake_time`,
      child_nap_hours = `play_demo_questionnaire/child_information/child_nap_hours`,
      child_sleep_location = `play_demo_questionnaire/child_information/child_sleep_location`,
      mother_childbirth_age = `play_demo_questionnaire/group_mominfo/mom_childbirth_age`,
      mother_race = `play_demo_questionnaire/group_mominfo/mom_race`,
      mother_ethnicity = `play_demo_questionnaire/group_mominfo/mom_ethnicity`,
    )
}

###################################################################
#' Cleans the old/original demographic/screening form by selecting
#' a subset of variables
#'
#' @param csv_fn A filename for the CSV file associated with the original
#' screening form
#' @returns A data frame with cleaned data field names suitable for merging
#' with data from the other screening/demographic forms.
clean_1 <- function(csv_fn) {
  stopifnot(is.character(csv_fn))
  stopifnot(file.exists(csv_fn))
  
  box::use(readr[read_csv])
  box::use(dplyr[select, mutate, filter])
  
  df_1 <- readr::read_csv(csv_fn, show_col_types = FALSE)
  
  if (!is.data.frame(df_1)) {
    message("Failure to read file '", csv_fn, "'")
    return(NULL)
  }
  df_1 |>
    dplyr::select(
      submit_date = c_today,
      site_id = `play_phone_questionnaire/group_siteinfo/site_id`,
      sub_num = `play_phone_questionnaire/group_siteinfo/subject_number`,
      child_age_mos = `play_phone_questionnaire/check_childage`,
      child_sex = `play_phone_questionnaire/child_sex`,
      language_to_child = `play_phone_questionnaire/language_spoken_child`,
      language_spoken_home = `play_phone_questionnaire/language_spoken_home`,
      child_bornonduedate = `play_phone_questionnaire/child_information/child_onterm`,
      child_weight_pounds = `play_phone_questionnaire/child_information/child_weight_pounds`,
      child_weight_ounces = `play_phone_questionnaire/child_information/child_weight_ounces`,
      child_birth_complications = `play_phone_questionnaire/child_information/child_birth_complications`,
      major_illnesses_injuries = `play_phone_questionnaire/child_information/major_illnesses_injuries`,
      child_sleep_time = `play_phone_questionnaire/child_information/child_sleep_time`,
      child_wake_time = `play_phone_questionnaire/child_information/child_wake_time`,
      child_nap_hours = `play_phone_questionnaire/child_information/child_nap_hours`,
      child_sleep_location = `play_phone_questionnaire/child_information/child_sleep_location`,
      mother_childbirth_age = `play_phone_questionnaire/parent_information/mother_information/mother_childbirth_age`,
      mother_race = `play_phone_questionnaire/parent_information/mother_information/mother_race`,
      mother_ethnicity = `play_phone_questionnaire/parent_information/mother_information/mother_ethnicity`,
    ) |>
    dplyr::mutate(
      site_id = dplyr::recode(
        site_id,
        NYU = "NYUNI",
        new_york_unive = "NYUNI",
        georgetown_uni = "GEORG",
        GTN = "GEORG",
        UCR = "UCRIV"
      )
    ) |>
    dplyr::filter(!stringr::str_detect(site_id, '_of__'))
}
