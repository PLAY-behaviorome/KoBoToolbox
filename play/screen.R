#-------------------------------------------------------------------------------
# screen.R
#
# A set of functions used to process PLAY project screening/demographic survey
# data.
# 
# These functions make use of the `box` package.
#
# To invoke the `screen` module functions, execute `box::use(./play/screen)` 
# in the root directory.

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
           csv_dir = "data/csv/screening",
           retrieve_from_kobo = FALSE) {
    stopifnot(is.character(xlsx_dir))
    stopifnot(dir.exists(xlsx_dir))
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    box::use(./kobo[list_data_filtered, retrieve_save_many_xlsx])
    box::use(./files[load_xlsx_save_many_csv])
    
    kb_screen <- list_data_filtered("[Dd]emographic")
    
    if (retrieve_from_kobo) {
      message("Retrieving files from KoBoToolbox site.")
      retrieve_save_many_xlsx(kb_screen, xlsx_dir)
    }
    
    load_xlsx_save_many_csv(xlsx_dir, csv_dir, "Demographic")
    
    screen_csv_fns <-
      list.files(csv_dir, pattern = "[0-9]+.*\\.csv", full.names = TRUE)
    clean_merge(screen_csv_fns)
  }

#-------------------------------------------------------------------------------
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
        clean_3(fns[3])) |>
    dplyr::arrange(submit_date)
}

#-------------------------------------------------------------------------------
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
    
    if (!(context_str %in% c('home', 'house', 'mom', 'child'))) {
      message("`context_str` not `home`, `house`, `mom`, or `child`.")
    }
    
    box::use(stringr[str_detect])
    box::use(tibble[tibble])
    
    english_spoken <- str_detect(lang_str, "[Ee]nglish")
    spanish_spoken <- str_detect(lang_str, "[Ss]panish")
    other_spoken <- str_detect(lang_str, "[Oo]nglish")

    df <-
      tibble::tibble(lang_spoken = lang_str,
                     english_spoken,
                     spanish_spoken,
                     other_spoken)
    names(df) <- paste0(names(df), "_", context_str)
    df
  }

#-------------------------------------------------------------------------------
#' Make data frame of home language environment data
#' 
#' @param df
make_language_df <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(tibble[tibble])
  
  if ("play_phone_questionnaire/group_siteinfo/site_id" %in% names(df)) {
    site_id <- df$`play_phone_questionnaire/group_siteinfo/site_id`
    sub_num <- df$`play_phone_questionnaire/group_siteinfo/subject_number`
    child_age_mos <- df$`play_phone_questionnaire/check_childage`
    child_sex <- df$`play_phone_questionnaire/child_sex`
    mom_lang <- NA
    home_lang <- df$`play_phone_questionnaire/language_spoken_house`
    child_lang <- df$`play_phone_questionnaire/language_spoken_child`
  } else {
    site_id <- df$`play_demo_questionnaire/group_siteinfo/site_id`
    sub_num <- df$`play_demo_questionnaire/group_siteinfo/subject_number`
    child_age_mos <- df$`play_demo_questionnaire/check_childage`
    child_sex <- df$`play_demo_questionnaire/child_sex`
    mom_lang <- df$`play_demo_questionnaire/language_spoken_mom`
    home_lang <- df$`play_demo_questionnaire/language_spoken_house`
    child_lang <- df$`play_demo_questionnaire/language_spoken_child`
  }
  
  lang_mom <- clean_lang_data(mom_lang, "mom")
  lang_home <- clean_lang_data(home_lang, "house")
  lang_child <- clean_lang_data(child_lang, "child")
  
  out_df <- tibble::tibble(site_id = site_id, 
         sub_num = sub_num,
         child_age_mos = child_age_mos,
         child_sex = child_sex)
  
  cbind(out_df, lang_mom, lang_home, lang_child)
}

#-------------------------------------------------------------------------------
#' Clean subset of screening/demo variables
#' 
#' clean_3() cleans the "new" demographic screening forms.
#'
#' @param csv_fn A filename for the CSV file associated with the original
#' screening form
#' @returns A data frome with cleaned field names suitable for merging with 
#' other screening/demographic files.
clean_3 <- function(csv_fn) {
  stopifnot(is.character(csv_fn))
  stopifnot("`csv_fn` not found" = file.exists(csv_fn))
  
  box::use(readr[read_csv])
  box::use(dplyr[select])
  
  df_3 <- readr::read_csv(csv_fn, show_col_types = FALSE)
  if (!is.data.frame(df_3)) {
    message("Failure to read file '", csv_fn, "'")
    return(NULL)
  }
  df_3 |>
    dplyr::select(
      submit_date = c_today,
      site_id = `play_demo_questionnaire/group_siteinfo/site_id`,
      sub_num = `play_demo_questionnaire/group_siteinfo/subject_number`,
      child_age_mos = `play_demo_questionnaire/check_childage`,
      child_sex = `play_demo_questionnaire/child_sex`,
      #language_mom <- `play_demo_questionnaire/language_spoken_mom`,
      language_to_child = `play_demo_questionnaire/language_spoken_child`,
      language_spoken_home = `play_demo_questionnaire/language_spoken_house`,
      child_bornonduedate = `play_demo_questionnaire/child_information/child_onterm`,
      child_weight_pounds = `play_demo_questionnaire/child_information/child_weight_pounds`,
      child_weight_ounces = `play_demo_questionnaire/child_information/child_weight_ounces`,
      child_birth_complications = `play_demo_questionnaire/child_information/child_birth_complications`,
      child_hearing_disabilities = `play_demo_questionnaire/child_information/hearing_disabilities`,
      child_vision_disabilities = `play_demo_questionnaire/child_information/vision_disabilities`,
      major_illnesses_injuries = `play_demo_questionnaire/child_information/major_illnesses_injuries`,
      #child_race = `play_demo_questionnaire/child_information/child_race`,
      #child_ethnicity = `play_demo_questionnaire/child_information/child_ethnicity`,
      child_sleep_time = `play_demo_questionnaire/child_information/child_sleep_time`,
      child_wake_time = `play_demo_questionnaire/child_information/child_wake_time`,
      child_nap_hours = `play_demo_questionnaire/child_information/child_nap_hours`,
      child_sleep_location = `play_demo_questionnaire/child_information/child_sleep_location`,
      mother_childbirth_age = `play_demo_questionnaire/group_mominfo/mom_childbirth_age`,
      mother_biological = `play_demo_questionnaire/group_mominfo/mom_biological`,
      mother_relation = `play_demo_questionnaire/group_mominfo/mom_relation`,
      mother_race = `play_demo_questionnaire/group_mominfo/mom_race`,
      mother_ethnicity = `play_demo_questionnaire/group_mominfo/mom_ethnicity`,
      mother_education = `play_demo_questionnaire/group_mominfo/mom_education`,
      mother_employment = `play_demo_questionnaire/group_mominfo/mom_employment`,
      mother_occupation = `play_demo_questionnaire/group_mominfo/mom_occupation`,
      # childcare
      childcare_types = `play_demo_questionnaire/group_lh0wi25/group_child_care_arrangements/childcare_types`,
      childcare_hrs = `play_demo_questionnaire/group_lh0wi25/group_child_care_arrangements/childcare_hours`,
      childcare_age = `play_demo_questionnaire/group_lh0wi25/group_child_care_arrangements/childcare_age`
    )
}

#-------------------------------------------------------------------------------
#' Clean subset of screening/demo variables
#' 
#' clean_3() cleans the "new" demographic screening forms.
#'
#' @param csv_fn A filename for the CSV file associated with the original
#' screening form
#' @returns A data frome with cleaned field names suitable for merging with 
#' other screening/demographic files.
clean_2 <- function(csv_fn) {
  stopifnot(is.character(csv_fn))
  stopifnot("`csv_fn` not found" = file.exists(csv_fn))
  
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
      #language_mom <- `play_demo_questionnaire/language_spoken_mom`,
      language_to_child = `play_demo_questionnaire/language_spoken_child`,
      language_spoken_home = `play_demo_questionnaire/language_spoken_house`,
      child_bornonduedate = `play_demo_questionnaire/child_information/child_onterm`,
      child_weight_pounds = `play_demo_questionnaire/child_information/child_weight_pounds`,
      child_weight_ounces = `play_demo_questionnaire/child_information/child_weight_ounces`,
      child_birth_complications = `play_demo_questionnaire/child_information/child_birth_complications`,
      child_hearing_disabilities = `play_demo_questionnaire/child_information/hearing_disabilities`,
      child_vision_disabilities = `play_demo_questionnaire/child_information/vision_disabilities`,
      major_illnesses_injuries = `play_demo_questionnaire/child_information/major_illnesses_injuries`,
      #child_race = `play_demo_questionnaire/child_information/child_race`,
      #child_ethnicity = `play_demo_questionnaire/child_information/child_ethnicity`,
      child_sleep_time = `play_demo_questionnaire/child_information/child_sleep_time`,
      child_wake_time = `play_demo_questionnaire/child_information/child_wake_time`,
      child_nap_hours = `play_demo_questionnaire/child_information/child_nap_hours`,
      child_sleep_location = `play_demo_questionnaire/child_information/child_sleep_location`,
      mother_childbirth_age = `play_demo_questionnaire/group_mominfo/mom_childbirth_age`,
      mother_biological = `play_demo_questionnaire/group_mominfo/mom_biological`,
      mother_relation = `play_demo_questionnaire/group_mominfo/mom_relation`,
      mother_race = `play_demo_questionnaire/group_mominfo/mom_race`,
      mother_ethnicity = `play_demo_questionnaire/group_mominfo/mom_ethnicity`,
      mother_education = `play_demo_questionnaire/group_mominfo/mom_education`,
      mother_employment = `play_demo_questionnaire/group_mominfo/mom_employment`,
      mother_occupation = `play_demo_questionnaire/group_mominfo/mom_occupation`,
      # childcare
      childcare_types = `play_demo_questionnaire/group_child_care_arrangements/childcare_types`,
      childcare_hrs = `play_demo_questionnaire/group_child_care_arrangements/childcare_hours`,
      childcare_age = `play_demo_questionnaire/group_child_care_arrangements/childcare_age`
    )
}


#-------------------------------------------------------------------------------
#' Clean subset of screening/demo variables.
#' 
#' clean_1() cleans the "old" demographic screening forms.
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
      # language_mom <- NA,
      language_to_child = `play_phone_questionnaire/language_spoken_child`,
      language_spoken_home = `play_phone_questionnaire/language_spoken_home`,
      child_bornonduedate = `play_phone_questionnaire/child_information/child_onterm`,
      child_weight_pounds = `play_phone_questionnaire/child_information/child_weight_pounds`,
      child_weight_ounces = `play_phone_questionnaire/child_information/child_weight_ounces`,
      child_birth_complications = `play_phone_questionnaire/child_information/child_birth_complications`,
      child_hearing_disabilities = `play_phone_questionnaire/child_information/hearing_disabilities`,
      child_vision_disabilities = `play_phone_questionnaire/child_information/vision_disabilities`,
      major_illnesses_injuries = `play_phone_questionnaire/child_information/major_illnesses_injuries`,
      # child_race = NA,
      # child_ethnicity = NA,
      child_sleep_time = `play_phone_questionnaire/child_information/child_sleep_time`,
      child_wake_time = `play_phone_questionnaire/child_information/child_wake_time`,
      child_nap_hours = `play_phone_questionnaire/child_information/child_nap_hours`,
      child_sleep_location = `play_phone_questionnaire/child_information/child_sleep_location`,
      # mother info
      mother_childbirth_age = `play_phone_questionnaire/parent_information/mother_information/mother_childbirth_age`,
      mother_biological = `play_phone_questionnaire/parent_information/mother_information/mother_biological`,
      mother_relation = `play_phone_questionnaire/parent_information/mother_information/specify_adopted`, 
      mother_race = `play_phone_questionnaire/parent_information/mother_information/mother_race`,
      mother_ethnicity = `play_phone_questionnaire/parent_information/mother_information/mother_ethnicity`,
      mother_education = `play_phone_questionnaire/parent_information/mother_information/mother_ethnicity`,
      mother_employment = `play_phone_questionnaire/parent_information/mother_information/mother_employment`,
      mother_occupation = `play_phone_questionnaire/parent_information/mother_information/mother_occupation`,
      # childcare
      childcare_types = `play_phone_questionnaire/group_child_care_arrangements/childcare_types`,
      childcare_hrs = `play_phone_questionnaire/group_child_care_arrangements/childcare_hours`,
      childcare_age = `play_phone_questionnaire/group_child_care_arrangements/childcare_age`
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
