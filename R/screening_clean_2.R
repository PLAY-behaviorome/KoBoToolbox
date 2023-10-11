###################################################################
#' Cleans new demographic/screening form by selecting
#' a subset of variables.
#'
#' @param csv_fn A filename for the CSV file associated with the original
#' screening form
#' @returns A data frome with cleaned field names suitable for merging with 
#' other screening/demographic files.
screening_clean_2 <- function(csv_fn) {
  stopifnot(is.character(csv_fn))
  stopifnot("csv_fn not found" = file.exists(csv_fn))
  
  suppressPackageStartupMessages(require(readr))
  suppressPackageStartupMessages(require(dplyr))
  
  df_2 <- readr::read_csv(csv_fn, 
                          col_types = readr::cols(.default = 'c'),
                          show_col_types = FALSE)
  
  if (!is.data.frame(df_2)) {
    message("Failure to read file '", csv_fn, "'")
    return(NULL)
  }
  
  df_2 %>%
    dplyr::select(
      submit_date = c_today,
      site_id = `play_demo_questionnaire/group_siteinfo/site_id`,
      sub_num = `play_demo_questionnaire/group_siteinfo/subject_number`,
      play_id = `play_demo_questionnaire/play_id`,
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
