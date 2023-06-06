###################################################################
#' Cleans the old/original demographic/screening form by selecting
#' a subset of variables
#'
#' @param csv_fn A filename for the CSV file associated with the original
#' screening form
#' @return A data frome
clean_demog_1 <- function(csv_fn) {
  require(readr)
  stopifnot(file.exists(csv_fn))
  
  df_1 <- readr::read_csv(csv_fn, show_col_types = FALSE)
  
  if (!is.data.frame(df_1)) {
    message("Failure to read file '", csv_fn, "'")
    return(NULL)
  }
  df_1 %>%
    dplyr::select(
      .,
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
    ) %>%
    dplyr::mutate(
      .,
      site_id = dplyr::recode(
        site_id,
        NYU = "NYUNI",
        new_york_unive = "NYUNI",
        georgetown_uni = "GEORG",
        GTN = "GEORG",
        UCR = "UCRIV"
      )
    ) %>%
    dplyr::filter(.,!stringr::str_detect(site_id, '_of__'))
}
