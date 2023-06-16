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
    
    box::use(. / kobo[list_data_filtered, retrieve_save_many_xlsx])
    box::use(. / files[load_xlsx_save_many_csv])
    
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
  box::use(stringr[str_replace])
  
  rbind(clean_1(fns[1]),
        clean_2(fns[2]),
        clean_3(fns[3])) |>
    dplyr::arrange(submit_date) |>
    dplyr::mutate(household_members = stringr::str_replace_all(household_members,
                                                               "__", "_"))
}



#-------------------------------------------------------------------------------
export_csv <-
  function(df,
           csv_dir = "data/csv/screening/agg",
           csv_fn = "PLAY-demo-screen-clean.csv") {
    stopifnot(is.data.frame(df))
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    stopifnot(is.character(csv_fn))
    
    box::use(readr[write_csv])
    readr::write_csv(df, file.path(csv_dir, csv_fn))
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

add_fips <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(. / geo)
  
  
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
    sub_num <-
      df$`play_phone_questionnaire/group_siteinfo/subject_number`
    child_age_mos <- df$`play_phone_questionnaire/check_childage`
    child_sex <- df$`play_phone_questionnaire/child_sex`
    mom_lang <- NA
    home_lang <- df$`play_phone_questionnaire/language_spoken_house`
    child_lang <-
      df$`play_phone_questionnaire/language_spoken_child`
  } else {
    site_id <- df$`play_demo_questionnaire/group_siteinfo/site_id`
    sub_num <-
      df$`play_demo_questionnaire/group_siteinfo/subject_number`
    child_age_mos <- df$`play_demo_questionnaire/check_childage`
    child_sex <- df$`play_demo_questionnaire/child_sex`
    mom_lang <- df$`play_demo_questionnaire/language_spoken_mom`
    home_lang <- df$`play_demo_questionnaire/language_spoken_house`
    child_lang <- df$`play_demo_questionnaire/language_spoken_child`
  }
  
  lang_mom <- clean_lang_data(mom_lang, "mom")
  lang_home <- clean_lang_data(home_lang, "house")
  lang_child <- clean_lang_data(child_lang, "child")
  
  out_df <- tibble::tibble(
    site_id = site_id,
    sub_num = sub_num,
    child_age_mos = child_age_mos,
    child_sex = child_sex
  )
  
  cbind(out_df, lang_mom, lang_home, lang_child)
}

#-------------------------------------------------------------------------------
remove_variable_identifiers <- function(df, vb = TRUE) {
  stopifnot(is.data.frame(df))
  
  box::use(stringr[str_detect])
  box::use(dplyr[select])
  
  var_names <- basename(names(df))
  
  contains_name <- stringr::str_detect(var_names, 'name')
  contains_address <- stringr::str_detect(var_names, 'addr')
  contains_phone <- stringr::str_detect(var_names, 'phone')
  contains_email <- stringr::str_detect(var_names, 'email')
  contains_birthdate <- stringr::str_detect(var_names, 'birthdate')
  contains_first <- stringr::str_detect(var_names, 'first[12]?')
  contains_last <- stringr::str_detect(var_names, 'last[12]?')
  contains_city <- stringr::str_detect(var_names, 'city')
  contains_year <- stringr::str_detect(var_names, 'year[12]?')
  contains_month <- stringr::str_detect(var_names, 'month[12]?')
  contains_day <- stringr::str_detect(var_names, '/day[12]?$')
  
  identifiable_data <- contains_name | contains_address |
    contains_phone |
    contains_email | contains_birthdate | contains_first |
    contains_last |
    contains_city | contains_year | contains_month | contains_day
  
  if (vb) {
    message("Removed n = ",
            sum(identifiable_data),
            " of ",
            length(var_names),
            " columns.")
  }
  
  identifiable_cols <- (1:length(names(df)))[identifiable_data]
  
  df |>
    dplyr::select(-dplyr::all_of(identifiable_cols))
  
}

#-------------------------------------------------------------------------------
#' Removes variable header/group info from screening data
#'
#'@param df A data frame of demographic/screening data.
#'@returns A data frame with renamed column names.
remove_variable_headers <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(stringr[str_remove_all])
  
  old_names <- names(df)
  
  new_names <- old_names |>
    # 'Newer' data
    stringr::str_remove_all("play_demo_questionnaire/") |>
    # 'Older' data
    stringr::str_remove_all("play_phone_questionnaire/") |>
    stringr::str_remove_all("group_siteinfo/") |>
    stringr::str_remove_all("group_contact_info/") |>
    stringr::str_remove_all("group_address/") |>
    stringr::str_remove_all("group_ut0kj94/") |>
    stringr::str_remove_all("group_lh0wi25/")
  
  names(df) <- new_names
  df
}

#-------------------------------------------------------------------------------
#' Remove unneeded Databrary-related fields from screening data frame.
#'
#' @param df A data frame of screening data.
#' @returns A data frame with some fields removed.
remove_databrary_fields <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(dplyr[select, contains])
  
  dplyr::select(df,-contains('group_databrary'))
}

#-------------------------------------------------------------------------------
#' Remove unneeded fields from screening data frame.
#'
#' @param df A data frame of screening data.
#' @returns A data frame with some fields removed.
remove_metadata_fields <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(dplyr[select, contains])
  
  df |>
    dplyr::select(
      -dplyr::contains('note'),-dplyr::contains('instructions'),-dplyr::contains('acknowledge'),-dplyr::contains('screener'),-dplyr::contains('__'),-dplyr::contains('meta/instanceID'),-dplyr::contains('_uuid'),-dplyr::contains('_submission_time'),-dplyr::contains('_index'),-dplyr::contains('_parent_index'),-dplyr::contains('_tags'),-dplyr::contains('_version_'),
      # used to compute guid-dplyr::contains('day'),-dplyr::contains('concat1'),-dplyr::contains('Participant_ID_concat2'),
      # other meta-`_id`,-start,-end,-update_date,
      # Spanish-language version-dplyr::contains('NOTA')
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
clean_3 <- function(csv_fn, add_geo = TRUE) {
  stopifnot(is.character(csv_fn))
  stopifnot("`csv_fn` not found" = file.exists(csv_fn))
  
  box::use(readr[read_csv])
  box::use(dplyr[select, mutate, left_join, join_by])
  box::use(. / geo)
  
  df_3 <- readr::read_csv(csv_fn, show_col_types = FALSE)
  if (!is.data.frame(df_3)) {
    message("Failure to read file '", csv_fn, "'")
    return(NULL)
  }
  # df_3 <- df_3 |>
  #   dplyr::mutate(site_id = `play_demo_questionnaire/group_siteinfo/site_id`,
  #                 sub_num = `play_demo_questionnaire/group_siteinfo/subject_number`)
  
  # if (add_geo) {
  #   ad_3 <- geo$make_addresses(df_3, 'new')
  #   df_3 <- dplyr::left_join(df_3, ad_3, dplyr::join_by(`play_demo_questionnaire/group_siteinfo/site_id` == site_id,
  #                                                       `play_demo_questionnaire/group_siteinfo/subject_number` == sub_num),
  #                            relationship = "many-to-many")
  # }
  
  df_3 |>
    dplyr::select(
      submit_date = c_today,
      site_id = `play_demo_questionnaire/group_siteinfo/site_id`,
      sub_num = `play_demo_questionnaire/group_siteinfo/subject_number`,
      # state_fips,
      # county_fips,
      child_age_mos = `play_demo_questionnaire/check_childage`,
      child_sex = `play_demo_questionnaire/child_sex`,
      #language_mom <- `play_demo_questionnaire/language_spoken_mom`,
      language_to_child = `play_demo_questionnaire/language_spoken_child`,
      language_spoken_home = `play_demo_questionnaire/language_spoken_house`,
      child_bornonduedate = `play_demo_questionnaire/child_information/child_onterm`,
      child_weight_pounds = `play_demo_questionnaire/child_information/child_weight_pounds`,
      child_weight_ounces = `play_demo_questionnaire/child_information/child_weight_ounces`,
      child_birth_complications = `play_demo_questionnaire/child_information/child_birth_complications`,
      child_birth_complications_specify = `play_demo_questionnaire/child_information/specify_birth_complications`,
      child_hearing_disabilities = `play_demo_questionnaire/child_information/hearing_disabilities`,
      child_hearing_disabilities_specify = `play_demo_questionnaire/child_information/specify_hearing`,
      child_vision_disabilities = `play_demo_questionnaire/child_information/vision_disabilities`,
      child_vision_disabilities_specify = `play_demo_questionnaire/child_information/specify_vision`,
      major_illnesses_injuries = `play_demo_questionnaire/child_information/major_illnesses_injuries`,
      major_illnesses_injuries_specify = `play_demo_questionnaire/child_information/specify_illnesses_injuries`,
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
      childcare_age = `play_demo_questionnaire/group_lh0wi25/group_child_care_arrangements/childcare_age`,
      childcare_language = `play_demo_questionnaire/group_lh0wi25/group_child_care_arrangements/childcare_language`,
      # bio dad
      biodad_childbirth_age = `play_demo_questionnaire/group_biodad/biodad_childbirth_age`,
      biodad_race = `play_demo_questionnaire/group_biodad/biodad_race`,
      biodad_ethnicity = `play_demo_questionnaire/group_biodad/biodad_ethnicity`,
      # family structure
      household_members = `play_demo_questionnaire/group_family_structure/household_members`
    )
}

#-------------------------------------------------------------------------------
#' Clean subset of screening/demo variables
#'
#' clean_2() cleans the "new" demographic screening form for Spanish-speaking
#' parents.
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
      child_birth_complications_specify = `play_demo_questionnaire/child_information/specify_birth_complications`,
      child_hearing_disabilities = `play_demo_questionnaire/child_information/hearing_disabilities`,
      child_hearing_disabilities_specify = `play_demo_questionnaire/child_information/specify_hearing`,
      child_vision_disabilities = `play_demo_questionnaire/child_information/vision_disabilities`,
      child_vision_disabilities_specify = `play_demo_questionnaire/child_information/specify_vision`,
      major_illnesses_injuries = `play_demo_questionnaire/child_information/major_illnesses_injuries`,
      major_illnesses_injuries_specify = `play_demo_questionnaire/child_information/specify_illnesses_injuries`,
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
      childcare_age = `play_demo_questionnaire/group_child_care_arrangements/childcare_age`,
      childcare_language = `play_demo_questionnaire/group_child_care_arrangements/childcare_language`,
      # bio dad
      biodad_childbirth_age = `play_demo_questionnaire/group_biodad/biodad_childbirth_age`,
      biodad_race = `play_demo_questionnaire/group_biodad/biodad_race`,
      biodad_ethnicity = `play_demo_questionnaire/group_biodad/biodad_ethnicity`,
      # family structure
      household_members = `play_demo_questionnaire/group_family_structure/household_members`
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
      child_birth_complications_specify = `play_phone_questionnaire/child_information/specify_birth_complications`,
      child_hearing_disabilities = `play_phone_questionnaire/child_information/hearing_disabilities`,
      child_hearing_disabilities_specify = `play_phone_questionnaire/child_information/specify_hearing`,
      child_vision_disabilities = `play_phone_questionnaire/child_information/vision_disabilities`,
      child_vision_disabilities_specify = `play_phone_questionnaire/child_information/specify_vision`,
      major_illnesses_injuries = `play_phone_questionnaire/child_information/major_illnesses_injuries`,
      major_illnesses_injuries_specify = `play_phone_questionnaire/child_information/specify_illnesses_injuries`,
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
      childcare_age = `play_phone_questionnaire/group_child_care_arrangements/childcare_age`,
      childcare_language = `play_phone_questionnaire/group_child_care_arrangements/childcare_language`,
      # bio dad
      biodad_childbirth_age = `play_phone_questionnaire/parent_information/father_information/father_childbirth_age`,
      biodad_race = `play_phone_questionnaire/parent_information/father_information/father_race`,
      biodad_ethnicity = `play_phone_questionnaire/parent_information/father_information/father_ethnicity`,
      # family structure
      household_members = `play_phone_questionnaire/group_family_structure/household_members`
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

#-------------------------------------------------------------------------------
test_clean <- function(fn) {
  stopifnot(is.character(fn))
  stopifnot(file.exists(fn))
  
  box::use(readr[read_csv, cols])
  box::use(. / geo)
  
  # Import all as character to avoid type guess conflicts
  df <-
    readr::read_csv(fn,
                    col_types = readr::cols(.default = 'c'),
                    show_col_types = FALSE)
  
  message("Cleaning '", fn, "'.")
  
  df |>
    remove_variable_headers() |>
    geo$make_addresses("new") |>
    geo$get_multiple_census_geos() |>
    remove_variable_identifiers() |>
    remove_metadata_fields() |>
    remove_databrary_fields()
}

#-------------------------------------------------------------------------------
test_join <- function() {
  box::use(dplyr[full_join])
  
  fl <- list.files("data/csv/screening", full.names = TRUE)
  
  d1c <- test_clean(fl[1])
  d2c <- test_clean(fl[2])
  d3c <- test_clean(fl[3])
  
  m1 <- dplyr::full_join(d3c, d2c)
  dplyr::full_join(m1, d1c)
}

#-------------------------------------------------------------------------------
test_two_addr <- function() {
  box::use(tidygeocoder[geocode])
  my <-
    data.frame(singlelineaddr = c("608 East Prospect Avenue, State College, PA 16801", 
                                  "7472 West Layton Way, Littleton, CO 80123"))
  tidygeocoder::geocode(my,
    address = singlelineaddr,
    method = "census",
    full_results = TRUE,
    api_options = list(census_return_type = 'geographies')
  )
}

#-------------------------------------------------------------------------------
clean_mom_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  df |>
    tidyr::unite(col = "mom_childbirth_age", 
                 c("group_mominfo/mom_childbirth_age",
                   "parent_information/mother_information/mother_childbirth_age"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_race",
                 c("group_mominfo/mom_race", "parent_information/mother_information/mother_race"),
                 na.rm = TRUE)
                 
}

#-------------------------------------------------------------------------------
clean_lang_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(tidyr[unite])
  box::use(dplyr[rename, select])
  
  df |>
    tidyr::unite(col = "language_spoken_home", 
                 c("language_spoken_house", "language_spoken_home"), 
                 na.rm = TRUE) |>
    dplyr::rename(language_spoken_child_comments = language_spoken_child_other) |>
    dplyr::rename(language_spoken_home_comments = language_spoken_home_other) |>
    dplyr::rename(language_spoken_mom_comments = language_spoken_mom_other) |>
    dplyr::select(-contains(c("/english", "/spanish", "/other")))
}

#-------------------------------------------------------------------------------
clean_play_id <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(tidyr[unite])
  df |>
    tidyr::unite(col = "play_id", c("play_id", "concat2"), na.rm = TRUE)
    
}

#-------------------------------------------------------------------------------
test_combine_fields <- function(df) {
  # stopifnot(is.character(fn))
  # stopifnot(file.exists(fn))
  # 
  # box::use(readr[read_csv, cols])
  box::use(tidyr[unite])
  box::use(dplyr[mutate])
  
  # df <-
  #   readr::read_csv(fn,
  #                   col_types = readr::cols(.default = 'c'),
  #                   show_col_types = FALSE)
  
  df |> 
    tidyr::unite(col = "play_id", c("play_id", "concat2"), na.rm = TRUE) |>
    tidyr::unite(col = "language_spoken_home", 
                 c("language_spoken_house", "language_spoken_home"), 
                   na.rm = TRUE) |>
    dplyr::select(-contains(c("/english", "/spanish", "/other"))) |> 
    tidyr::unite(col = "mom_childbirth_age", c("group_mominfo/mom_childbirth_age",
                                               "parent_information/mother_information/mother_childbirth_age")) |>
    dplyr::rename(child_age_weeks = check_childage)
}

