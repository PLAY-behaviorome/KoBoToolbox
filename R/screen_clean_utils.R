# screening (demographic survey) variable cleaning functions

#-------------------------------------------------------------------------------
#' Removes variable header/group info from screening data
#'
#'@param df A data frame of demographic/screening data.
#'@returns A data frame with renamed column names.
screen_remove_variable_headers <- function(df) {
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
screen_remove_databrary_fields <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(dplyr[select, contains])
  
  dplyr::select(df,-contains('group_databrary'))
}

#-------------------------------------------------------------------------------
screen_remove_identifiers <- function(df, vb = TRUE) {
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
#' Remove unneeded fields from screening data frame.
#'
#' @param df A data frame of screening data.
#' @returns A data frame with some fields removed.
screen_remove_metadata_fields <- function(df) {
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
#' @export
screen_clean_child_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(tidyr[unite])
  box::use(dplyr[rename_with])
  
  df |>
    tidyr::unite(col = "child_illnesses_injuries_specify", c("child_information/specify_illnesses_injuries",
                                                            "child_information/indicate_injuries_illnesses"),
                 na.rm = TRUE) |>
    dplyr::rename_with(~ gsub("child_information/", "", .x, fixed = TRUE)) |>
    dplyr::rename(child_birth_complications_specify = specify_birth_complications) |>
    dplyr::rename(child_hearing_disabilities = hearing_disabilities) |>
    dplyr::rename(child_hearing_disabilities_specify = specify_hearing) |>
    dplyr::rename(child_vision_disabilities = vision_disabilities) |>
    dplyr::rename(child_vision_disabilities_specify = specify_vision) |>
    dplyr::rename(child_major_illnesses_injuries = major_illnesses_injuries) |>
    dplyr::rename(child_developmentaldelays = other_developmentaldelays) |>
    dplyr::rename(child_developmentaldelays_specify = specify_developmentaldelays) |>
    dplyr::rename(child_sleep_location_specify = specify_child_sleep_location) |>
    dplyr::rename(child_age_mos = check_childage) |>
    dplyr::mutate(subject_number = stringr::str_pad(subject_number, 3, "left", "0"))
}

#-------------------------------------------------------------------------------
screen_clean_childcare_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(dplyr[rename_with, select])
  
  df |>
    dplyr::rename_with(~ gsub("group_child_care_arrangements/", "", .x, fixed = TRUE)) |>
    dplyr::select(-contains(c("/nanny", "/relative", "/childcare", "/none")))
}

#-------------------------------------------------------------------------------
screen_clean_lang_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(tidyr[unite])
  box::use(dplyr[rename, select])
  
  df |>
    tidyr::unite(col = "language_spoken_home", 
                 c("language_spoken_house", "language_spoken_home"), 
                 na.rm = TRUE) |>
    tidyr::unite(col = "language_spoken_home_comments", c(22, 127), na.rm = TRUE) |>
    dplyr::rename(language_spoken_child_comments = language_spoken_child_other) |>
    dplyr::rename(language_spoken_mom_comments = language_spoken_mom_other) |>
    dplyr::select(-contains(c("/english", "/spanish", "/other")))
}

#-------------------------------------------------------------------------------
screen_clean_play_id <- function(df) {
  stopifnot(is.data.frame(df))
  
  df |>
    tidyr::unite(col = "play_id", c("play_id", "concat2"), na.rm = TRUE)
}

#-------------------------------------------------------------------------------
screen_clean_biodad_father_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(tidyr[unite])
  
  df |>
    tidyr::unite(col = biodad_childbirth_age, 
                 c("group_biodad/biodad_childbirth_age", 
                   "parent_information/father_information/father_home")) |>
    tidyr::unite(col = biodad_race,
                 c("group_biodad/biodad_race", "parent_information/father_information/father_race")) |>
    dplyr::select(-`group_biodad/biodad_childbirth_age_over21`)
}

#-------------------------------------------------------------------------------
screen_clean_mom_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(tidyr[unite])
  
  df |>
    tidyr::unite(col = "mom_childbirth_age", 
                 c("group_mominfo/mom_childbirth_age",
                   "parent_information/mother_information/mother_childbirth_age"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_race",
                 c("group_mominfo/mom_race", "parent_information/mother_information/mother_race"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_bio", c("group_mominfo/mom_biological", "group_mominfo/mom_relation"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_birth_country", c("group_mominfo/mom_birth_country", 
                                              "parent_information/mother_information/mother_birth_country"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_birth_country_specify", c("group_mominfo/specify_mom_birth_country",
                                                      "parent_information/mother_information/specify_mother_birth_country"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_education", c("group_mominfo/mom_education", "parent_information/mother_information/mother_education"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_employment", c("group_mominfo/mom_employment",
                                           "parent_information/mother_information/mother_employment"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_occupation", c("group_mominfo/mom_occupation", 
                                           "parent_information/mother_information/mother_occupation"), na.rm = TRUE) |>
    tidyr::unite(col = "mom_jobs_number", c("group_mominfo/mom_jobs_number",
                                            "parent_information/mother_information/mother_jobs_number"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_training", c("group_mominfo/mom_training", "parent_information/mother_information/mother_training"),
                 na.rm = TRUE)
}

#-------------------------------------------------------------------------------
screen_clean_family_structure <- function(df) {
  stopifnot(is.data.frame(df))
  
  df |>
    dplyr::rename(child_onlychild = `group_family_structure/only_child`) |>
    dplyr::rename(child_onlychild_specify = `group_family_structure/specify_onlychild`) |>
    dplyr::rename(household_members = `group_family_structure/household_members`)
}

#-------------------------------------------------------------------------------
screen_recode_site_id <- function(df) {
  stopifnot(is.data.frame(df))
  
  df |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "new_york_unive", "NYUNI")) |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "^NYU$", "NYUNI")) |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "georgetown_uni", "GEORG")) |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "^GTN$", "GEORG")) |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "^UCR$", "UCRIV")) |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "^university_of__3", "?????"))
}

#-------------------------------------------------------------------------------
screen_clean_raw_csv <- function(fn) {
  stopifnot(is.character(fn))
  stopifnot(file.exists(fn))
  
  # Import all as character to avoid type guess conflicts
  df <-
    readr::read_csv(fn,
                    col_types = readr::cols(.default = 'c'),
                    show_col_types = FALSE)
  
  message("Cleaning '", fn, "'.")
  
  df |>
    screen_remove_variable_headers() |>
    # geo$make_addresses("new") |>
    # geo$get_multiple_census_geos() |>
    screen_remove_identifiers() |>
    screen_remove_metadata_fields() |>
    screen_remove_databrary_fields()
}

#-------------------------------------------------------------------------------
screen_clean_raw_join <- function() {
  
  fl <- list.files(file.path(here::here(), "data/csv/screening"), full.names = TRUE)
  
  d1c <- screen_clean_raw_csv(fl[1])
  d2c <- screen_clean_raw_csv(fl[2])
  d3c <- screen_clean_raw_csv(fl[3])
  
  m1 <- dplyr::full_join(d3c, d2c)
  dplyr::full_join(m1, d1c)
}

#-------------------------------------------------------------------------------
screen_remove_selected_cols <- function(df) {
  df |>
    dplyr::select(-end,
                  -c_today,
                  -update_date,
                  -day,
                  -day2,
                  -day1,
                  -contains("group_family_structure"),
                  -contains("parent_information"),
                  -concat1,
                  -Participant_ID_concat2,
                  -contains("NOTA_El_"),
                  -state)
}

#-------------------------------------------------------------------------------
screen_select_reorder_cols <- function(df) {
  stopifnot(is.data.frame(df))
  
  df |>
    dplyr::rename(submit_date = start) |>
    dplyr::select(submit_date,
                  site_id,
                  subject_number,
                  play_id,
                  child_age_mos,
                  child_sex,
                  child_bornonduedate,
                  child_onterm,
                  child_birthage,
                  child_weight_pounds,
                  child_weight_ounces,
                  child_birth_complications,
                  child_birth_complications_specify,
                  child_hearing_disabilities,                
                  child_hearing_disabilities_specify,        
                  child_vision_disabilities,                 
                  child_vision_disabilities_specify,
                  child_major_illnesses_injuries,         
                  child_illnesses_injuries_specify,
                  child_developmentaldelays,           
                  child_developmentaldelays_specify,
                  child_sleep_time,
                  child_wake_time,
                  child_nap_hours,
                  child_sleep_location,
                  mom_bio,
                  mom_childbirth_age,
                  mom_race,
                  mom_birth_country,
                  mom_birth_country_specify,
                  mom_education,
                  mom_employment,
                  mom_occupation,
                  mom_jobs_number,
                  mom_training,
                  biodad_childbirth_age,
                  biodad_race,
                  contains("language_spoken"),
                  contains("childcare_"))
}


#-------------------------------------------------------------------------------
# screen_clean_fields <- function(df) {
#   stopifnot(is.data.frame(df))
#   
#   df |> 
#     screen_clean_child_info() |>
#     screen_clean_lang_info() |>
#     screen_clean_mom_info() |>
#     screen_clean_biodad_father_info() |>
#     screen_clean_childcare_info() |>
#     screen_clean_family_structure() |>
# #    screen_clean_play_id() |>
#     screen_remove_selected_cols() |>
#     screen_select_reorder_cols() |>
#     screen_recode_site_id() |>
#     dplyr::rename("participant_ID" = "subject_number")
# }

# #-------------------------------------------------------------------------------
# screen_add_db_vol_id <- function(df) {
#   PLAY_VOLS <-
#     readr::read_csv(
#       "../data/csv/play_site_vols.csv",
#       col_types = readr::cols(.default = 'c'),
#       show_col_types = FALSE
#     )
# 
#   dplyr::left_join(df, PLAY_VOLS, by = 'site_id') |>
#     dplyr::select(-c('play_site_id', 'site_name'))
# }

retrieve_db_session_status <- function(i = 1, df, vb = FALSE) {
  this_row <- df[i, ]
  this_vol_id <- this_row$play_vol_id
  this_sub_num <- this_row$subject_number
  
  if (vb) message("Retrieving session info from Databrary volume '", this_vol_id, "'")
  vol_sessions <- databraryr::download_session_csv(as.numeric(this_vol_id), as_df = TRUE)
  
  assertthat::not_empty(vol_sessions)
  
  if (vb) message("Selecting participant ", this_sub_num)
  if (this_sub_num %in% vol_sessions$`participant-ID`) {
    this_session <- vol_sessions |>
      dplyr::filter(`participant-ID` == this_sub_num)
    if ('group-name' %in% names(this_session)) {
      return(this_session$`group-name`)     
    } else {
      return(NA)
    }
  } else {
    if (vb) message("Participant-ID '", this_sub_num, "' not found in volume '", this_vol_id, "'")
    return(NA)
  }
}