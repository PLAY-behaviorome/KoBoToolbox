# protocol/R/functions.R

###################################################################
#' Lists the datasets available on KoBoToolbox using the PLAY credentials.
#' #'
#' @param URL A string that is the API call to extract the data. It defaults to 'https://kc.kobotoolbox.org/api/v1/data'
#' @param return_df A logical value indicating whether to return a data frame (the default) or JSON.
#'
#' @return A data.frame with the 'description', 'title', 'url', and other information for the deployed forms on PLAY.
list_kobo_data <-
  function(URL = "https://kc.kobotoolbox.org/api/v1/data",
           return_df = TRUE) {
    if (!is.character(URL)) {
      stop("`URL` must be a character string.")
    }
    
    require(httr)
    
    kobo_api_key <- Sys.getenv("KOBO_API_KEY")
    if (!is.character(kobo_api_key)) {
      stop('No KoBoToolbox API key stored in ~/.Renviron.')
    }
    
    config_params <-
      httr::add_headers(Authorization = paste0('Token ', kobo_api_key))
    
    r <- httr::GET(URL, config = config_params)
    if (httr::status_code(r) == 200) {
      c <- httr::content(r, as = 'text', encoding = 'utf8')
      if (return_df) {
        jsonlite::fromJSON(c)
      } else {
        # JSON
        c
      }
    } else {
      message('HTTP call to ',
              URL,
              ' failed with status `',
              httr::status_code(r),
              '`.')
      NULL
    }
  }

load_xlsx_save_csv <- function(fl, out_dir) {
  require(tools)
  xl <- readxl::read_xlsx(fl)
  # fn_csv <- stringr::str_replace(fl, pattern = "\\.xlsx", replacement = "\\.csv")
  # fn_csv <- stringr::str_replace(fn_csv, pattern = "/xlsx/", "/csv/")
  fn_csv <- file.path(out_dir, paste0(file_path_sans_ext(basename(fl)), ".csv"))
  if (dir.exists(dirname(fn_csv))) {
    readr::write_csv(xl, fn_csv)
    message("File saved: '", fn_csv, "'")
  } else {
    warning("Directory not found: '", dirname(fn_csv), "'")
  }
}

load_screening_xlsx_save_csv <- function(in_dir, out_dir) {
  require(purrr)
  demog_fns <- list.files(file.path(in_dir), 
                          pattern = "Demographic_Questionnaire", full.names = TRUE)
  purrr::map(demog_fns, load_xlsx_save_csv, out_dir)
}

clean_merge_demog <- function(fns) {
  rbind(clean_demog_1(fns[1]), 
        clean_demog_2(fns[2]), 
        clean_demog_3(fns[3])) %>%
    dplyr::arrange(., submit_date)
}

clean_demog_1 <- function(csv_fn_1) {
  df_1 <- readr::read_csv(csv_fn_1, show_col_types = FALSE)
  #names(df_1) <- basename(names(df_1))
  df_1 %>%
    #dplyr::mutate(., submit_date = c_today) %>%
    dplyr::select(., submit_date = c_today, 
                  site_id = `play_phone_questionnaire/group_siteinfo/site_id`,
                  sub_num = `play_phone_questionnaire/group_siteinfo/subject_number`,
                  child_age_mos = `play_phone_questionnaire/check_childage`,
                  child_sex = `play_phone_questionnaire/child_sex`,
                  language_spoken_home = `play_phone_questionnaire/language_spoken_home`,
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

clean_demog_2 <- function(csv_fn_2) {
  df_2 <- readr::read_csv(csv_fn_2, show_col_types = FALSE)
  df_2 %>%
    dplyr::select(submit_date = c_today, 
                  site_id = `play_demo_questionnaire/group_siteinfo/site_id`,
                  sub_num = `play_demo_questionnaire/group_siteinfo/subject_number`,
                  child_age_mos = `play_demo_questionnaire/check_childage`,
                  child_sex = `play_demo_questionnaire/child_sex`,
                  language_spoken_home = `play_demo_questionnaire/language_spoken_house`,
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

clean_demog_3 <- function(csv_fn_3) {
  df_3 <- readr::read_csv(csv_fn_3, show_col_types = FALSE)
  df_3 %>%
    dplyr::select(submit_date = c_today, 
                  site_id = `play_demo_questionnaire/group_siteinfo/site_id`,
                  sub_num = `play_demo_questionnaire/group_siteinfo/subject_number`,
                  child_age_mos = `play_demo_questionnaire/check_childage`,
                  child_sex = `play_demo_questionnaire/child_sex`,
                  language_spoken_home = `play_demo_questionnaire/language_spoken_house`,
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