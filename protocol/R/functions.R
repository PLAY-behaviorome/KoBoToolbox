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

###################################################################
#' Retrieves an XLS formatted export file from KoBoToolbox and
#' saves it to a local directory.
#'
#' @param form_index An integer value indicating which row (KoBoToolbox form)
#' should be retrieved and saved. The default value for testing purposes is 13.
#' @param kb_df A data frame with the available KoBoToolbox data, typically
#' output of list_kobo_data(). The default is to call list_kobo_data().
#' @param save_dir A character string indicating the directory to save the
#' exported file. The default is 'tmp'.
#' @return The name of the saved .xls file.
retrieve_save_xls_export <-
  function(form_index = 13,
           kb_df = list_kobo_data(),
           save_dir = 'tmp') {
    if (!is.numeric(form_index)) {
      stop('`form_index` must be a number')
    }
    if (form_index <= 0) {
      stop('`form_index` must be > 0')
    }
    if (!is.character(save_dir)) {
      stop('`save_dir` must be a character string')
    }
    if (!dir.exists(save_dir)) {
      stop(paste0('Directory not found: ', save_dir))
    }
    if (is.null(kb_df)) {
      message('Unable to return list of KoBoToolbox forms.')
      NULL
    } else {
      form_URL <- paste0(kb_df$url[form_index], '.xls')
      # Append KoBo form id to cleaned file name to avoid duplicates
      form_name <-
        create_cleaned_augmented_form_name(form_index, kb_df)
      # Must export as .xlsx even though API queries for .xls
      file_name <- file.path(save_dir, paste0(form_name, '.xlsx'))
      
      require(httr)
      
      kobo_api_key <- Sys.getenv("KOBO_API_KEY")
      if (!is.character(kobo_api_key)) {
        stop('No KoBoToolbox API key stored in ~/.Renviron.')
      }
      
      require(httr)
      
      config_params <-
        httr::add_headers(Authorization = paste0('Token ', kobo_api_key))
      r <- httr::GET(form_URL, config = config_params)
      if (httr::status_code(r) == 200) {
        c <- httr::content(r, as = 'raw')
        writeBin(c, file_name)
        message('Saved `', file_name, '`')
        file_name
      } else {
        message('HTTP call to ',
                form_URL,
                ' failed with status `',
                httr::status_code(r),
                '`')
        NULL
      }
    }
  }

create_cleaned_augmented_form_name <-
  function(form_index = 13,
           kb_df = list_kobo_data()) {
    if (!is.numeric(form_index)) {
      stop('`form_index` must be a number')
    }
    if (form_index <= 0) {
      stop('`form_index` must be > 0')
    }
    if (is.null(kb_df)) {
      message('Unable to return list of KoBoToolbox forms.')
      NULL
    } else {
      paste0(
        extract_kb_form_id(form_index, kb_df),
        "_",
        clean_kobo_form_name(extract_kb_form_name(form_index, kb_df))
      )
    }
  }

###################################################################
#' Remove spaces and parentheses from a KoBoToolbox form name.
#'
#' @param kb_form_name A form name.
#' @return A string containing the reformatted filename.
clean_kobo_form_name <- function(kb_form_name) {
  if (!is.character(kb_form_name)) {
    stop('`kb_form_name` must be a character string')
  }
  
  require(stringr)
  
  fn <-
    stringr::str_replace_all(kb_form_name, pattern = "[ ]+", "_")
  stringr::str_replace_all(fn, pattern = '[\\(,\\)]', "")
}

###################################################################
#' Extract the id of an available dataset from a list of
#' available datasets.
#'
#' @param form_index An integer value indicating which row (KoBoToolbox form)
#' should be retrieved and saved. The default value for testing purposes is 13.
#' @param kb_df A data frame with the available KoBoToolbox data, typically
#' output of list_kobo_data(). The default is to call list_kobo_data().
#' @return A number
extract_kb_form_id <-
  function(form_index = 13,
           kb_df = list_kobo_data()) {
    if (!is.numeric(form_index)) {
      stop('`form_index` must be a number')
    }
    if (form_index <= 0) {
      stop('`form_index` must be > 0')
    }
    if (is.null(kb_df)) {
      message('Unable to return list of KoBoToolbox forms.')
      NULL
    } else {
      kb_df$id[form_index]
    }
  }

###################################################################
#' Extract the name of an available dataset from a list of
#' available datasets.
#'
#' @param form_index An integer value indicating which row (KoBoToolbox form)
#' should be retrieved and saved. The default value for testing purposes is 13.
#' @param kb_df A data frame with the available KoBoToolbox data, typically
#' output of list_kobo_data(). The default is to call list_kobo_data().
#' @return A string.
extract_kb_form_name <-
  function(form_index = 13,
           kb_df = list_kobo_data()) {
    if (!is.numeric(form_index)) {
      stop('`form_index` must be a number')
    }
    if (form_index <= 0) {
      stop('`form_index` must be > 0')
    }
    if (is.null(kb_df)) {
      message('Unable to return list of KoBoToolbox forms.')
      NULL
    } else {
      kb_df$title[form_index]
    }
  }

retrieve_screening_xlsx <- function(df, save_dir) {
  n_files <- dim(df)[1]
  purrr::map(1:n_files, retrieve_save_xls_export, kb_df = df, save_dir = save_dir)
}

load_xlsx_save_csv <- function(fl, out_dir) {
  require(tools)
  xl <- readxl::read_xlsx(fl)
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
        clean_demog_2(fns[3])) %>%
    dplyr::arrange(., submit_date)
}

clean_demog_1 <- function(csv_fn_1) {
  df_1 <- readr::read_csv(csv_fn_1, show_col_types = FALSE)
  df_1 %>%
    dplyr::select(., submit_date = c_today, 
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

clean_demog_2 <- function(csv_fn_2) {
  df_2 <- readr::read_csv(csv_fn_2, show_col_types = FALSE)
  df_2 %>%
    dplyr::select(submit_date = c_today, 
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

select_demog <- function(df, string) {
  require(dplyr)
  df %>% 
    select(contains(string))
}
