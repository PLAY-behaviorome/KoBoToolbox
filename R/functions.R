# R/functions.R

###################################################################
#' Cleans/empties a target directory, usually one containing data
#' @param update_data Logical value indicating whether to clean the directory.
#' @param data_dir Directory to clean
clean_data_dir <- function(update_data, data_dir) {
  if (update_data) {
    unlink(paste0(data_dir, "/*"))
  }
}

###################################################################
#' Lists the datasets available on KoBoToolbox using the PLAY credentials.
#'
#' @param URL A string that is the API call to extract the data. It defaults
#' to 'https://kc.kobotoolbox.org/api/v1/data'
#' @param return_df A logical value indicating whether to return a data frame
#' (the default) or JSON.
#' @return A data.frame with the 'description', 'title', 'url', and other
#' information for the deployed forms on PLAY.
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

###################################################################
#' Normalizes the file/form names of files on KoBoToolbox
#'
#' @param form_index An integer value indicating which row (KoBoToolbox form)
#' should be retrieved and saved. The default value for testing purposes is 13.
#' @param kb_df A data frame with the available KoBoToolbox data, typically
#' output of list_kobo_data(). The default is to call list_kobo_data().
#' @return An array of strings
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

###################################################################
#' Retrieves demographic/screening or home visit forms from KoBoToolbox and
#' saves them in a local directory.
#'
#' @param df A dataframe of the selected forms from the KoBoToolbox API
#' @param save_dir A character string indicating the directory to save
#' the downloaded files.
#' @returns NULL
retrieve_kobo_xlsx <- function(df, save_dir) {
  require(purrr)
  
  stopifnot("df must be a data frame" = is.data.frame(df))
  stopifnot("save_dir not found" = dir.exists(save_dir))
  
  n_files <- dim(df)[1]
  purrr::map(1:n_files,
             retrieve_save_xls_export,
             kb_df = df,
             save_dir = save_dir)
}

###################################################################
#' Loads a single XLSX-formatted data file, converts it to CSV, and saves it.
#'
#' @param fn Filename for XLSX file
#' @param out_dir Directory to save CSV file
load_xlsx_save_csv <- function(fn, out_dir) {
  require(tools)
  require(readxl)
  require(readr)
  
  stopifnot("fn not found" = file.exists(fn))
  stopifnot("out_dir not found" = dir.exists(out_dir))
  
  xl <- readxl::read_xlsx(fn)
  fn_csv <-
    file.path(out_dir, paste0(file_path_sans_ext(basename(fn)), ".csv"))
  if (dir.exists(dirname(fn_csv))) {
    readr::write_csv(xl, fn_csv)
    message("File saved: '", fn_csv, "'")
  } else {
    warning("Directory not found: '", dirname(fn_csv), "'")
  }
}

###################################################################
#' Loads all XLSX-formatted files in `in_dir` and saves CSV-formatted files
#' to `out_dir`
#'
#' @param in_dir A string indicating the input directory
#' @param out_dir A string indicating the output directory
load_xlsx_save_many_csvs <- function(in_dir, out_dir, filter_str) {
  require(purrr)
  
  # stopifnot("in_dir not found" = dir.exists(in_dir))
  # stopifnot("out_dir not found" = dir.exists(out_dir))
  
  fns <- list.files(in_dir, pattern = filter_str, full.names = TRUE)
  if (length(fns) < 1) {
    warning("fns is empty")
    NULL
  } else {
    purrr::map(fns, load_xlsx_save_csv, out_dir)
  }
}

###################################################################
#' Cleans set of screening/demographic data files and aggregates them into
#' a single data frame.
#'
#' @param fns A character vector of CSV filenames.
#' @return A single dataframe with the aggregated demographic data files.
clean_merge_demog <- function(fns) {
  rbind(clean_demog_1(fns[1]),
        clean_demog_2(fns[2]),
        clean_demog_2(fns[3])) %>%
    dplyr::arrange(., submit_date)
}

###################################################################
#' Cleans the old/original demographic/screening form by selecting
#' a subset of variables
#'
#' @param csv_fn A filename for the CSV file associated with the original
#' screening form
#' @return A data frome
clean_demog_1 <- function(csv_fn) {
  require(readr)
  
  stopifnot("csv_fn not found" = file.exists(csv_fn))
  
  df_1 <- readr::read_csv(csv_fn, show_col_types = FALSE)
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
    dplyr::filter(., !stringr::str_detect(site_id, '_of__'))
}

###################################################################
#' Cleans new demographic/screening form by selecting
#' a subset of variables.
#'
#' @param csv_fn A filename for the CSV file associated with the original
#' screening form
#' @return A data frome
clean_demog_2 <- function(csv_fn) {
  require(readr)
  require(dplyr)
  
  stopifnot("csv_fn not found" = file.exists(csv_fn))
  
  df_2 <- readr::read_csv(csv_fn, show_col_types = FALSE)
  df_2 %>%
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
#' Augments cleaned demographic/screening data frame with 'n_calls'
#' variable starting at the beginning of the project.
#'
#' @param df Demographic/screening data frame.
#' @return Augmented data frame.
add_n_calls_to_demog <- function(df) {
  require(dplyr)
  df %>%
    dplyr::arrange(., submit_date) %>%
    dplyr::mutate(., n_calls = seq_along(submit_date))
}

###################################################################
#' Creates a time series plot of the cumulative number of recruiting/screening
#' calls.
#'
#' @param df Augmented data frame.
#' @return Plot
plot_call_timeseries <- function(df) {
  require(dplyr)
  require(ggplot2)
  df %>%
    filter(.,!is.na(submit_date),!is.na(n_calls),!is.na(site_id)) %>%
    ggplot(.) +
    aes(submit_date, n_calls, color = site_id) +
    geom_point()
}

###################################################################
#' Create bar plot summarizing total calls by site.
#'
#' @param df Augmented data frame.
#' @return Plot
plot_calls_by_site <- function(df) {
  require(dplyr)
  df %>%
    ggplot(.) +
    aes(site_id, fill = site_id) +
    geom_bar() +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )) # Rotate text
}

###################################################################
#' Standardizes the format of the home visit questionnaire XLSX
#' data files.
#'
#' @param in_dir Directory where the XLSX files are stored
#' @return Results of `file.rename()`
rename_home_xlsx <- function(in_dir) {
  require(stringr)
  require(purrr)
  
  #stopifnot("in_dir not found" = dir.exists(in_dir))
  
  fl <- list.files(in_dir, full.names = TRUE)
  fl_home_old <- fl[stringr::str_detect(fl, 'Home')]
  fl_home_new <-
    purrr::map_chr(fl_home_old, make_standard_form_name)
  file.rename(fl_home_old, fl_home_new)
}

###################################################################
#' Helper function called by rename_home_xlsx() to create standard
#' file name.
#'
#' @param fn Old file name
#' @return New file name and path
make_standard_form_name <- function(fn) {
  this_dir <- dirname(fn)
  this_fn <- basename(fn)
  form_id <- stringr::str_extract(this_fn, '[0-9]+')
  fn <- paste0(
    form_id,
    "_PLAY_HomeQuestionnares",
    "_",
    extract_age_group_from_name(this_fn),
    "_",
    form_language(this_fn),
    '.xlsx'
  )
  file.path(this_dir, fn)
}

###################################################################
#' Helper function to extract the age group from a file name.
#'
#' @param form_name File (form) name.
#' @return Age group as a string
extract_age_group_from_name <- function(form_name) {
  age_grps <- stringr::str_match(form_name, "[ _\\(]+(12|18|24)[ _]+")
  age_grps[, 2]
}

###################################################################
#' Helper function to detect whether a file/form contains data from
#' a bilingual participant family based on the file/form name.
#'
#' @param form_name
#' @return Logical vector of names that contain the detected string.
form_is_bilingual <- function(form_name) {
  stringr::str_detect(form_name, "[Bb]ilingual")
}

###################################################################
#' Helper function to detect whether a file/form contains data from
#' a Spanish-speaking family.
#'
#' @param form_name
#' @return Logical vector of names that contain the detected string.
form_is_spanish <- function(form_name) {
  stringr::str_detect(form_name, "[Ss]panish")
}

###################################################################
#' Helper function to determine the language spoken by the participant
#' family based on information in the file/form name.
#'
#' @param form_name
#' @return String with appropriate language indicator.
form_language <- function(form_name) {
  is_bilingual <- form_is_bilingual(form_name)
  is_spanish <- form_is_spanish(form_name)
  form_lang <- rep("english", length(form_name))
  form_lang[is_bilingual & is_spanish] <- "bilingual_spanish"
  form_lang[is_bilingual & !is_spanish] <- "bilingual_english"
  form_lang
}

###################################################################
#' Opens a home visit CSV file;  extracts non_mbcdi or mbcdi
#' questions, and saves a new file with the selected questions.
#'
#' @param fp CSV filename
#' @param csv_save_dir Directory for the saved files.
#' @param these_questions String indicating non_mbcdi or mbcdi questions should
#' be extracted and saved.
#' @param rename_cols Logical value indicating whether to rename the columns
#' using the basename() function.
#' @param vb Logical value. Produce verbose output or not.
open_split_save <- function(fp,
                            csv_save_dir = 'tmp',
                            these_questions = 'non_mbcdi',
                            rename_cols = FALSE,
                            vb = TRUE) {
  if (file.exists(fp)) {
    df <- readr::read_csv(fp, show_col_types = FALSE)
  } else {
    stop(paste0('Cannot read file `', fp, '`'))
  }
  
  out_fn <- file.path(
    csv_save_dir,
    paste0(
      stringr::str_extract(fp, '[0-9]+'),
      '_',
      these_questions,
      '_',
      extract_age_group_from_name(fp),
      '_',
      tolower(form_language(fp)),
      '.csv'
    )
  )
  
  if (vb)
    message("Output file directory: ", out_fn)
  
  if (!is.null(df)) {
    if (these_questions == 'non_mbcdi') {
      extract_save_non_mbcdi(df, out_fn, rename_cols)
    } else {
      extract_save_mcdi(df, out_fn, rename_cols)
    }
  } else {
    message('Error in exporting data to `', out_fn, '`')
  }
}

###################################################################
#' Extracts the non-MB-CDI questions from a home visit data frame.
#'
#' @param df Data frame
#' @param rename_cols Rename cols using basename()
#' @return Data frame with selected questions.
extract_non_mbcdi <-
  function(df,
           rename_cols = FALSE) {
    require(dplyr)
    
    play_id_col <-
      (1:length(names(df)))[stringr::str_detect(names(df), 'participant_id')]
    
    # Select non-mcdi cols
    mcdi_qs <- stringr::str_detect(names(df), 'mcdi|vocab')
    non_mcdi_qs <- !(mcdi_qs)
    non_mcdi_cols <- (1:length(names(df)))[non_mcdi_qs]
    
    non_mcdi <- df %>%
      dplyr::select(., all_of(play_id_col), all_of(non_mcdi_cols))
    
    if (rename_cols) {
      non_mcdi <- dplyr::rename_with(non_mcdi, basename)
    }
    
    non_mcdi
  }

###################################################################
#' Wrapper function to combine question extraction and file-saving
#' for the non-MB-CDI home visit questions.
#'
#' @param df Data frame
#' @param fn Output file name
#' @param rename_cols Rename cols using basename()
#' @return Saves CSV data file.
extract_save_non_mbcdi <-
  function(df,
           fn,
           rename_cols = FALSE) {
    require(dplyr)
    
    non_mcdi <-
      extract_non_mbcdi(df, rename_cols)
    readr::write_csv(non_mcdi, fn)
    message('Saved ', fn)
  }

###################################################################
#' Extracts the MB-CDI questions from a home visit data frame.
#' 
#' @param df Data frame
#' @param rename_cols Rename cols using basename()
#' @return Data frame with selected questions.
extract_mbcdi <-
  function(df,
           rename_cols = FALSE) {
    require(dplyr)
    play_id_col <-
      (1:length(names(df)))[stringr::str_detect(names(df), 'participant_id')]
    
    # Select non-mcdi cols
    mcdi_qs <- stringr::str_detect(names(df), 'mcdi|vocab')
    # non_mcdi_qs <- !(mcdi_qs)
    mcdi_cols <- (1:length(names(df)))[mcdi_qs]
    
    mcdi <- df %>%
      dplyr::select(., all_of(play_id_col), all_of(mcdi_cols))
    
    if (rename_cols) {
      mcdi <- dplyr::rename_with(mcdi, basename)
    }
    
    mcdi
  }

###################################################################
#' Wrapper function to combine question extraction and file-saving
#' for the MB-CDI home visit questions.
#' 
#' @param df Data frame
#' @param fn Output file name
#' @param rename_cols Rename cols using basename()
#' @return Saves CSV data file.
extract_save_mcdi <- function(df, fn, rename_cols = FALSE) {
  require(readr)
  require(dplyr)
  
  mcdi <- extract_mbcdi(df, rename_cols)
  
  if (rename_cols) {
    mcdi <- dplyr::rename_with(mcdi, basename)
  }
  
  readr::write_csv(mcdi, fn)
  message('Saved ', fn)
}

###################################################################
#' Opens a CSV, removes identifying columns, and saves new copy with
#' the identifiers removed.
#' 
#' @param fp Filename
#' @csv_save_dir Dirctory for saved file
#' @these_questions Filter to select type of files
#' @rename_cols Rename cols with basename()
#' @vb Provide verbose output
open_deidentify_save <- function(fp,
                                 csv_save_dir = 'tmp',
                                 these_questions = 'non_mbcdi',
                                 rename_cols = FALSE,
                                 vb = TRUE) {
  require(tidyverse)
  
  if (!dir.exists(csv_save_dir)) {
    stop("Directory not found: '", csv_save_dir, "'")
  }
  
  if (file.exists(fp)) {
    df <- readr::read_csv(fp, show_col_types = FALSE)
    if (is.data.frame(df)) {
      df <- remove_identifiers(df)
    } else {
      stop(paste0('Error in reading data frame.'))
    }
  } else {
    stop(paste0('Cannot read file `', fp, '`'))
  }
  
  out_fn <- file.path(
    csv_save_dir,
    paste0(
      stringr::str_extract(fp, '[0-9]{6}'),
      '_',
      these_questions,
      '_',
      extract_age_group_from_name(fp),
      '_',
      tolower(form_language(fp)),
      '_deidentified',
      '.csv'
    )
  )
  
  if (!is.null(df)) {
    readr::write_csv(df, out_fn)
    message('Saved `', out_fn, '`')
  } else {
    message('Error in exporting data to `', out_fn, '`')
  }
}

###################################################################
#' Detects presence of identifying information from column names
#' in a data frame then returns data frame without those columns.
#' 
#' @param df
#' @return Data frame with identifiers removed.
remove_identifiers <- function(df) {
  contains_name <- stringr::str_detect(names(df), 'name')
  contains_address <- stringr::str_detect(names(df), 'address')
  contains_phone <- stringr::str_detect(names(df), 'phone')
  contains_email <- stringr::str_detect(names(df), 'email')
  contains_birthdate <- stringr::str_detect(names(df), 'birthdate')
  contains_first <- stringr::str_detect(names(df), 'first[12]?')
  contains_last <- stringr::str_detect(names(df), 'last[12]?')
  contains_city <- stringr::str_detect(names(df), 'city')
  contains_year <- stringr::str_detect(names(df), 'year[12]?')
  contains_month <- stringr::str_detect(names(df), 'month[12]?')
  contains_day <- stringr::str_detect(names(df), '/day[12]?$')
  
  identifiable_data <- contains_name | contains_address |
    contains_phone |
    contains_email | contains_birthdate | contains_first |
    contains_last |
    contains_city | contains_year | contains_month | contains_day
  
  identifiable_cols <- (1:length(names(df)))[identifiable_data]
  
  df_deidentified <- df %>%
    dplyr::select(.,-all_of(identifiable_cols))
  
  df_deidentified
}
