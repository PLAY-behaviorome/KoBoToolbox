# R/functions.R

###################################################################
#' Cleans/empties a target directory, usually one containing data
#' @param update_data Logical value indicating whether to clean the directory.
#' @param data_dir Directory to clean
clean_data_dir <- function(update_data, data_dir) {
  stopifnot(is.logical(update_data))
  stopifnot(dir.exists(data_dir))
  
  if (update_data) {
    unlink(paste0(data_dir, "/*"))
  }
}

###################################################################
#' Creates data directories for workflow. These are hidden
#' from git, so we have to regenerate them whenever we create a new
#' branch.
create_data_dirs <- function() {
  dir.create("data/xlsx/home_visit/raw", recursive = TRUE)
  dir.create("data/xlsx/home_visit/std_name", recursive = TRUE)
  dir.create("data/xlsx/screening", recursive = TRUE)
  dir.create("data/xlsx/post_visit", recursive = TRUE)
  
  dir.create("data/csv/post_visit", recursive = TRUE)
  dir.create("data/csv/screening", recursive = TRUE)
  dir.create("data/csv/home_visit/raw", recursive = TRUE)
  dir.create("data/csv/home_visit/non_mbcdi", recursive = TRUE)
  dir.create("data/csv/home_visit/mbcdi", recursive = TRUE)
  dir.create("data/csv/home_visit/non_mbcdi/deid", recursive = TRUE)
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

    stopifnot(is.character(URL))
    stopifnot(is.logical(return_df))

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
        "NULL"
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
  stopifnot(is.data.frame(df))
  stopifnot(dir.exists(save_dir))
  
  n_files <- dim(df)[1]
  purrr::map_chr(1:n_files,
             retrieve_save_xls_export,
             kb_df = df,
             save_dir = save_dir)
}

###################################################################
make_screening_df <- function(kb_screen, xlsx_dir, csv_dir) {
  stopifnot(is.data.frame(kb_screen))
  stopifnot(dir.exists(xlsx_dir))
  stopifnot(dir.exists(csv_dir))
  
  retrieve_kobo_xlsx(kb_screen, xlsx_dir)
  load_xlsx_save_many_csvs(xlsx_dir, csv_dir, "Demographic")
  screen_csv_fns <- list.files(csv_dir, pattern = "[0-9]+.*\\.csv", full.names = TRUE)
  clean_merge_demog(screen_csv_fns)
}

###################################################################
make_post_visit_df <- function(kb_post_visit, xlsx_dir, csv_dir) {
  require(readr)
  stopifnot(is.data.frame(kb_post_visit))
  stopifnot(dir.exists(xlsx_dir))
  stopifnot(dir.exists(csv_dir))

  retrieve_kobo_xlsx(kb_post_visit, xlsx_dir)
  load_xlsx_save_many_csvs(xlsx_dir, csv_dir, "Post\\-Visit")
  #clean_merge_post_visit()
  readr::read_csv(file.path(csv_dir, "361981_PLAY_Post-Visit_Notes.csv"), 
                  show_col_types = FALSE)
}

###################################################################
#' Loads a single XLSX-formatted data file, converts it to CSV, and saves it.
#'
#' @param fn Filename for XLSX file
#' @param out_dir Directory to save CSV file
load_xlsx_save_csv <- function(fn, out_dir, vb = FALSE) {
  require(tools)
  require(readxl)
  require(readr)
  stopifnot(file.exists(fn))
  stopifnot(dir.exists(out_dir))
  
  xl <- readxl::read_xlsx(fn)
  fn_csv <-
    file.path(out_dir, paste0(file_path_sans_ext(basename(fn)), ".csv"))
  if (dir.exists(dirname(fn_csv))) {
    readr::write_csv(xl, fn_csv)
    if (vb) message("File saved: '", fn_csv, "'")
  } else {
    warning("Directory not found: '", dirname(fn_csv), "'")
  }
  fn_csv
}

###################################################################
#' Loads all XLSX-formatted files in `in_dir` and saves CSV-formatted files
#' to `out_dir`
#'
#' @param in_dir A string indicating the input directory
#' @param out_dir A string indicating the output directory
load_xlsx_save_many_csvs <- function(in_dir, out_dir, filter_str) {
  require(purrr)
  stopifnot(dir.exists(in_dir))
  stopifnot(dir.exists(out_dir))
  stopifnot(is.character(filter_str))
  
  fns <- list.files(in_dir, pattern = filter_str, full.names = TRUE)
  if (length(fns) < 1) {
    warning("fns is empty")
    NULL
  } else {
    purrr::map_chr(fns, load_xlsx_save_csv, out_dir)
  }
}

###################################################################
#' Loads all XLSX-formatted files in `in_dir` and saves CSV-formatted files
#' to `out_dir`
#'
#' @param in_dir A string indicating the input directory
#' @param out_dir A string indicating the output directory
load_xlsx_save_many_csvs_2 <- function(fns, out_dir) {
  require(purrr)
  stopifnot(is.character(fns))
  stopifnot(dir.exists(out_dir))
  
#  fns <- list.files(in_dir, pattern = filter_str, full.names = TRUE)
  if (length(fns) < 1) {
    warning("fns is empty")
    NULL
  } else {
    purrr::map_chr(fns, load_xlsx_save_csv, out_dir)
  }
}

###################################################################
#' Cleans set of screening/demographic data files and aggregates them into
#' a single data frame.
#'
#' @param fns A character vector of CSV filenames.
#' @return A single dataframe with the aggregated demographic data files.
clean_merge_demog <- function(fns) {
  stopifnot(is.character(fns))
  
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
  if (!is.data.frame(df_2)) {
    message("Failure to read file '", csv_fn, "'")
    return(NULL)
  }
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
  stopifnot(is.data.frame(df))
  
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
  stopifnot(is.data.frame(df))
  
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
  stopifnot(is.data.frame(df))

  df %>%
    ggplot(.) +
    aes(n_calls, fill = site_id) +
    geom_col() +
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
rename_home_xlsx <- function(fl, out_dir) {
  require(stringr)
  require(purrr)
#  stopifnot(is.character(fl))
  stopifnot(dir.exists(out_dir))
  
  # fl <- list.files(in_dir, full.names = TRUE)
  # fl_home_old <- unlist(fl[stringr::str_detect(fl, 'Home')])
  fl_home_old <- fl[stringr::str_detect(fl, 'Home')]
  fl_home_new <-
    purrr::map_chr(fl_home_old, make_standard_form_name)
  fl_home_new <- file.path(out_dir, fl_home_new)
  file.rename(fl_home_old, fl_home_new)
  fl_home_new
}

###################################################################
#' Helper function called by rename_home_xlsx() to create standard
#' file name.
#'
#' @param fn Old file name
#' @return New file name and path
make_standard_form_name <- function(fn) {
  stopifnot(is.character(fn))
  
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
#  file.path(this_dir, fn)
  fn
}

###################################################################
#' Helper function to extract the age group from a file name.
#'
#' @param form_name File (form) name.
#' @return Age group as a string
extract_age_group_from_name <- function(form_name) {
  stopifnot(is.character(form_name))
  
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
  stopifnot(is.character(form_name))

    stringr::str_detect(form_name, "[Bb]ilingual")
}

###################################################################
#' Helper function to detect whether a file/form contains data from
#' a Spanish-speaking family.
#'
#' @param form_name
#' @return Logical vector of names that contain the detected string.
form_is_spanish <- function(form_name) {
  stopifnot(is.character(form_name))

    stringr::str_detect(form_name, "[Ss]panish")
}

###################################################################
#' Helper function to determine the language spoken by the participant
#' family based on information in the file/form name.
#'
#' @param form_name
#' @return String with appropriate language indicator.
form_language <- function(form_name) {
  stopifnot(is.character(form_name))

  is_bilingual <- form_is_bilingual(form_name)
  is_spanish <- form_is_spanish(form_name)
  form_lang <- rep("english", length(form_name))
  form_lang[is_bilingual & is_spanish] <- "bilingual_spanish"
  form_lang[is_bilingual & !is_spanish] <- "bilingual_english"
  form_lang
}

###################################################################
split_non_mbcdi_csvs <- function(fl, out_dir) {
  require(purrr)
  stopifnot(is.character(fl))
  stopifnot(dir.exists(out_dir))
  
  purrr::map_chr(
    fl,
    open_split_save,
    csv_save_dir = out_dir,
    these_questions = 'non_mbcdi'
  )
  #list.files(out_dir, full.names = TRUE)
}

###################################################################
split_mbcdi_csvs <- function(fl, out_dir) {
  require(purrr)
  stopifnot(is.character(fl))
  stopifnot(dir.exists(out_dir))
  
  purrr::map_chr(
    fl,
    open_split_save,
    csv_save_dir = out_dir,
    these_questions = 'mbcdi'
  )
}

###################################################################
remove_identifiers_non_mbcdi <- function(fl, out_dir) {
  require(purrr)
  stopifnot(is.character(fl))
  stopifnot(dir.exists(out_dir))
  
  purrr::map_chr(fl,
    open_deidentify_save,
    csv_save_dir = out_dir,
    these_questions = 'non_mbcdi'
  )
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
                            vb = FALSE) {
  if (file.exists(fp)) {
    df <- readr::read_csv(fp, show_col_types = FALSE)
  } else {
    stop(paste0('Cannot read file `', fp, '`'))
  }
  stopifnot(is.character(csv_save_dir))
  stopifnot(dir.exists(csv_save_dir))
  stopifnot(is.logical(rename_cols))
  stopifnot(is.logical(vb))
  
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
  out_fn
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
    stopifnot(is.data.frame(df))
    stopifnot(is.logical(rename_cols))
    
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
    stopifnot(is.data.frame(df))
    stopifnot(is.character(fn))
    stopifnot(is.logical(rename_cols))
    
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
    stopifnot(is.data.frame(df))
    stopifnot(is.logical(rename_cols))
    
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
  stopifnot(is.data.frame(df))
  stopifnot(is.character(fn))
  stopifnot(is.logical(rename_cols))
  
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
                                 vb = FALSE) {
  require(tidyverse)
  stopifnot(is.character(fp))
  stopifnot(dir.exists(csv_save_dir))
  stopifnot(is.character(these_questions))
  stopifnot(is.logical(rename_cols))
  stopifnot(is.logical(vb))
  
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
      stringr::str_extract(fp, '[0-9]+'),
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
  out_fn
}

###################################################################
#' Detects presence of identifying information from column names
#' in a data frame then returns data frame without those columns.
#' 
#' @param df
#' @return Data frame with identifiers removed.
remove_identifiers <- function(df) {
  require(stringr)
  stopifnot(is.data.frame(df))
  
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

###################################################################
#' Takes a list of CSV data files and merges them.
#' NOTE: we use read.csv() to _avoid_ the problem of inferring
#' column types differently across the datasets.
make_aggregate_data_file <- function(fl) {
  require(purrr)
  stopifnot(is.character(fl))
  
  purrr::map_df(fl, read.csv, colClasses = 'character')
}

###################################################################
# Helper functions for cleaning non-MBDI data frames

remove_technology_use_scale <- function(df) {
  require(dplyr)
  stopifnot(is.data.frame(df))
  
  dplyr::select(df, -contains('technology_use_scale'))
}

remove_doctor_told_you <- function(df) {
  require(dplyr)
  stopifnot(is.data.frame(df))
  
  stopifnot(is.data.frame(df))
  dplyr::select(df, -contains('doctor_told_you'))
}

remove_databrary_fields <- function(df) {
  require(dplyr)
  stopifnot(is.data.frame(df))
  
  stopifnot(is.data.frame(df))
  dplyr::select(df, -contains('group_databrary'))
}

reconcile_typicalday <- function(df) {
  require(stringr)
  stopifnot(is.data.frame(df))

  names(df) <- stringr::str_replace_all(names(df), 'typicalday', 'typical_day')
  df
}

remove_permissive_locomotor_milestones_label <- function(df) {
  require(stringr)
  stopifnot(is.data.frame(df))
  
  old_names <- names(df)
  new_names <- old_names
  contains_locomotor <-
    stringr::str_detect(new_names, pattern = "locomotor_milestones.*health|division|rothbart|mediause|pets|typical|acknowledge")
  new_names[contains_locomotor] <-
    stringr::str_remove(new_names[contains_locomotor], "group_locomotor_milestones\\.")
  names(df) <- new_names
  df
}

remove_X_meta_cols <- function(df) {
  require(dplyr)
  stopifnot(is.data.frame(df))
  
  dplyr::select(df, -contains("X_"), -contains("meta.instanceID"))
}

remove_redundant_group_labels <- function(df) {
  require(stringr)
  stopifnot(is.data.frame(df))
  
  names(df) <- stringr::str_remove_all(names(df), 'group_homevisitquestionnaires\\.')
  names(df) <- stringr::str_remove_all(names(df), 'group_combinedquestionnaires\\.')
  names(df) <- stringr::str_remove_all(names(df), 'group_')
  df
}

change_CSLON_to_CSULB <- function(df) {
  require(dplyr)
  stopifnot(is.data.frame(df))
  
  dplyr::mutate(df, site_id = recode(site_id, "CSLON" = "CSULB"))
}

clean_dfs <- function(df) {
  stopifnot(is.data.frame(df))
  
  df %>%
    reconcile_typicalday() %>%
    remove_technology_use_scale() %>%
    remove_doctor_told_you() %>%
    remove_permissive_locomotor_milestones_label() %>%
    remove_databrary_fields() %>%
    remove_X_meta_cols() %>%
    remove_redundant_group_labels() %>%
    change_CSLON_to_CSULB()
}

###################################################################
#' Accessing Databrary
#'
check_databrary_login <- function(db_login_id = Sys.getenv("DATABRARY_LOGIN")) {
  require(databraryapi)
  stopifnot(is.character(db_login_id))

  databraryapi::login_db(db_login_id)
  
  # _targets.R assigns Sys.getenv("DATABRARY_LOGIN") to `db_login_id`
  # if (!file.exists('.databrary.RData')) {
  #   if (params$databrary_login == db_login_id) {
  #     stop('Cannot login to Databrary with login id: `',
  #          db_login_id,
  #          '`')
  #   } else {
  #     logged_in_db <- databraryapi::login_db(db_login_id)
  #   }
  #   if (!logged_in_db) {
  #     stop('Automatic log in failed. Please log in manually.')
  #   } else {
  #   logged_in_db = TRUE
  # }
  # logged_in_db
}

###################################################################
play_vols <- tibble::tibble(
  play_site_id = c(
    'PLAYProject_GEORG',
    'PLAYProject_CHOPH',
    'PLAYProject_CSULB',
    'PLAYProject_CUNYS',
    'PLAYProject_NYUNI',
    'PLAYProject_INDNA',
    'PLAYProject_OHIOS',
    'PLAYProject_PRINU',
    'PLAYProject_PURDU',
    'PLAYProject_STANF',
    'PLAYProject_UCRIV',
    'PLAYProject_UCSCR',
    'PLAYProject_UHOUS',
    'PLAYProject_VBLTU',
    'PLAYProject_VCOMU',
    'PLAYProject_UIOWA',
    'PLAYProject_BOSTU',
    'PLAYProject_CSUFL',
    'PLAYProject_UGEOR',
    'PLAYProject_UTAUS',
    'PLAYProject_RUTGU',
    'PLAYProject_UMIAM',
    'PLAYProject_UOREG'
  ),
  play_vol_id = c(
    954,
    1370,
    1376,
    1023,
    899,
    1400,
    1103,
    979,
    1363,
    1362,
    966,
    1066,
    1397,
    1391,
    982,
    1422,
    1008,
    1481,
    1515,
    1517,
    1546,
    996,
    1459
  ),
  site_name = c(
    "Georgetown University",
    "Children's Hospital of Philadelphia",
    "Cal State Long Beach",
    "CUNY Staten Is",
    "New York University",
    "Indiana University",
    "Ohio State University",
    "Princeton University",
    "Purdue University",
    "Stanford University",
    "UC Riverside",
    "UC Santa Cruz",
    "University of Houston",
    "Vanderbilt University",
    "Virginia Commonwealth University",
    "University of Iowa",
    "Boston University",
    "Cal State Fullerton",
    "University of Georgia",
    "University of Texas at Austin",
    "Rutgers University",
    "University of Miami",
    "University of Oregon"
  )
)

###################################################################
lookup_databrary_session <-
  function(this_site_id,
           s_number) {
    
    require(dplyr)
    require(databraryapi)
    require(stringr)
    
    if (!is.character(this_site_id)) {
      stop('`this_site_id` must be a character string.')
    }
    if (!is.numeric(as.numeric(s_number))) {
      stop('`s_number` must be a number.')
    }
    if (!file.exists('.databrary.RData')) {
      stop('Not logged-in to Databrary.')
    }
    
    this_volume <-
      dplyr::filter(play_vols, stringr::str_detect(play_site_id, this_site_id))
    if (dim(this_volume)[1] <= 0) {
      message('No volume found for site ', this_site_id)
      return(NULL)
    }
    
    vol_sessions <-
      databraryapi::list_sessions(as.numeric(this_volume$play_vol_id, vb = TRUE))
    
    if (!is.null(vol_sessions)) {
      s_number_zero_padded <- stringr::str_pad(s_number, 3, 'left', 0)
      df <- dplyr::filter(vol_sessions, stringr::str_detect(name, paste0(s_number_zero_padded, '$'))) 
      df <- dplyr::rename(df, session_name = name)
      df <- dplyr::mutate(df, release = recode(release, "1"="shared", "2"= "learning", "0"="private"))
      dplyr::select(df, -top)
    } else {
      message('Cannot access volume ', this_volume$play_vol_id)
      NULL
    }
  }

###################################################################
get_databrary_session_data <-  function(this_site_id,
                                        s_number, vb = FALSE) {
  require(dplyr)
  require(stringr)
  require(databraryapi)
  if (!is.character(this_site_id)) {
    stop('`this_site_id` must be a character string.')
  }
  if (!is.numeric(as.numeric(s_number))) {
    stop('`s_number` must be a number.')
  }
  if (!file.exists('.databrary.RData')) {
    stop('Not logged-in to Databrary.')
  }
  stopifnot(is.logical(vb))
  
  this_volume <-
    dplyr::filter(play_vols, stringr::str_detect(play_site_id, this_site_id))
  if (dim(this_volume)[1] <= 0) {
    message('No volume found for site ', this_site_id)
    return(NULL)
  }
  
  if (vb) message("Site: ", this_site_id, " | Session: ", subject_number)
  vol_sessions <-
    databraryapi::download_session_csv(as.numeric(this_volume$play_vol_id))
  
  if (!is.null(vol_sessions)) {
    s_number_zero_padded <- stringr::str_pad(s_number, 3, 'left', 0)
    df <- dplyr::filter(vol_sessions, stringr::str_detect(session_name, paste0(s_number_zero_padded, '$'))) 
    #df <- dplyr::rename(df, session_name = name)
    #df <- dplyr::mutate(df, release = recode(release, "1"="shared", "2"= "learning", "0"="private"))
    #dplyr::select(df, -context.state)
    df
  } else {
    message('Cannot access session data from volume ', this_volume$play_vol_id)
    NULL
  }
}

###################################################################
get_databrary_session_data_2 <-  function(row, df, vb = FALSE) {
  
  require(dplyr)
  require(stringr)
  require(databraryapi)
  stopifnot(is.numeric(row))
  stopifnot(row >= 1)
  stopifnot(is.data.frame(df))
  stopifnot(is.logical(vb))
  
  if (!file.exists('.databrary.RData')) {
    stop('Not logged-in to Databrary.')
  }
  
  this_row <- df[row,]
  
  this_volume <-
    dplyr::filter(play_vols, stringr::str_detect(play_site_id, this_row$site_id))
  # if (dim(this_volume)[1] <= 0) {
  #   message('No volume found for site ', this_site_id)
  #   return(NULL)
  # }
  
  if (vb) message("Site: ", this_row$site_id, " | Session: ", this_row$subject_number)
  vol_sessions <-
    databraryapi::download_session_csv(as.numeric(this_volume$play_vol_id))
  
  if (!is.null(vol_sessions)) {
    s_number_zero_padded <- stringr::str_pad(this_row$subject_number, 3, 'left', 0)
    df <- dplyr::filter(vol_sessions, stringr::str_detect(session_name, paste0(s_number_zero_padded, '$'))) 
    #df <- dplyr::rename(df, session_name = name)
    #df <- dplyr::mutate(df, release = recode(release, "1"="shared", "2"= "learning", "0"="private"))
    #dplyr::select(df, -context.state)
    df
  } else {
    message('Cannot access session data from volume ', this_volume$play_vol_id)
    NULL
  }
}

###################################################################
get_db_session_data_from_site <- function(this_site, vb = FALSE) {
  require(databraryapi)
  stopifnot(is.character(this_site))
  stopifnot(is.logical(vb))
  
  # if (!check_databrary_login()) {
  #   stop('Not logged-in to Databrary.')
  # }
  
  this_volume <-
    dplyr::filter(play_vols, stringr::str_detect(play_site_id, this_site))
  if (is.null(this_volume)) {
    if (vb) message("Problem retrieving PLAY site data from `play_vols`")
    return(NULL)
  }
  
  vol_sessions <-
    databraryapi::download_session_csv(as.numeric(this_volume$play_vol_id))
  
  if (!is.null(vol_sessions)) {
    vol_sessions
  } else {
    if (vb) message('Cannot access session data from volume: ', this_volume$play_vol_id)
    NULL
  }
}

###################################################################
summarize_sessions_by_site <- function(this_site, vb = FALSE) {
  require(dplyr)
  stopifnot(is.character(this_site))
  stopifnot(is.logical(vb))
  
  sessions_df <- get_db_session_data_from_site(this_site, vb = vb)
  if (is.null(sessions_df)) {
    if (vb) message("No sessions data in data frame.")
    return(NULL)
  }
  if (dim(sessions_df)[1] == 0) {
    if (vb) message("No sessions data in data frame.")
    return(NULL)
  }
  
  if ('group_name' %in% names(sessions_df)) {
    sessions_df %>%
      group_by(group_name) %>% 
      summarize(n_sessions = n())
  } else {
    if (vb) message("Problem retrieving session data from site: ", this_site)
    return(NULL)
  }
}

make_site_session_summary_multiple <- function(play_vols) {
  if (!databraryapi::login_db(Sys.getenv("DATABRARY_LOGIN"))) {
    message("Not authenticated to Databrary.")
    return(NULL)
  }
  
  purrr::map_df(play_vols$play_site_id, make_site_session_summary)
}

make_site_session_summary <- function(this_site, vb = FALSE) {
  require(dplyr)
  require(tidyr)
  require(databraryapi)
  stopifnot(is.character(this_site))
  stopifnot(is.logical(vb))
  
  site_df <- summarize_sessions_by_site(this_site, vb = vb)
  if (is.null(site_df)) {
    if (vb) message("No data retrieved from site: ", this_site)
    return(NULL)
  }
  
  site_info <- get_site_info(this_site, vb = vb)
  
  df <- dplyr::mutate(site_df, site_id = site_info$play_site_id,
                      site_vol_id = site_info$play_vol_id,
                      site_name = site_info$site_name)
  
  df %>% 
    tidyr::pivot_wider(names_from = group_name, values_from = n_sessions)
}

###################################################################
get_save_databrary_session_csv <- function(this_site = "NYUNI", 
                                           csv_dir = "data/csv/databrary_vols", 
                                           vb = FALSE) {
  require(databraryapi)
  require(readr)
  stopifnot(is.character(this_site))
  stopifnot(is.character(csv_dir), dir.exists(csv_dir))
  stopifnot(is.logical(vb))
  
  if (vb) message("Retrieving sessions from '", this_site, "'.")
  df <- get_db_session_data_from_site(this_site, vb = vb)
  
  if ((!is.data.frame(df)) || is.null(df)) {
    if (vb) message(" No data retrieved from site '", this_site, "'. No file saved.")
    NULL
  } else {
    fn = file.path(csv_dir, paste0(this_site, ".csv"))
    if (vb) message(" Saved data frame to '", fn, "'.")
    readr::write_csv(df, fn)
  }
}

###################################################################
get_save_all_databrary_session_csvs <- function(vb = FALSE) {
  require(databraryapi)
  
  if (databraryapi::login_db(Sys.getenv("DATABRARY_LOGIN"))) {
    purrr::map(play_vols$play_site_id, get_save_databrary_session_csv, vb = TRUE)
    #get_save_databrary_session_csv()
  }
    if (vb) message("Not logged in to Databrary.")
    NULL
}

###################################################################
get_site_info <- function(this_site, vb = FALSE) {
  require(dplyr)
  require(tidyr)
  stopifnot(is.character(this_site))
  stopifnot(is.logical(vb))
  
  dplyr::filter(play_vols, stringr::str_detect(play_site_id, this_site))
}


###################################################################
make_databrary_url_from_session <- function(session_df) {
  stopifnot(is.data.frame(session_df))
  
  paste0(
    "https://nyu.databrary.org/volume/",
    session_df$vol_id,
    "/slot/",
    session_df$session_id,
    '/-'
  )
}

###################################################################
add_databrary_url_to_session_df <- function(session_df) {
  require(dplyr)
  stopifnot(is.data.frame(session_df))

  dplyr::mutate(session_df, 
                session_url = purrr::map_chr(1:dim(session_df)[1], 
                                             make_databrary_url_from_session_list, df = session_df))
}

###################################################################
make_databrary_url_from_session_list <- function(row_index = 1, df) {
  stopifnot(is.numeric(row_index))
  stopifnot(is.data.frame(df))
  
  this_session <- df[row_index,]
  make_databrary_url_from_session(this_session)
}

###################################################################
generate_databrary_guid <- function(session_df) {
  require(stringr)
  stopifnot(is.data.frame(session_df))
  
  paste0(session_df$session_id, stringr::str_pad(session_df$vol_id, 5, 'left', 0))
}

###################################################################
generate_databrary_url <- function(session_df) {
  stopifnot(is.data.frame(session_df))
  paste0(
    "https://nyu.databrary.org/volume/",
    session_df$vol_id,
    "/slot/",
    session_df$session_id,
    '/-'
  )
}

###################################################################
add_play_session_name <- function(df) {
  require(dplyr)
  stopifnot(is.data.frame(df))
  
  # Input is data frame/tibble from PLAY_non_mbcdi.csv
  dplyr::mutate(df, session_name = paste0('PLAY_', site_id, '_', 
                                          stringr::str_pad(subject_number, 3, 'left', 0)))
}

###################################################################
add_databrary_info_to_home_visit_df <- function(df, vb = FALSE) {
  require(databraryapi)
  require(purrr)
  require(dplyr)
  stopifnot(is.data.frame(df))
  stopifnot(is.logical(vb))
  
  # Check Databrary login
  if (vb) "Authenticating to Databrary using stored credentials."
  auth_status <- databraryapi::login_db(Sys.getenv("DATABRARY_LOGIN"))
  stopifnot("Not logged in to Databrary." = auth_status)
  
  # Generate KoBo and add full Databrary-compatible PLAY session name
  if (vb) message('Creating formatted session name.')
  # play_kobo <- readr::read_csv(fn, show_col_types = FALSE)
  play_kobo <- add_play_session_name(df)
  
  # Gather session info from Databrary and add GUID and URL
  if (vb) message('Getting session info from Databrary.')
  
  #play_db_sessions <- purrr::map2_df(play_kobo$site_id, play_kobo$subject_number, lookup_databrary_session)
  #play_db_sessions <- purrr::map2_df(play_kobo$site_id, play_kobo$subject_number, get_databrary_session_data)
  n_rows <- dim(play_kobo)[1]
  play_db_sessions <- purrr::map_df(1:n_rows, get_databrary_session_data_2, play_kobo, vb = vb)
  
  if (vb) message('Modifying Databrary session info.')
  
  play_db_sessions <- dplyr::mutate(play_db_sessions, databrary_guid = generate_databrary_guid(play_db_sessions),
                                    databrary_url = generate_databrary_url(play_db_sessions))
  
  # Join databases with common session_id's
  dplyr::inner_join(play_kobo, play_db_sessions)
}

###################################################################
test_kb_db <- function(rg, df) {
  require(purrr)
  stopifnot(is.numeric(rg))
  stopifnot(is.data.frame(df))
  
  purrr::map_df(rg, get_databrary_session_data_2, df, vb = TRUE)
}

###################################################################
select_kbt_for_site <- function(this_site = "NYUNI", 
                                df = targets::tar_load(home_visit_df),
                                vb = FALSE) {
  require(dplyr)
  stopifnot(is.character(this_site))
  stopifnot(is.data.frame(df), !is.null(df))
  stopifnot(is.logical(vb))
  
  df %>%
    dplyr::filter(., stringr::str_detect(site_id, this_site))
}
