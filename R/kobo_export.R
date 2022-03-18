###################################################################
#' Installs packages required for use of the KoboToolbox commands.
#'
install_kobo_packages <- function() {
  if (!(require(koboloadeR))) {
    if (!require(devtools)) {
      install.packages('devtools')
    }
    devtools::install_github("unhcr/koboloadeR")
  }
  
  if (!require(getPass)) {
    install.packages('getPass')
  }
  
  if (!require(httr)) {
    install.packages('httr')
  }
  
  if (!require(readr)) {
    install.packages('readr')
  }
  
  if (!require(jsonlite)) {
    install.packages("jsonlite")
  }
  
  if (!require(knitr)) {
    install.packages("knitr")
  }
  
  if (!require(magrittr)) {
    install.packages("magrittr")
  }
  
  if (!require(readxl)) {
    install.packages("readxl")
  }
}

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

find_xlsx_file <- function(form_year = '2021',
                           age_group = '12',
                           lang_grp = 'English',
                           xlsx_dir = 'tmp') {
  search_pattern <-
    paste0(age_group, '_', lang_grp, '.*', form_year, '.*\\.xlsx$')
  list.files(xlsx_dir, search_pattern, full.names = TRUE)
}

import_play_xlsx <- function(form_year = '2021',
                             age_group = '12',
                             lang_grp = 'English',
                             xlsx_dir = 'tmp') {
  this_file <-
    find_xlsx_file(form_year, age_group, lang_grp, xlsx_dir)
  
  if (!is.null(this_file)) {
    readxl::read_excel(this_file)
  } else {
    message('File not found: ', file.path(this_file))
  }
}

import_xlsx_save_raw <- function(form_year = '2021',
                                 age_grp = '12',
                                 lang_grp = 'English',
                                 xlsx_dir = 'tmp',
                                 csv_dir = 'tmp',
                                 vb = TRUE) {
  if (vb)
    message('Importing data for ',
            paste0(form_year, '_', age_grp, '_', lang_grp))
  
  df <- import_play_xlsx(form_year, age_grp, lang_grp, xlsx_dir)
  
  if (!is.null(df)) {
    fp <- file.path(csv_dir,
                    paste0(form_year, '_raw_',
                           age_grp, '_',
                           lang_grp, '.csv'))
    readr::write_csv(df, fp)
    if (vb)
      message('Wrote: `', fp, '`')
  } else {
    message('Error in importing data.')
  }
}

import_xlsx_clean_save_non_mbcdi <- function(form_year = '2021',
                                             age_grp = '12',
                                             lang_grp = 'English',
                                             xlsx_dir = 'tmp',
                                             csv_dir = 'tmp',
                                             vb = TRUE) {
  if (vb)
    message('Importing data for ',
            paste0(form_year, '_', age_grp, '_', lang_grp))
  df <- import_play_xlsx(form_year, age_grp, lang_grp, xlsx_dir)
  
  if (!is.null(df)) {
    extract_save_non_mbcdi(df, file.path(
      csv_dir,
      paste0(form_year, '_non_mbcdi_',
             age_grp, '_',
             lang_grp, '.csv')
    ))
  } else {
    message('Error in importing data.')
  }
}

open_trim_csv <- function(fn, col_index = 246) {
  df <- read.csv(fn, as.is = TRUE)
  
  if (dim(df)[2] > 246) {
    df[, 1:246]
  } else {
    df
  }
  
  if (dim(df)[1] == 0) {
    message('No data in ', fn)
    NULL
  } else {
    df
  }
}

make_non_mbcdi_csv <-
  function(update_2020 = FALSE,
           out_fn = 'tmp/PLAY_non_mbcdi_all.csv') {
    mapply(import_xlsx_clean_save_non_mbcdi,
           rep('2021', 9),
           rep(c('12', '18', '24'), 3),
           rep(
             c('English', 'Bilingual_English', 'Bilingual_Spanish'),
             each = 3
           ))
    
    if (update_2020) {
      mapply(import_xlsx_clean_save_non_mbcdi,
             rep('2020', 9),
             rep(c('12', '18', '24'), 3),
             rep(
               c('English', 'Bilingual_English', 'Bilingual_Spanish'),
               each = 3
             ))
    }
    
    f_all <-
      list.files('tmp', '202[01]_non_mbcdi_[12]',
                 full.names = TRUE)
    
    mmm <- purrr::map_df(f_all, open_trim_csv)
    readr::write_csv(mmm, out_fn)
    
    cleanup_var_names(out_fn)
    # xfun::gsub_file('tmp/PLAY_non_mbcdi_all.csv',
    #                 'group_combinedquestionnaires.',
    #                 '')
    # xfun::gsub_file('tmp/PLAY_non_mbcdi_all.csv',
    #                 'group_homevisitquestionnaires.',
    #                 '')
    # xfun::gsub_file('tmp/PLAY_non_mbcdi_all.csv',
    #                 'group_', '')
  }

cleanup_var_names <- function(fn) {
  xfun::gsub_file(fn,
                  'group_combinedquestionnaires.',
                  '')
  xfun::gsub_file(fn,
                  'group_homevisitquestionnaires.',
                  '')
  xfun::gsub_file(fn,
                  'group_', '')
}

extract_save_mcdi <- function(df, fn, rename_cols = FALSE) {
  play_id_col <-
    (1:length(names(df)))[stringr::str_detect(names(df), 'participant_id')]
  
  mcdi_qs <- stringr::str_detect(names(df), 'mcdi')
  mcdi_cols <- (1:length(names(df)))[mcdi_qs]
  
  mcdi <- df %>%
    dplyr::select(., all_of(play_id_col), all_of(mcdi_cols))
  
  if (rename_cols) {
    mcdi <- dplyr::rename_with(mcdi, basename)
  }
  
  readr::write_csv(mcdi, fn)
  message('Saved ', fn)
}

extract_non_mbcdi <-
  function(df,
           rename_cols = FALSE,
           remove_identifying_data = TRUE) {
    if (remove_identifying_data)
      df <- remove_identifiers(df)
    
    play_id_col <-
      (1:length(names(df)))[stringr::str_detect(names(df), 'participant_id')]
    
    # Select non-mcdi cols
    mcdi_qs <- stringr::str_detect(names(df), 'mcdi')
    non_mcdi_qs <- !(mcdi_qs)
    non_mcdi_cols <- (1:length(names(df)))[non_mcdi_qs]
    
    non_mcdi <- df %>%
      dplyr::select(., all_of(play_id_col), all_of(non_mcdi_cols))
    
    if (rename_cols) {
      non_mcdi <- dplyr::rename_with(non_mcdi, basename)
    }
    
    non_mcdi
  }

extract_save_non_mbcdi <-
  function(df,
           fn,
           rename_cols = FALSE,
           remove_identifying_data = TRUE) {
    non_mcdi <-
      extract_non_mbcdi(df, rename_cols, remove_identifying_data)
    readr::write_csv(non_mcdi, fn)
    message('Saved ', fn)
  }

import_raw_extract_split_save <- function(form_year = '2021',
                                          age_grp = '12',
                                          lang_grp = 'English',
                                          csv_input_dir = 'tmp',
                                          csv_save_dir = 'tmp',
                                          these_questions = 'non_mbcdi',
                                          rename_cols = FALSE,
                                          remove_identifying_data = FALSE,
                                          vb = TRUE) {
  stopifnot(form_year %in% c('2020', '2021'))
  stopifnot(age_grp %in% c('12', '18', '24'))
  stopifnot(lang_grp %in% c('English', 'Bilingual_English', 'Bilingual_Spanish'))
  
  require(tidyverse)
  
  if (vb)
    message('Importing data for ',
            paste0(form_year, '_', age_grp, '_', lang_grp))
  
  fp <- file.path(csv_input_dir,
                  paste0(form_year, '_raw_',
                         age_grp, '_',
                         lang_grp, '.csv'))
  
  if (file.exists(fp)) {
    df <- readr::read_csv(fp, show_col_types = FALSE)
  } else {
    stop(paste0('Cannot read file `', fp, '`'))
  }
  
  out_fn <- file.path(
    csv_save_dir,
    paste0(
      form_year,
      '_',
      these_questions,
      '_',
      age_grp,
      '_',
      tolower(lang_grp),
      '.csv'
    )
  )
  
  if (!is.null(df)) {
    if (these_questions == 'non_mbcdi') {
      extract_save_non_mbcdi(df, out_fn, rename_cols, remove_identifying_data)
    } else {
      extract_save_mcdi (df, out_fn, rename_cols)
    }
  } else {
    message('Error in exporting data to `', out_fn, '`')
  }
}

import_non_mbcdi_remove_ids <- function(form_year = '2021',
                                        age_grp = '12',
                                        lang_grp = 'English',
                                        csv_input_dir = 'tmp',
                                        csv_save_dir = 'tmp',
                                        these_questions = 'non_mbcdi',
                                        rename_cols = FALSE,
                                        vb = FALSE) {
  stopifnot(form_year %in% c('2020', '2021'))
  stopifnot(age_grp %in% c('12', '18', '24'))
  stopifnot(lang_grp %in% c('English', 'Bilingual_English', 'Bilingual_Spanish'))
  
  require(tidyverse)
  
  if (vb)
    message('Importing data for ',
            paste0(form_year, '_', age_grp, '_', lang_grp))
  
  fp <- file.path(csv_input_dir,
                  paste0(
                    form_year,
                    '_non_mbcdi_',
                    age_grp,
                    '_',
                    tolower(lang_grp),
                    '.csv'
                  ))
  
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
      form_year,
      '_',
      these_questions,
      '_',
      age_grp,
      '_',
      tolower(lang_grp),
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
  contains_day <- stringr::str_detect(names(df), 'day[12]?')
  
  identifiable_data <- contains_name | contains_address |
    contains_phone |
    contains_email | contains_birthdate | contains_first |
    contains_last |
    contains_city | contains_year | contains_month | contains_day
  
  identifiable_cols <- (1:length(names(df)))[identifiable_data]
  
  df_deidentified <- df %>%
    dplyr::select(., -all_of(identifiable_cols))
  
  df_deidentified
}

bulk_download_kobo_forms <- function(df,
                                     year,
                                     update_form = TRUE,
                                     download_xlsx = TRUE,
                                     download_dir = 'tmp/xlsx') {
  stopifnot(is.data.frame(df))
  stopifnot(is.logical(update_form))
  stopifnot(is.logical(download_xlsx))
  stopifnot(is.character(download_dir))
  
  if (update_form) {
    kb_home <- df %>%
      dplyr::filter(., stringr::str_detect(title, year))
    
    if (!dir.exists(download_dir)) {
      message(paste0(download_dir, ' does not exist. Creating.'))
      dir.create(download_dir)
    }
    
    if (download_xlsx) {
      purrr::map(
        1:dim(kb_home)[1],
        retrieve_save_xls_export,
        kb_df = kb_home,
        save_dir = download_dir
      )
      message(paste0(
        dim(kb_home)[1],
        ' forms from ',
        year,
        ' downloaded to `',
        download_dir,
        '`'
      ))
    } else {
      message(paste0(
        '`download_xlsx` is  ',
        download_xlsx,
        ' : No data downloaded'
      ))
    }
  } else {
    message(paste0('No data from ', year, ' downloaded'))
  }
}
