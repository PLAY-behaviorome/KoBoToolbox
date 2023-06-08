#-------------------------------------------------------------------------------
# kobo.R
#
# A set of functions used to access files using the KoBoToolbox API.

#-------------------------------------------------------------------------------
#' Lists the datasets available on KoBoToolbox using the PLAY credentials.
#'
#' @param URL A string that is the API call to extract the data. It defaults
#' to 'https://kc.kobotoolbox.org/api/v1/data'
#' @param return_df A logical value indicating whether to return a data frame
#' (the default) or JSON.
#' @returns A data.frame with the 'description', 'title', 'url', and other
#' information for the deployed forms on PLAY.
list_data <-
  function(URL = "https://kc.kobotoolbox.org/api/v1/data",
           return_df = TRUE) {
    stopifnot(is.character(URL))
    stopifnot(is.logical(return_df))
    
    box::use(httr)
    
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

#-------------------------------------------------------------------------------
#' Lists the datasets available on KoBoToolbox using the 
#' PLAY KoBoToolbox credentials and filters them based on a user-defined string
#'
#' @param filter_str A string indicating what the form name must contain.
#' @returns A data.frame with the filtered KoBoToolbox forms.
#' @export
list_data_filtered <- function(filter_str = '[Dd]emographic') {
  stopifnot(is.character(filter_str))

  box::use(dplyr[filter])
  box::use(stringr[str_detect])
  
  kb_df <- list_data()
  
  dplyr::filter(kb_df,
                stringr::str_detect(title, filter_str))
}

#-------------------------------------------------------------------------------
#' Retrieves demographic/screening or home visit forms from KoBoToolbox and
#' saves an XLSX file in a local directory.
#'
#' @param df A data frame of the selected forms from the KoBoToolbox API.
#' Default is to call list_kobo_data().
#' @param save_dir A character string indicating the directory to save
#' the downloaded files. Default is 'tmp'.
#' @returns Files retrieved and saved by call to retrieve_save_xlsx()
#' @export
retrieve_save_many_xlsx <- function(df = list_data(), save_dir = 'tmp') {
  stopifnot(is.data.frame(df))
  stopifnot(dim(df)[1] > 0)
  stopifnot(is.character(save_dir))
  stopifnot(dir.exists(save_dir))
  
  box::use(purrr[map_chr])
  
  n_files <- dim(df)[1]
  message("Retrieving ", n_files, " files from KoBoToolbox site.")
  purrr::map_chr(1:n_files,
                 retrieve_save_xlsx,
                 kb_df = df,
                 save_dir = save_dir, .progress = "KoBo files:")
}

#-------------------------------------------------------------------------------
#' Retrieves an XLS formatted export file from KoBoToolbox and
#' saves it to a local directory.
#'
#' @param form_index An integer value indicating which row (KoBoToolbox form)
#' should be retrieved and saved. The default value for testing purposes is 13.
#' @param kb_df A data frame with the available KoBoToolbox data, typically
#' output of list_kobo_data(). The default is to call list_kobo_data().
#' @param save_dir A character string indicating the directory to save the
#' exported file. The default is 'tmp'.
#' @returns The name of the saved .xlsx file.
retrieve_save_xlsx <-
  function(form_index = 13,
           kb_df = list_data(),
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
      
      box::use(httr)
      
      # Append KoBo form id to cleaned file name to avoid duplicates
      form_name <-
        create_cleaned_augmented_form_name(form_index, kb_df)
      
      # Must export as .xlsx even though API queries for .xls
      file_name <- file.path(save_dir, paste0(form_name, '.xlsx'))
      
      kobo_api_key <- Sys.getenv("KOBO_API_KEY")
      if (!is.character(kobo_api_key)) {
        stop('No KoBoToolbox API key stored in ~/.Renviron.')
      }
      
      config_params <-
        httr::add_headers(Authorization = paste0('Token ', kobo_api_key))
      r <- httr::GET(form_URL, config = config_params)
      if (httr::status_code(r) == 200) {
        c <- httr::content(r, as = 'raw')
        writeBin(c, file_name)
        # message('Saved `', file_name, '`')
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

#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
#' Remove spaces and parentheses from a KoBoToolbox form name.
#'
#' @param kb_form_name A form name.
#' @return A string containing the reformatted filename.
clean_kobo_form_name <- function(kb_form_name) {
  if (!is.character(kb_form_name)) {
    stop('`kb_form_name` must be a character string')
  }
  
  box::use(stringr[str_replace_all])
  
  fn <-
    stringr::str_replace_all(kb_form_name, pattern = "[ ]+", "_")
  stringr::str_replace_all(fn, pattern = '[\\(,\\)]', "")
}


#-------------------------------------------------------------------------------
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
           kb_df = list_data()) {
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

#-------------------------------------------------------------------------------
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


#-------------------------------------------------------------------------------
#' Check stored KoBoToolbox credentials.
#' 
#' @param api_key_name Name of API key variable; Default is 'KOBO_API_KEY'.
#' @returns TRUE if credentials can be verified.
verify_kbt_credentials = function(api_key_name = "KOBO_API_KEY") {
  stopifnot(is.character(api_key_name))
  kobo_api_key <- Sys.getenv(api_key_name)
  if (kobo_api_key == '') {
    message("'", api_key_name, "' not found in ~/.Renviron. Please add via `usethis::edit_r_environ()`.")
    FALSE
  } else {
    TRUE
  }
}