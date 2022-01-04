###################################################################
#' Installs packages required for use of the KoboToolbox commands.
#' 
install_kobo_packages <- function() {
  if(!(require(koboloadeR))) {
    if(!require(devtools)) {
      install.packages('devtools')
    }
    devtools::install_github("unhcr/koboloadeR")
  }
  
  if(!require(getPass)) {
    install.packages('getPass')
  }
  
  if(!require(httr)) {
    install.packages('httr')
  }
  
  if(!require(readr)) {
    install.packages('readr')
  }
  
  if(!require(jsonlite)) {
    install.packages("jsonlite")
  }
  
  if(!require(knitr)) {
    install.packages("knitr")
  }
  
  if(!require(magrittr)) {
    install.packages("magrittr")
  }
  
  if(!require(readxl)) {
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
list_kobo_data <- function(URL = "https://kc.kobotoolbox.org/api/v1/data",
                           return_df = TRUE) {
  
  if (!is.character(URL)) {
    stop("`URL` must be a character string.")
  }
  
  require(httr)
  
  kobo_api_key <- Sys.getenv("KOBO_API_KEY")
  if (!is.character(kobo_api_key)) {
    stop('No KoBoToolbox API key stored in ~/.Renviron.')
  }
  
  config_params <- httr::add_headers(Authorization = paste0('Token ', kobo_api_key))
  
  r <- httr::GET(URL, config = config_params)
  if (httr::status_code(r) == 200) {
    c <- httr::content(r, as = 'text', encoding = 'utf8')
    if (return_df) {
      jsonlite::fromJSON(c)      
    } else { # JSON
      c
    }
  } else {
    message('HTTP call to ', URL, ' failed with status `', httr::status_code(r), '`.')
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
retrieve_save_xls_export <- function(form_index = 13, kb_df = list_kobo_data(), save_dir = 'tmp') {
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
    form_name <- create_cleaned_augmented_form_name(form_index, kb_df)
    # Must export as .xlsx even though API queries for .xls
    file_name <- file.path(save_dir, paste0(form_name, '.xlsx'))
    
    require(httr)
    
    kobo_api_key <- Sys.getenv("KOBO_API_KEY")
    if (!is.character(kobo_api_key)) {
      stop('No KoBoToolbox API key stored in ~/.Renviron.')
    }
    
    require(httr)
    
    config_params <- httr::add_headers(Authorization = paste0('Token ', kobo_api_key))
    r <- httr::GET(form_URL, config = config_params)
    if (httr::status_code(r) == 200) {
      c <- httr::content(r, as = 'raw')
      writeBin(c, file_name)
      message('Saved `', file_name, '`.')
      file_name
    } else {
      message('HTTP call to ', form_URL, ' failed with status `', httr::status_code(r), '`.')
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
extract_kb_form_name <- function(form_index = 13, kb_df = list_kobo_data()) {
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
  
  fn <- stringr::str_replace_all(kb_form_name, pattern = "[ ]+", "_")
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
extract_kb_form_id <- function(form_index = 13, kb_df = list_kobo_data()) {
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

create_cleaned_augmented_form_name <- function(form_index = 13, kb_df = list_kobo_data()) {
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
    paste0(extract_kb_form_id(form_index, kb_df), "_", 
           clean_kobo_form_name(extract_kb_form_name(form_index, kb_df)))
  }
}