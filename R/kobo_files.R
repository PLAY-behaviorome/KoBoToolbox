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
#' Lists the datasets available using the PLAY credentials.
#' 
#' In typical uses, this only needs to be run once on a given machine with `update_csv = TRUE`.
#' 
#' @param update_csv A logical value that controls whether a new CSV is written to the 'hidden' directory.
#' It defaults to FALSE.
#' @param forms_dir A string containing the path to the 'hidden' directory.
#' @param forms_fn A string that generates the default file name for the CSV.
#'
#' @return A data.frame with the 'description', 'title', 'url', and other information for the deployed forms on PLAY.
list_kobo_datasets <- function(update_csv = FALSE,
                               forms_dir = file.path('csv', '.analysis'),
                               forms_fn = 'kobo_forms.csv') {
  
  # Validate input parameters
  if (!is.logical(update_csv)) {
    message('`update_csv` must be a logical value. Coercing to FALSE.')
    update_csv <- FALSE
  }
  if (!is.character(forms_dir)) {
    stop('`forms_dir` must be a character string.')
  }
  if (!is.character(forms_fn)) {
    stop('`forms_fn` must be a character string.')
  }
  
  require(koboloadeR)
  
  kobo_login <- function() {
    require(getPass)
    
    kb_user_name <- getPass::getPass("KoBo user name: ")
    kb_pw <- getPass::getPass("KoBo password: ")
    
    sprintf('%s:%s', kb_user_name, kb_pw)
  }
  
  df <- koboloadeR::kobo_datasets(user = kobo_login())
  if (is.null(df)) {
    message('No data found for these KoBoToolbox credentials.')
    NULL
  } else {
    if (update_csv) {
      if (dir.exists(forms_dir)) {
        readr::write_csv(df, file.path(forms_dir, forms_fn))
        message('Saved `', file.path(forms_dir, forms_fn), '`.')
      } else {
        stop('Directory not found: `', forms_dir, '`.')
      }
    } 
    df
  }
}


###################################################################
#' Loads a saved CSV of the PLAY KoBoToolbox forms.
#' 
#' This is stored in a hidden directory to reduce errors.
#' 
#' @param form_fn Optional. A string with the full path to the 'kobo_forms.csv' file.
#'
#' @return A data.frame with the 'description', 'title', 'url', and other information for the deployed forms on PLAY.
load_saved_kobo_forms_df <- function(forms_fn = file.path('csv', '.analysis', 'kobo_forms.csv')) {
  
  if (!is.character(forms_fn)) {
    stop('`forms_fn` must be a character string.')
  }
  if (!file.exists(forms_fn)) {
    stop('File not found: `', forms_fn, '`.')
  }
  readr::read_csv(forms_fn)
}

###################################################################
#' Downloads a CSV data file from the KoBoToolbox server given a URL.
#' 
#' @param url A URL to the data file on the KoBoToolbox server.
#' 
#' @return A data.frame from the URL.
kobo_form_url_to_df <- function(url) {
  if (!is.character(url)) {
    stop('`url` must be a character string.')
  }
  
  require(httr)
  
  kobo_api_key <- Sys.getenv("KOBO_API_KEY")
  if (!is.character(kobo_api_key)) {
    stop('No KoBoToolbox API key stored in .Renviron.')
  }
  
  config_params <- httr::add_headers(Authorization = paste0('Token ', kobo_api_key),
                                     Accept = 'text/csv')
  
  r <- httr::GET(url, config = config_params)
  if (httr::status_code(r) == 200) {
    c <- httr::content(r, as = 'text', encoding = 'utf8')
    readr::read_csv(c)
  } else {
    message('HTTP call failed with status `', httr::status_code(r), '`.')
    NULL
  }
}

###################################################################
#' Saves a specified form's response data as a CSV. Set-up for using purrr() with a list of indices.
#' 
#' @param form_index An integer representing a row in the 'kobo_df' data.frame.
#' @param kobo_df A data.frame of valid forms for the PLAY Project.
#' 
#' @return NULL if there is no data for a given form.
save_kobo_forms_csvs <- function(form_index = 1, kobo_df = load_saved_kobo_forms_df()) {
  if (!is.numeric(form_index)) {
    stop('`form_index` must be a number.')
  }
  if (form_index <= 0) {
    stop('`form_index` must be > 0.')
  }
  if (form_index > dim(kobo_df[1])) {
    stop('`form_index` must not exceed the number of rows in `kobo_df`.')
  }
  
  this_form = kobo_df[form_index,]
  this_form_df <- kobo_form_url_to_df(this_form$url)
  if (is.null(this_form_df)) {
    message('No data for form: `', this_form$title, '`.')
    NULL
  } else {
    save_kobo_form_df_to_csv(this_form_df, form_fn = paste0(this_form$title, '.csv'))    
  }
}

###################################################################
#' Saves a single PLAY project form data.frame as a CSV.
#' 
#' @param df A data.frame with information about a PLAY project form.
#' @param csv_dir A string indicating what directory the form-level CSV should be saved.
#' @param form_fn A string indicating the file name to use for the CSV. 
#' In typical uses, this is derived from the 'title' field of the data.frame 'df'.
#' 
save_kobo_form_df_to_csv <- function(df, 
                                     csv_dir = 'csv/raw',
                                     form_fn = 'test.csv') {
  if (is.null(df)) {
    stop('`df` must not be NULL.')
  }
  if (!is.character(csv_dir)) {
    stop('`csv_dir` must be a character string.')
  }
  if (!dir.exists(csv_dir)) {
    stop('Directory not found: `', csv_dir, '`.')
  }
  if (!is.character(form_fn)) {
    stop('`form_fn` must be a character string.')
  }
  if (file.exists(file.path(csv_dir, form_fn))) {
    message('File exists: `', form_fn, '`. Overwriting.')
  }
  
  readr::write_csv(df, file.path(csv_dir, form_fn))
  message('Saved `', file.path(csv_dir, form_fn), '`.')
}

###################################################################
get_exports_params_from_kobo <- function(return_df = TRUE) {

  if(!is.logical(return_df)) {
    stop('`return_df` must be logical value')
  }
  
  require(httr)
  
  kobo_api_key <- Sys.getenv("KOBO_API_KEY")
  if (!is.character(kobo_api_key)) {
    stop('No KoBoToolbox API key stored in .Renviron.')
  }
  
  config_params <- httr::add_headers(Authorization = paste0('Token ', kobo_api_key),
                                     Accept = 'application/json')
  
  url <- paste0('https://kf.kobotoolbox.org/exports/?format=json')
  r <- httr::GET(url, config = config_params)
  if (httr::status_code(r) == 200) {
    c <- httr::content(r, as = 'text', encoding = 'utf8')
    j <- jsonlite::fromJSON(c)
    if (return_df) {
      j$results      
    } else { # JSON
      j 
    }
    # If return_xls_url, extract URL from JSON, otherwise, return JSON as list
    } else {
    message('HTTP call failed with status `', httr::status_code(r), '`.')
    NULL
  }
}

###################################################################
get_xls_url_for_play_form <- function(id_string = 'esFKTZc924Hu5ebtfE5uSJ',
                                      return_xls_url = TRUE) {
  if (!is.character(id_string)) {
    stop('`id_string` must be a character string.')
  }
  if (!is.logical(return_xls_url)) {
    stop('`return_xls_url` must be a logical value.')
  }
  
  require(httr)
  
  kobo_api_key <- Sys.getenv("KOBO_API_KEY")
  if (!is.character(kobo_api_key)) {
    stop('No KoBoToolbox API key stored in .Renviron.')
  }
  
  config_params <- httr::add_headers(Authorization = paste0('Token ', kobo_api_key),
                                     Accept = 'application/json')
  
  url <- paste0('https://kf.kobotoolbox.org/exports/', id_string, '/?format=json')
  r <- httr::GET(url, config = config_params)
  if (httr::status_code(r) == 200) {
    c <- httr::content(r, as = 'text', encoding = 'utf8')
    j <- jsonlite::fromJSON(c)
    # If return_xls_url, extract URL from JSON, otherwise, return JSON as list
    if (return_xls_url) {
      j$result
    } else {
      j
    }
  } else {
    message('HTTP call failed with status `', httr::status_code(r), '`.')
    NULL
  }
}

###################################################################
#' Retrieves an XLSX format spreadsheet using the KoBoToolbox API 
#' and credentials stored in .Renviron. The file is then stored as 
#' an Excel spreadsheet in the default `xlsx/`
#' file directory.  
#'
#' @param url The KoBoToolbox URL for the form to export. The `url` field
#' comes from the output returned by `get_exports_params_from_kobo()`.
#' @param xlsx_fn The name of the output file that is saved. Default
#' is to extract the file name from the URL and save it in xlsx/<filename>.
#'
save_xlsx_export_from_url <- function(url, 
                                      xlsx_fn = file.path('xlsx', basename(url))) {
  if (!is.character(url)) {
    stop('`url` must be a character string.')
  }
  if (!is.character(xlsx_fn)) {
    stop('`xlsx_fn` must be a character string.')
  }
  
  require(httr)
  
  kobo_api_key <- Sys.getenv("KOBO_API_KEY")
  if (!is.character(kobo_api_key)) {
    stop('No KoBoToolbox API key stored in .Renviron.')
  }
  
  config_params <- httr::add_headers(Authorization = paste0('Token ', kobo_api_key),
                                     Accept = '.xlsx')
  
  r <- httr::GET(url, config = config_params)
  if (httr::status_code(r) == 200) {
    c <- httr::content(r, as = 'raw')
    writeBin(c, xlsx_fn)
    message('Saved `', xlsx_fn, '`.')
  } else {
    message('HTTP call to ', url, ' failed with status `', httr::status_code(r), '`.')
    NULL
  }  
}

###################################################################
save_kobo_forms_xlsx <- function(export_index, kobo_df) {
  this_export = kobo_df[export_index,]
  if (is.null(this_export)) {
    message('No data for export: `', this_export$uid, '`.')
    NULL
  } else {
    xlsx_parse <- unlist(stringr::str_split(this_export$result, '/'))
    parse_len <- length(xlsx_parse)
    xlsx_fn <- xlsx_parse[parse_len]
    xlsx_fn <- stringr::str_replace_all(xlsx_fn, '%20', '_')
    save_xlsx_export_from_url(this_export$result, file.path('xlsx', xlsx_fn))
  }
}
