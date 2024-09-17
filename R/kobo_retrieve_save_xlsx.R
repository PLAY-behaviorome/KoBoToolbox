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
#' @param save_form A logical value. Save the KBT "form" file. Default is TRUE.
#' @returns The name of the saved .xlsx file.
#' 
#' @export
kobo_retrieve_save_xlsx <-
  function(form_index = 13,
           kb_df = kobo_list_data(),
           save_dir = 'tmp',
           ans_dir = 'ans',
           qs_dir = 'qs',
           save_form = TRUE,
           vb = TRUE) {
    
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
      #form_URL <- kb_df$url[form_index]
      questions_URL <- kb_df$form_url[form_index]
      
      # suppressPackageStartupMessages(require(httr))
      
      # Append KoBo form id to cleaned file name to avoid duplicates
      form_name <-
        kobo_create_cleaned_augmented_form_name(form_index, kb_df)
      
      # Must export as .xlsx even though API queries for .xls
      file_name_ans <- file.path(save_dir, ans_dir, paste0(form_name, '.xlsx'))
      file_name_qs <- file.path(save_dir, qs_dir, paste0(form_name, '-forms.xlsx'))
      
      # kobo_api_key <- Sys.getenv("KOBO_API_KEY")
      # if (!is.character(kobo_api_key)) {
      #   stop('No KoBoToolbox API key stored in ~/.Renviron.')
      # }
      # 
      # config_params <-
      #   httr::add_headers(Authorization = paste0('Token ', kobo_api_key))
      # 
      # r <- httr::GET(form_URL, config = config_params)
      # if (httr::status_code(r) == 200) {
      #   c <- httr::content(r, as = 'raw')
      #   writeBin(c, file_name)
      #   # message('Saved `', file_name, '`')
      #   file_name
      # } else {
      #   message('HTTP call to ',
      #           form_URL,
      #           ' failed with status `',
      #           httr::status_code(r),
      #           '`')
      #   "NULL"
      # }
      
      message("Retrieving questionnaire responses...")
      rq <- make_kobo_request(form_URL)
      resp <- tryCatch(
        httr2::req_perform(rq),
        httr2_error = function(cnd) {
          if (vb)
            message("Error retrieving : ", form_URL)
          NULL
        }
      )
      
      if (!is.null(resp)) {
        body <- httr2::resp_body_raw(resp)
        writeBin(body, file_name_ans)
        message('Saved `', file_name_ans, '`')
      } else {
        resp
      }
      
      # Questionnaire forms
      if (save_form) {
        message("Retrieving questions...")
        if (!dir.exists(file.path(save_dir, qs_dir))) {
          message("Directory does not exist: ", file.path(save_dir, qs_dir))
          return(NULL)
        }
        
        rq <- make_kobo_request(questions_URL)
        resp <- tryCatch(
          httr2::req_perform(rq),
          httr2_error = function(cnd) {
            if (vb)
              message("Error retrieving : ", questions_URL)
            NULL
          }
        )
        
        if (!is.null(resp)) {
          body <- httr2::resp_body_raw(resp)
          writeBin(body, file_name_qs)
          message('Saved `', file_name_qs, '`')
        } else {
          resp
        }
        
        # r <- httr::GET(questions_URL, config = config_params)
        # if (httr::status_code(r) == 200) {
        #   c <- httr::content(r, as = 'raw')
        #   writeBin(c, file_name_qs)
        #   # message('Saved `', file_name, '`')
        #   file_name_qs
        # } else {
        #   message('HTTP call to ',
        #           form_URL,
        #           ' failed with status `',
        #           httr::status_code(r),
        #           '`')
        #   "NULL"
        # }        
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
kobo_create_cleaned_augmented_form_name <-
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
      # source("R/kobo_extract_form_id.R")
      # source("R/kobo_clean_form_name.R")
      # source("R/kobo_extract_form_name.R")
      
      paste0(
        kobo_extract_form_id(form_index, kb_df),
        "_",
        kobo_clean_form_name(kobo_extract_form_name(form_index, kb_df))
      )
    }
  }
