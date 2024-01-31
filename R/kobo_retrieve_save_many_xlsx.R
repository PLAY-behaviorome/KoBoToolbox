#-------------------------------------------------------------------------------
#' Retrieves demographic/screening or home visit forms from KoBoToolbox and
#' saves an XLSX file in a local directory.
#'
#' @param df A data frame of the selected forms from the KoBoToolbox API.
#' Default is to call list_kobo_data().
#' @param save_dir A character string indicating the directory to save
#' the downloaded files. Default is 'tmp'.
#' @param save_form Logical value. Save the KBT "form" file or not. 
#' Default is TRUE.
#' 
#' @returns Files retrieved and saved by call to kobo_retrieve_save_xlsx()
#' 
#' @export
kobo_retrieve_save_many_xlsx <-
  function(df = kobo_list_data(),
           save_dir = 'tmp',
           save_form = TRUE) {
    
    stopifnot(is.data.frame(df))
    stopifnot(dim(df)[1] > 0)
    stopifnot(is.character(save_dir))
    stopifnot(dir.exists(save_dir))
    
    # source("R/kobo_retrieve_save_xlsx.R")
    
    n_files <- dim(df)[1]
    message("Retrieving ", n_files, " files from KoBoToolbox site.")
    purrr::map(
      1:n_files,
      kobo_retrieve_save_xlsx,
      kb_df = df,
      save_dir = save_dir,
      save_form = TRUE,
      .progress = "KoBo files:"
    )
  }