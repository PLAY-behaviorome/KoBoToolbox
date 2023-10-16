#-------------------------------------------------------------------------------
#' Retrieves demographic/screening or home visit forms from KoBoToolbox and
#' saves an XLSX file in a local directory.
#'
#' @param df A data frame of the selected forms from the KoBoToolbox API.
#' Default is to call list_kobo_data().
#' @param save_dir A character string indicating the directory to save
#' the downloaded files. Default is 'tmp'.
#' @returns Files retrieved and saved by call to kobo_retrieve_save_xlsx()
#' @export
kobo_retrieve_save_many_xlsx <-
  function(df = kobo_list_data(), save_dir = 'tmp') {
    stopifnot(is.data.frame(df))
    stopifnot(dim(df)[1] > 0)
    stopifnot(is.character(save_dir))
    stopifnot(dir.exists(save_dir))
  
    n_files <- dim(df)[1]
    message("Retrieving ", n_files, " files from KoBoToolbox site.")
    purrr::map_chr(
      1:n_files,
      kobo_retrieve_save_xlsx,
      kb_df = df,
      save_dir = save_dir,
      .progress = "KoBo files:"
    )
  }