################################################################################
#' Retrieves demographic/screening or home visit forms from KoBoToolbox and
#' saves them in a local directory.
#'
#' @param df A data frame of the selected forms from the KoBoToolbox API
#' @param save_dir A character string indicating the directory to save
#' the downloaded files.
#' @return NULL
retrieve_kobo_xlsx <- function(df, save_dir) {
  stopifnot(is.data.frame(df))
  stopifnot(dir.exists(save_dir))
  
  require(purrr)
  
  n_files <- dim(df)[1]
  purrr::map_chr(1:n_files,
                 retrieve_save_xls_export,
                 kb_df = df,
                 save_dir = save_dir)
}