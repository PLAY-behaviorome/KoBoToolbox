###################################################################
file_split_mbcdi_csvs <- function(fl, out_dir) {
  require(purrr)
  stopifnot(is.character(fl))
  stopifnot(dir.exists(out_dir))
  
  purrr::map_chr(fl,
                 open_split_save,
                 csv_save_dir = out_dir,
                 these_questions = 'mbcdi')
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
    
    # Add subject_number and site_id temporarily for cross-referencing.
    subject_number <-
      (1:length(names(df)))[stringr::str_detect(names(df), 'subject_number')]
    site_id <-
      (1:length(names(df)))[stringr::str_detect(names(df), 'site_id')]
    
    # Select non-mcdi cols
    mcdi_qs <- stringr::str_detect(names(df), 'mcdi|vocab')
    # non_mcdi_qs <- !(mcdi_qs)
    mcdi_cols <- (1:length(names(df)))[mcdi_qs]
    
    mcdi <- df %>%
      dplyr::select(
        .,
        all_of(play_id_col),
        all_of(site_id),
        all_of(subject_number),
        all_of(mcdi_cols)
      )
    
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
