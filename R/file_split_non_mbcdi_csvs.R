###################################################################
file_split_non_mbcdi_csvs <- function(fl, out_dir) {
  require(purrr)
  stopifnot(is.character(fl))
  stopifnot(dir.exists(out_dir))
  
  purrr::map_chr(fl,
                 open_split_save,
                 csv_save_dir = out_dir,
                 these_questions = 'non_mbcdi')
  #list.files(out_dir, full.names = TRUE)
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
  
  base_fn <- basename(fp)
  
  out_fn <- file.path(
    csv_save_dir,
    paste0(
      stringr::str_extract(base_fn, '^[0-9]+'),
      '_',
      these_questions,
      '_',
      extract_age_group_from_name(base_fn),
      '_',
      tolower(form_language(base_fn)),
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

