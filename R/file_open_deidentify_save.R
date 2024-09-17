###################################################################
#' Opens a CSV, removes identifying columns, and saves new copy with
#' the identifiers removed.
#'
#' @param fp Filename
#' @csv_save_dir Dirctory for saved file
#' @these_questions Filter to select type of files
#' @rename_cols Rename cols with basename()
#' @vb Provide verbose output
file_open_deidentify_save <- function(fp,
                                 csv_save_dir = 'tmp',
                                 these_questions = 'non_mbcdi',
                                 rename_cols = FALSE,
                                 vb = FALSE) {
  require(tidyverse)
  stopifnot(is.character(fp))
  stopifnot(dir.exists(csv_save_dir))
  stopifnot(is.character(these_questions))
  stopifnot(is.logical(rename_cols))
  stopifnot(is.logical(vb))
  
  if (!dir.exists(csv_save_dir)) {
    stop("Directory not found: '", csv_save_dir, "'")
  }
  
  if (file.exists(fp)) {
    df <- readr::read_csv(fp, show_col_types = FALSE)
    if (is.data.frame(df)) {
      df <- file_remove_identifiers(df)
    } else {
      stop(paste0('Error in reading data frame.'))
    }
  } else {
    stop(paste0('Cannot read file `', fp, '`'))
  }
  
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
      '_deidentified',
      '.csv'
    )
  )
  
  if (!is.null(df)) {
    readr::write_csv(df, out_fn)
    message('Saved `', out_fn, '`')
  } else {
    message('Error in exporting data to `', out_fn, '`')
  }
  out_fn
}