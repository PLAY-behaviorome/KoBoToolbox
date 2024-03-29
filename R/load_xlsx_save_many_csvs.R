###################################################################
#' Loads all XLSX-formatted files in `in_dir` and saves CSV-formatted files
#' to `out_dir`
#'
#' @param in_dir A string indicating the input directory
#' @param out_dir A string indicating the output directory
load_xlsx_save_many_csvs <- function(in_dir, out_dir, filter_str) {
  stopifnot(dir.exists(in_dir))
  stopifnot(dir.exists(out_dir))
  stopifnot(is.character(filter_str))
  
  suppressPackageStartupMessages(require(purrr))
  
  fns <- list.files(in_dir, pattern = filter_str, full.names = TRUE)
  if (length(fns) < 1) {
    warning("No files found in '", in_dir, "' with '", filter_str, "' in file name.")
    NULL
  } else {
    purrr::map_chr(fns, load_xlsx_save_csv, out_dir)
  }
}