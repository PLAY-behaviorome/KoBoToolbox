#-------------------------------------------------------------------------------
#' Loads all XLSX-formatted files in `in_dir` and saves CSV-formatted files
#' to `out_dir`
#'
#' @param in_dir A string indicating the input directory
#' @param out_dir A string indicating the output directory
#' @export
file_load_xlsx_save_many_csv <- function(in_dir, out_dir, filter_str) {
  stopifnot(dir.exists(in_dir))
  stopifnot(dir.exists(out_dir))
  stopifnot(is.character(filter_str))
  
  # if (!('file_load_xlsx_save_csv' %in% ls()))
  #   source("R/file_load_xlsx_save_csv.R")
  
  fns <- list.files(in_dir, pattern = filter_str, full.names = TRUE)
  if (length(fns) < 1) {
    warning("No files found in '",
            in_dir,
            "' with '",
            filter_str,
            "' in file name.")
    NULL
  } else {
    purrr::map_chr(fns, file_load_xlsx_save_csv, out_dir)
  }
}