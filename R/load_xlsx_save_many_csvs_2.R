###################################################################
#' Loads all XLSX-formatted files in `in_dir` and saves CSV-formatted files
#' to `out_dir`
#'
#' @param in_dir A string indicating the input directory
#' @param out_dir A string indicating the output directory
load_xlsx_save_many_csvs_2 <- function(fns, out_dir) {
  require(purrr)
  stopifnot(is.character(fns))
  stopifnot(dir.exists(out_dir))
  
  #  fns <- list.files(in_dir, pattern = filter_str, full.names = TRUE)
  if (length(fns) < 1) {
    warning("fns is empty")
    NULL
  } else {
    purrr::map_chr(fns, load_xlsx_save_csv, out_dir)
  }
}
