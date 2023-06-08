
################################################################################
#' Loads a single XLSX-formatted data file, converts it to CSV, and saves it.
#'
#' @param fn Filename for XLSX file.
#' @param out_dir Directory to save CSV file.
#' @returns The name of the saved CSV file.
load_xlsx_save_csv <- function(fn, out_dir, vb = FALSE) {
  stopifnot(file.exists(fn))
  stopifnot(dir.exists(out_dir))
  
  box::use(tools[file_path_sans_ext])
  box::use(readxl[read_xlsx])
  box::use(readr[write_csv])

  xl <- readxl::read_xlsx(fn)
  fn_csv <-
    file.path(out_dir, paste0(file_path_sans_ext(basename(fn)), ".csv"))
  if (dir.exists(dirname(fn_csv))) {
    readr::write_csv(xl, fn_csv)
    if (vb)
      message("File saved: '", fn_csv, "'")
  } else {
    warning("Directory not found: '", dirname(fn_csv), "'")
  }
  fn_csv
}

###################################################################
#' Loads all XLSX-formatted files in `in_dir` and saves CSV-formatted files
#' to `out_dir`
#'
#' @param in_dir A string indicating the input directory
#' @param out_dir A string indicating the output directory
load_xlsx_save_many_csv <- function(in_dir, out_dir, filter_str) {
  stopifnot(dir.exists(in_dir))
  stopifnot(dir.exists(out_dir))
  stopifnot(is.character(filter_str))
  
  box::use(purrr)
  
  fns <- list.files(in_dir, pattern = filter_str, full.names = TRUE)
  if (length(fns) < 1) {
    warning("No files found in '", in_dir, "' with '", filter_str, "' in file name.")
    NULL
  } else {
    purrr::map_chr(fns, load_xlsx_save_csv, out_dir)
  }
}