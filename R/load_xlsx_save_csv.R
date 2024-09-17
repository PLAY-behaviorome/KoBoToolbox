###################################################################
#' Loads a single XLSX-formatted data file, converts it to CSV, and saves it.
#'
#' @param fn Filename for XLSX file
#' @param out_dir Directory to save CSV file
load_xlsx_save_csv <- function(fn, out_dir, vb = FALSE) {
  # require(tools)
  # require(readxl)
  # require(readr)
  stopifnot(file.exists(fn))
  stopifnot(dir.exists(out_dir))
  
  xl <- readxl::read_xlsx(fn)
  fn_csv <-
    file.path(out_dir, paste0(tools::file_path_sans_ext(basename(fn)), ".csv"))
  if (dir.exists(dirname(fn_csv))) {
    readr::write_csv(xl, fn_csv)
    if (vb)
      message("File saved: '", fn_csv, "'")
  } else {
    warning("Directory not found: '", dirname(fn_csv), "'")
  }
  fn_csv
}
