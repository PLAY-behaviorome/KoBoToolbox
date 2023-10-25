file_open_csv <- function(csv_fn) {
  assertthat::is.string(csv_fn)
  assertthat::is.readable(csv_fn)
  
  df <- readr::read_csv(convert_understands(csv_fn),
                        col_types = readr::cols(.default = 'c'),
                        show_col_types = FALSE)
}
