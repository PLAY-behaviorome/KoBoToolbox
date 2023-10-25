mcdi_convert_understands <- function(csv_fn) {
  assertthat::is.string(csv_fn)
  assertthat::is.readable(csv_fn)
  
  out_fn <- tempfile()
  gsub('understands___', 'understands_says', readLines(csv_fn)) |>
    writeLines(con = out_fn)
  out_fn
}
