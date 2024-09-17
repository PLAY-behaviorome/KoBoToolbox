###################################################################
#' Takes a list of CSV data files and merges them.
#' NOTE: we use read.csv() to _avoid_ the problem of inferring
#' column types differently across the datasets.
file_make_aggregate_from_csvs <- function(fl) {
  require(purrr)
  stopifnot(is.character(fl))
  
  purrr::map_df(fl, read.csv, colClasses = 'character')
}