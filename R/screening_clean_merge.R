###################################################################
#' Cleans set of screening/demographic data files and aggregates them into
#' a single data frame.
#'
#' @param fns A character vector of CSV file names.
#' @returns A single dataframe with the aggregated demographic data files.
screening_clean_merge <- function(fns) {
  stopifnot(is.character(fns))
  
  suppressPackageStartupMessages(require(dplyr))
  
  rbind(screening_clean_1(fns[1]),
        screening_clean_2(fns[2]),
        screening_clean_2(fns[3])) %>%
    dplyr::arrange(., submit_date)
}