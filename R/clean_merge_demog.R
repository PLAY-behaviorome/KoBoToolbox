###################################################################
#' Cleans set of screening/demographic data files and aggregates them into
#' a single data frame.
#'
#' @param fns A character vector of CSV filenames.
#' @return A single dataframe with the aggregated demographic data files.
clean_merge_demog <- function(fns) {
  stopifnot(is.character(fns))
  
  rbind(clean_demog_1(fns[1]),
        clean_demog_2(fns[2]),
        clean_demog_2(fns[3])) %>%
    dplyr::arrange(., submit_date)
}