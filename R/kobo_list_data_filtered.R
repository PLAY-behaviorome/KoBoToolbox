#-------------------------------------------------------------------------------
#' Lists the datasets available on KoBoToolbox using the 
#' PLAY KoBoToolbox credentials and filters them based on a user-defined string
#'
#' @param filter_str A string indicating what the form name must contain.
#' @returns A data.frame with the filtered KoBoToolbox forms.
#' @export
kobo_list_data_filtered <- function(filter_str = '[Dd]emographic') {
  stopifnot(is.character(filter_str))
  
  # source("R/kobo_list_data.R")
  
  kb_df <- kobo_list_data()
  
  dplyr::filter(kb_df,
                stringr::str_detect(title, filter_str))
}
