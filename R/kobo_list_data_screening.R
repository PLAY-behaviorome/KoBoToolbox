################################################################################
#' Lists the screening/demographic datasets available on KoBoToolbox using the 
#' PLAY KoBoToolbox credentials.
#'
#' @param URL A string that is the API call to extract the data. It defaults
#' to 'https://kc.kobotoolbox.org/api/v1/data'
#' @param return_df A logical value indicating whether to return a data frame
#' (the default) or JSON.
#' @returns A data.frame with the KoBoToolbox forms from the 
#' screening/demographic survey.
kobo_list_data_screening <- function() {
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(stringr))
  
  kb_df <- kobo_list_data()
  
  dplyr::filter(kb_df,
                stringr::str_detect(title, "Demographic"))
}