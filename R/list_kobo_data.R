################################################################################
#' Lists the datasets available on KoBoToolbox using the PLAY credentials.
#'
#' @param URL A string that is the API call to extract the data. It defaults
#' to 'https://kc.kobotoolbox.org/api/v1/data'
#' @param return_df A logical value indicating whether to return a data frame
#' (the default) or JSON.
#' @return A data.frame with the 'description', 'title', 'url', and other
#' information for the deployed forms on PLAY.
list_kobo_data <-
  function(URL = "https://kc.kobotoolbox.org/api/v1/data",
           return_df = TRUE) {
    stopifnot(is.character(URL))
    stopifnot(is.logical(return_df))
    
    require(httr)
    
    kobo_api_key <- Sys.getenv("KOBO_API_KEY")
    if (!is.character(kobo_api_key)) {
      stop('No KoBoToolbox API key stored in ~/.Renviron.')
    }
    
    config_params <-
      httr::add_headers(Authorization = paste0('Token ', kobo_api_key))
    
    r <- httr::GET(URL, config = config_params)
    if (httr::status_code(r) == 200) {
      c <- httr::content(r, as = 'text', encoding = 'utf8')
      if (return_df) {
        df <- jsonlite::fromJSON(c)
        
        # Add form URL
        df |>
          dplyr::mutate(form_url = paste0("https://kf.kobotoolbox.org/api/v2/assets/", 
                                          id_string, ".xls"))
      } else {
        # JSON
        c
      }
    } else {
      message('HTTP call to ',
              URL,
              ' failed with status `',
              httr::status_code(r),
              '`.')
      NULL
    }
  }
