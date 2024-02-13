#-------------------------------------------------------------------------------
#' Lists the datasets available on KoBoToolbox using the PLAY credentials.
#'
#' @param URL A string that is the API call to extract the data. It defaults
#' to 'https://kc.kobotoolbox.org/api/v1/data'
#' @returns A data.frame with the 'description', 'title', 'url', and other
#' information for the deployed forms on PLAY.
kobo_list_data <-
  function(URL = GET_KOBO_DATA_V1) {
    
    assertthat::is.string(URL)
    
    rq <- make_kobo_request(URL)
    resp <- tryCatch(
      httr2::req_perform(rq),
      httr2_error = function(cnd) {
        if (vb)
          message("Error retrieving data.")
        NULL
      }
    )
    
    if (!is.null(resp)) {
      df <- httr2::resp_body_json(resp) %>% 
        purrr::map(as.data.frame) %>% 
        purrr::list_rbind() %>%
        dplyr::mutate(form_url = sprintf(GET_KOBO_FORM, id_string))
      df
    } else {
      resp
    }
  }
