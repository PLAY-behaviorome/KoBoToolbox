make_kobo_request <- function(api_route) {
  assertthat::is.string(api_route)

  rq <- httr2::request(api_route) %>%
    httr2::req_headers(Authorization = paste0("Token ", KOBO_API_KEY)) %>%
    httr2::req_retry(max_tries = RETRY_LIMIT) %>%
    httr2::req_timeout(REQUEST_TIMEOUT)
  rq
}

