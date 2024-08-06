screen_recode_site_id <- function(df) {
  stopifnot(is.data.frame(df))
  
  df |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "new_york_unive", "NYUNI")) |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "^NYU$", "NYUNI")) |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "georgetown_uni", "GEORG")) |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "^GTN$", "GEORG")) |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "^UCR$", "UCRIV")) |>
    dplyr::mutate(site_id = stringr::str_replace_all(site_id, "^university_of__3", "?????"))
}
