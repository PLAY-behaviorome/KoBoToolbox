home_select_site_data <- function(this_site_id = "UCSCR",
                                  in_dir = "data/csv/home_visit/agg",
                                  in_fn = "PLAY-non-mcdi-kobo-latest.csv",
                                  out_dir = "data/csv/home_visit/agg/by-site",
                                  vb = FALSE) {
  assertthat::is.string(this_site_id)
  
  assertthat::is.string(in_dir)
  assertthat::is.readable(in_dir)
  
  assertthat::is.string(in_fn)
  
  assertthat::is.string(out_dir)
  assertthat::is.readable(out_dir)
  
  out_fn <- paste0(out_dir, "/PLAY-non-mcdi-", this_site_id, ".csv")
  
  in_fn_full <- paste0(in_dir, "/", in_fn)
  
  if (vb)
    message("Reading latest home_visit data file: '", in_fn_full, "'")
  df <-
    readr::read_csv(
      paste0(in_dir, "/", in_fn),
      col_types = readr::cols(.default = 'c'),
      show_col_types = FALSE
    )
  
  if (is.data.frame(df)) {
    if (vb)
      message("Filtering for data from site: ", this_site_id)
    df |>
      dplyr::filter(site_id == this_site_id) |>
      dplyr::arrange(participant_ID) |>
      readr::write_csv(out_fn)
    out_fn
  } else {
    message("No data returned from '", in_fn_full, "'")
  }
}