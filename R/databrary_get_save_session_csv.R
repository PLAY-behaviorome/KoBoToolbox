databrary_get_save_session_csv <- function(this_site = "NYUNI",
                                           csv_dir = "data/csv/site_sessions",
                                           vb = FALSE,
                                           rq = NULL) {
  stopifnot(is.character(this_site))
  stopifnot(is.character(csv_dir), dir.exists(csv_dir))
  stopifnot(is.logical(vb))
  
  require(databraryr)
  require(readr)
  
  if (!(exists("databrary_get_session_data_from_site"))) {
    if (vb)
      message("databrary_get_session_data_from_site() not in environment. Sourcing.")
    source("R/databrary_get_session_data_from_site.R")
  }
  
  if (!(exists("PLAY_VOLS"))) {
    PLAY_VOLS <-
      readr::read_csv("data/csv/_meta/play_site_vols.csv",
                      show_col_types = FALSE)
  }
  
  if (vb)
    message("Retrieving sessions from '", this_site, "'.")
  df <- databrary_get_session_data_from_site(this_site, vb = vb)
  
  if ((!is.data.frame(df)) || is.null(df)) {
    if (vb)
      message(" No data retrieved from site '", this_site, "'. No file saved.")
    NULL
  } else {
    fn = file.path(csv_dir, paste0(this_site, ".csv"))
    readr::write_csv(df, fn)
    fn
  }
}