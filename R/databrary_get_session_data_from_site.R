databrary_get_session_data_from_site <- function(this_site, vb = FALSE) {
  require(databraryr)
  stopifnot(is.character(this_site))
  stopifnot(is.logical(vb))
  
  if (!(exists("PLAY_VOLS"))) {
    PLAY_VOLS <-
      readr::read_csv("data/csv/_meta/play_site_vols.csv",
                      show_col_types = FALSE)
  }
  
  this_volume <-
    dplyr::filter(PLAY_VOLS, stringr::str_detect(play_site_id, this_site))
  if (is.null(this_volume)) {
    if (vb)
      message("Problem retrieving PLAY site data from `play_vols`")
    return(NULL)
  }
  
  vol_sessions <-
    databraryr::get_session_as_df(as.numeric(this_volume$play_vol_id))
  
  if (!is.null(vol_sessions)) {
    vol_sessions
  } else {
    if (vb)
      message('Cannot access session data from volume: ',
              this_volume$play_vol_id)
    NULL
  }
}