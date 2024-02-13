databrary_get_session_data_from_site <-
  function(this_site, vb = FALSE) {
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
    if (is.null(this_volume) | dim(this_volume)[1] == 0) {
      if (vb)
        message("Problem retrieving PLAY site data from `play_vols`")
      return(NULL)
    }
    if (vb) message("This volume is ", this_volume$play_vol_id)
    
    if (vb) message("Retrieving session info for volume ", this_volume$play_vol_id)
    vol_sessions <-
      databraryr::download_session_csv(
        as.numeric(this_volume$play_vol_id),
        vb = vb,
        as_df = TRUE,
        rq = lrq
      )
    
    if (!is.null(vol_sessions)) {
      vol_sessions |>
        dplyr::mutate(vol_id = this_volume$play_vol_id)
    } else {
      if (vb)
        message('Cannot access session data from volume: ',
                this_volume$play_vol_id)
      NULL
    }
  }