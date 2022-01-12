play_vols <- tibble::tibble(
  play_site_id = c(
    'PLAYProject_GEORG',
    'PLAYProject_CHOPH',
    'PLAYProject_CSULB',
    'PLAYProject_CUNYS',
    'PLAYProject_NYUNI',
    'PLAYProject_INDNA',
    'PLAYProject_OHIOS',
    'PLAYProject_PRINU',
    'PLAYProject_PURDU',
    'PLAYProject_STANF',
    'PLAYProject_UCRIV',
    'PLAYProject_UCSCR',
    'PLAYProject_UHOUS',
    'PLAYProject_VBLTU',
    'PLAYProject_VCOMU'
  ),
  play_vol_id = c(
    954,
    1370,
    1376,
    1023,
    899,
    1400,
    1103,
    979,
    1363,
    1362,
    966,
    1066,
    1397,
    1391,
    982
  )
)

lookup_databrary_session <-
  function(this_site_id,
           s_number = stringr::str_pad(subject_number, 3, 'left', 0)) {
    # make sure we are authenticated to Databrary
    
    if (!file.exists('.databrary.RData')) {
      stop('Not logged-in to Databrary.')
    }
    
    this_volume <-
      dplyr::filter(play_vols, stringr::str_detect(play_site_id, this_site_id))
    
    vol_sessions <-
      databraryapi::list_sessions(this_volume$play_vol_id)
    
    dplyr::filter(vol_sessions, stringr::str_detect(name, paste0(s_number, '$')))
  }

make_databrary_url_from_session <- function(session_df) {
  with(
    session_df,
    paste0(
      "https://nyu.databrary.org/volume/",
      vol_id,
      "/slot/",
      session_id,
      '/-'
    )
  )
}
