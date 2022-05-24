# These functions enable integration between the data collected via
# KoBoToolbox and that stored on Databrary

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
    'PLAYProject_VCOMU',
    'PLAYProject_UIOWA'
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
    982,
    1422
  )
)

lookup_databrary_session <-
  function(this_site_id,
           s_number) {
    
    if (!is.character(this_site_id)) {
      stop('`this_site_id` must be a character string.')
    }
    if (!is.numeric(as.numeric(s_number))) {
      stop('`s_number` must be a number.')
    }
    if (!file.exists('.databrary.RData')) {
      stop('Not logged-in to Databrary.')
    }
    
    this_volume <-
      dplyr::filter(play_vols, stringr::str_detect(play_site_id, this_site_id))
    if (dim(this_volume)[1] <= 0) {
      message('No volume found for site ', this_site_id)
      return(NULL)
    }
    
    vol_sessions <-
      databraryapi::list_sessions(as.numeric(this_volume$play_vol_id, vb = TRUE))
    
    if (!is.null(vol_sessions)) {
      s_number_zero_padded <- stringr::str_pad(s_number, 3, 'left', 0)
      df <- dplyr::filter(vol_sessions, stringr::str_detect(name, paste0(s_number_zero_padded, '$'))) 
      df <- dplyr::rename(df, session_name = name)
      df <- dplyr::mutate(df, release = recode(release, "1"="shared", "2"= "learning", "0"="private"))
      dplyr::select(df, -top)
    } else {
      message('Cannot access volume ', this_volume$play_vol_id)
      NULL
    }
  }

get_databrary_session_data <-  function(this_site_id,
                                        s_number) {
  
  if (!is.character(this_site_id)) {
    stop('`this_site_id` must be a character string.')
  }
  if (!is.numeric(as.numeric(s_number))) {
    stop('`s_number` must be a number.')
  }
  if (!file.exists('.databrary.RData')) {
    stop('Not logged-in to Databrary.')
  }
  
  this_volume <-
    dplyr::filter(play_vols, stringr::str_detect(play_site_id, this_site_id))
  if (dim(this_volume)[1] <= 0) {
    message('No volume found for site ', this_site_id)
    return(NULL)
  }
  
  vol_sessions <-
    databraryapi::download_session_csv(as.numeric(this_volume$play_vol_id))
  
  if (!is.null(vol_sessions)) {
    s_number_zero_padded <- stringr::str_pad(s_number, 3, 'left', 0)
    df <- dplyr::filter(vol_sessions, stringr::str_detect(session_name, paste0(s_number_zero_padded, '$'))) 
    #df <- dplyr::rename(df, session_name = name)
    #df <- dplyr::mutate(df, release = recode(release, "1"="shared", "2"= "learning", "0"="private"))
    #dplyr::select(df, -context.state)
    df
  } else {
    message('Cannot access volume ', this_volume$play_vol_id)
    NULL
  }
}

make_databrary_url_from_session <- function(session_df) {
  paste0(
      "https://nyu.databrary.org/volume/",
      session_df$vol_id,
      "/slot/",
      session_df$session_id,
      '/-'
    )
}

add_databrary_url_to_session_df <- function(session_df) {
  dplyr::mutate(session_df, 
                session_url = purrr::map_chr(1:dim(session_df)[1], 
                                             make_databrary_url_from_session_list, df = session_df))
}

make_databrary_url_from_session_list <- function(row_index = 1, df) {
  this_session <- df[row_index,]
  make_databrary_url_from_session(this_session)
}

generate_databrary_guid <- function(session_df) {
  paste0(session_df$session_id, stringr::str_pad(session_df$vol_id, 5, 'left', 0))
}

generate_databrary_url <- function(session_df) {
  paste0(
    "https://nyu.databrary.org/volume/",
    session_df$vol_id,
    "/slot/",
    session_df$session_id,
    '/-'
  )
}

add_play_session_name <- function(df) {
  # Input is data frame/tibble from PLAY_non_mbcdi.csv
  dplyr::mutate(df, session_name = paste0('PLAY_', site_id, '_', 
                                         stringr::str_pad(subject_number, 3, 'left', 0)))
}

add_databrary_info_to_kobo <- function(fn = 'tmp/PLAY_non_mbcdi_all.csv') {
  # Generate KoBo and add full Databrary-compatible PLAY session name
  message('Importing KoBo survey data.')
  play_kobo <- readr::read_csv(fn, show_col_types = FALSE)
  play_kobo <- add_play_session_name(play_kobo)
  
  # Gather session info from Databrary and add GUID and URL
  message('Getting session info from Databrary.')
  #play_db_sessions <- purrr::map2_df(play_kobo$site_id, play_kobo$subject_number, lookup_databrary_session)
  play_db_sessions <- purrr::map2_df(play_kobo$site_id, play_kobo$subject_number, get_databrary_session_data)
  
  message('Modifying Databrary session info.')
  play_db_sessions <- dplyr::mutate(play_db_sessions, databrary_guid = generate_databrary_guid(play_db_sessions),
                                    databrary_url = generate_databrary_url(play_db_sessions))
  
  # Join databases with common session_id's
  dplyr::inner_join(play_kobo, play_db_sessions)
}

create_save_kobo_db_merge_csv <- function(in_fn = 'tmp/PLAY_non_mbcdi_all.csv', 
                                          out_fn = 'tmp/PLAY_non_mbcdi_all_merge.csv') {
  df <- add_databrary_info_to_kobo(in_fn)
  
  readr::write_csv(df, out_fn)
  message("Saved file: ", out_fn)
}
