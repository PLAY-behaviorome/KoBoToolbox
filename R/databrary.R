# Functions to interact with Databrary

check_db_authentication <- function() {
  if (!file.exists('.databrary.RData')) {
    databraryapi::login_db()
  }
}

match_session_id_from_databrary <- function(df) {
  require(tidyverse)
  
  check_db_authentication()
  # Given a data frame with a session-specific data, determine if the session is on Databrary
  db_site_info <- get_db_site_info(df)
  if (is.null(db_site_info)) {
    stop(paste0('Unable to match this session to known sites.'))
    df
  }
  
  # Get session list from Databrary
  db_session_list_for_vol <- databraryapi::list_sessions_in_volume(db_site_info$db_vol_id)
  
  if (is.null(db_session_list_for_vol)) {
    stop(paste0('Unable to retrieve info for volume `', db_site_info$db_vol_id, '` from Databrary.'))
  }
  
  this_session <- db_session_list_for_vol %>%
    dplyr::filter(., stringr::str_detect(name, as.character(df$site_child_id))) %>%
    dplyr::select(., session_id)
  
  if (dim(this_session)[1] == 0) {
    paste0("_", df$site_child_id, "_NODB")
  } else {
    this_session
  }
}

get_db_site_info <- function(df) {
  require(tidyverse)
  
  if (is.null(df)) {
    stop('Data frame must not be NULL.')
    NULL
  }
  
  # sites_df <- readr::read_csv('csv/.analysis/sites_databrary.csv')
  # if (dim(sites_df)[1] <= 0) {
  #   stop('Unable to read `sites_databrary.csv`')
  # }
  
  if ('play_site_name' %in% names(df)) {
    sites_df <- readr::read_csv('csv/.analysis/sites_databrary.csv')
    
    if (dim(sites_df)[1] <= 0) {
      stop('Unable to read `sites_databrary.csv`')
    }
    
    this_site <- sites_df %>%
      dplyr::filter(., site_name == df$play_site_name)
    this_site
    
  } else {
    message("`play_site_id` not found in data frame.")
    NULL
  }
}

make_play_site_code <- function(df) {
  this_site <- get_db_site_info(df)
  this_session <- match_session_id_from_databrary(df)
  paste0("PLAY_", this_site$db_vol_id, this_session)
}

get_play_site_code <- function(row_index, df) {
  if (row_index > dim(df)[1]) {
    stop('Row `', row_index, '` out of range.')
  }
  if (row_index <= 0) {
    stop('`row_index` must be > 0.')
  }
  #cat(row_index, "\n")
  make_play_site_code(df[row_index,])
}

get_play_site_codes <- function(df) {
  row_index <- 1:dim(df)[1]
  unlist(purrr::map(row_index, get_play_site_code, df))
}
  
  