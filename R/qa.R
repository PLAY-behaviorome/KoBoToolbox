# QA

create_agg_basic_demo <- function(csv_dir = 'csv/by_form') {
  fl <- list.files(csv_dir, 'basic_demog_[0-9]{2}', full.names = TRUE)
  if (length(fl) > 0) {
    purrr::map_df(fl, readr::read_csv)
  }
}

create_agg_pets <- function() {
  purrr::map_df(c('12_English', '18_English', '24_English', 
                  '12_Bilingual_Spanish', '24_Bilingual_Spanish'), 
                load_kbform_measure, this_measure = 'pets', force_regeneration = FALSE)
}

create_agg_measure <- function(this_measure, 
                               these_forms = c('12_English', '18_English', '24_English', 
                                               '12_Bilingual_Spanish', '24_Bilingual_Spanish',
                                               '12_Bilingual_English')) {
  purrr::map_df(these_forms, 
                load_kbform_measure, this_measure = this_measure, force_regeneration = FALSE)
}

create_agg_all <- function() {
  purrr::map(these_measures, create_agg_measure)
}

find_session_in_kbform <- function(this_form = '12_English', this_vol = 899, this_session = 41894) {
  require(tidyverse)
  
  if (!is.character(this_form)) {
    stop('`this_form` must be a string.')
  }
  
  df <- load_kbform_measure(this_form)
  
  if (is.null(df)) {
    stop(paste0('No basic_demog data extracted for form `', this_form, '`.'))
  }
  
  if ('play_site_id' %in% names(df)) {
    this_kbt <- df %>%
      dplyr::filter(., stringr::str_detect(play_site_id, paste0(this_vol, this_session)))
    if (dim(this_kbt)[1] > 0) {
      this_kbt          
    } else {
      warning(paste0('No `play_site_id` == `', "PLAY_", this_vol, this_session, '` found for form `', this_form, '`'))
      NULL 
    }
  } else {
    warning(paste0('No `play_site_id` == `', "PLAY_", this_vol, this_session, '` found for form `', this_form, '`.'))
    NULL
  }
}

# load_basic_demog()
# Loads data frame of basic demographic info from a specified KoBoToolbox form from a specified directory
# where a CSV might reside. If there is no CSV, then the function regenerates the data frame.
load_kbform_measure <- function(this_form = '12_English', this_measure = 'basic_demog', 
                              csv_dir = 'csv',
                              regenerate_if_missing = TRUE,
                              save_regenerated = TRUE,
                              force_regeneration = FALSE) {
  require(tidyverse)
  
  if (!is.character(this_form)) {
    stop('`this_form` must be a string.')
  }
  if (!is.character(this_measure)) {
    stop('`this_measure` must be a string.')
  }
  if (!is.character(csv_dir)) {
    stop('`csv_dir` must be a string.')
  }
  if (!dir.exists(paste(csv_dir, 'by_form', sep="/"))) {
    stop('Directory `', csv_dir, '` not found.')
  }
  
  this_fn <- paste0(csv_dir, "/by_form/", this_measure, "_", this_form, ".csv")
  if (!file.exists(this_fn)) {
    message('  File `', this_fn, '` not found.')
    if (regenerate_if_missing) {
      df <- extract_clean(this_form, this_measure)
      if (save_regenerated) {
        readr::write_csv(df, this_fn)
        message("  Saved file `", this_fn, '`.')
      }
      df
    }
  } else {
    if (force_regeneration) {
      message("  Regenerating data for form `", this_form, '` and measure `', this_measure, '`.')
      df <- extract_clean(this_form, this_measure)
      df$site_child_id <- as.numeric(df$site_child_id)
      if (save_regenerated) {
        readr::write_csv(df, this_fn)
        message("  Saved file `", this_fn, '`.')
      }
      df
    } else {
      readr::read_csv(this_fn)      
    }
  }
}

extract_clean <- function(this_form = '12_English', this_measure = 'basic_demog') {
  if (!is.character(this_form)) {
    stop('`this_form` must be a string.')
  }
  if (!is.character(this_measure)) {
    stop('`this_measure` must be a string.')
  }
  if (this_measure == 'basic_demog') {
    df <- extract_clean_basic_demog(this_form)
  }
  if (this_measure == 'health') {
    df <- extract_clean_health(this_form)
  }
  if (this_measure == 'pets') {
    df <- extract_clean_pets(this_form)
  }
  if (this_measure == 'mbcdi_eng_short') {
    df <- extract_clean_mbcdi_eng_short(this_form)
  }
  if (this_measure == 'dll_span_short') {
    df <- extract_clean_dll_span_short(this_form)
  }
  if (this_measure == 'dll_eng_long') {
    df <- extract_clean_dll_eng_long(this_form)
  }
  if (this_measure == 'ecbq') {
    df <- extract_clean_ecbq(this_form)
  }  
  if (this_measure == 'media') {
    df <- extract_clean_media(this_form)
  }
  if (this_measure == 'household_labor') {
    df <- extract_clean_household_labor(this_form)
  }
  if (this_measure == 'typical_day') {
    df <- extract_clean_typical_day(this_form)
  }
  if (this_measure == 'demog_quest') {
    df <- extract_clean_demo_quest(this_form)
  }
  df
}

session_from_play_id <- function(this_play_session_id = 'PLAY_89938196') {
  stringr::str_extract(this_play_session_id, '[0-9]{5}$')
}

vol_from_play_id <- function(this_play_session_id = 'PLAY_89938196') {
  this_play_session_id %>%
    stringr::str_remove(., stringr::str_extract(this_play_session_id, '[0-9]{5}$')) %>%
    stringr::str_remove(., 'PLAY_')  
}

load_databrary_session <- function(this_play_session_id = 'PLAY_89938196') {
  
  if (stringr::str_detect(this_play_session_id, 'NODB')) {
    warning('No Databrary session for `', this_play_session_id, '`.')
    return(NULL)
  }
  
  this_session <- session_from_play_id(this_play_session_id)
  this_vol <- vol_from_play_id(this_play_session_id)
  db_sessions <- databraryapi::download_session_csv(vol_id = as.numeric(this_vol))
  
  this_db_session <- db_sessions %>%
    dplyr::filter(., vol_id == this_vol,
                  session_id == this_session)
  this_db_session
}

# compare Kb data to that on Databrary

kb_eq_db_dob <- function(this_form = '18_English', this_play_session_id = 'PLAY_89938196') {
  db_df <- load_databrary_session(this_play_session_id)
  kb_df <- find_session_in_kbform(this_form, this_session = session_from_play_id(this_play_session_id),
                                  this_vol = vol_from_play_id(this_play_session_id))
  kb_df$child_dob == db_df$participant.birthdate
}

kb_eq_db_session_date <- function(this_form = '18_English', this_play_session_id = 'PLAY_89938196') {
  db_df <- load_databrary_session(this_play_session_id)
  kb_df <- find_session_in_kbform(this_form, this_session = session_from_play_id(this_play_session_id),
                                  this_vol = vol_from_play_id(this_play_session_id))

  kb_df$todays_date == db_df$session_date
}

kb_eq_db_session_date <- function(this_form = '18_English', this_play_session_id = 'PLAY_89938196') {
  db_df <- load_databrary_session(this_play_session_id)
  kb_df <- find_session_in_kbform(this_form, this_session = session_from_play_id(this_play_session_id),
                                  this_vol = vol_from_play_id(this_play_session_id))
  
  kb_df$todays_date == db_df$session_date
}

kb_eq_db_sex <- function(this_form = '18_English', this_play_session_id = 'PLAY_89938196') {
  db_df <- load_databrary_session(this_play_session_id)
  kb_df <- find_session_in_kbform(this_form, this_session = session_from_play_id(this_play_session_id),
                                  this_vol = vol_from_play_id(this_play_session_id))
  
  kb_df$child_sex == tolower(db_df$participant.gender)
}