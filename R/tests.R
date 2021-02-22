# testing export integrity

session_has_csv_measures <- function(this_session_play_id = 'PLAY_89938196', 
                               these_measures = c('basic_demog',
                                                  'demog_quest',
                                                  'ecbq',
                                                  'health',
                                                  'household_labor',
                                                  'media',
                                                  'pets',
                                                  'typical_day',
                                                  'mbcdi_eng_short',
                                                  'dll_eng_long'),
                               by_session_csv_dir = 'csv/by_session') {
  if (!is.character(by_session_csv_dir)) {
    warning('`by_session_csv_dir` must be a character string.')
    return(NULL)
  }
  if (!dir.exists(by_session_csv_dir)) {
    warning('Directory not found: `', by_session_csv_dir, '`.')
    return(NULL)
  }
  
  this_session_csvs <- list.files(by_session_csv_dir, this_session_play_id)
  if (length(this_session_csvs) <= 0) {
    warning('No files found for session `', this_session_play_id, '`.')
    return(NULL)
  } else {
    session_csvs_found <- unlist(purrr::map(these_measures, session_has_this_measure, this_session_csvs))
    message('Found ', sum(session_csvs_found), ' CSVs of ', length(session_csvs_found), ' tested.')
  }
}

session_has_this_measure <- function(this_measure, this_session_csvs) {
  measure_in_csvs <- stringr::str_detect(this_session_csvs, this_measure)
  if (sum(measure_in_csvs) > 0) {
    message('CSV found for `', this_measure, '`: `', this_session_csvs[measure_in_csvs])
    TRUE
  } else {
    message('CSV not found for `', this_measure, '`.')
    FALSE
  }
}

expected_measures_by_form <- function(this_form, form_measures_fn = 'csv/.analysis/form_measures.csv') {
  require(tidyverse) # for pipe
  
  if (!is.character(this_form)) {
    warning('`this_form` must be a character string.')
    return(NULL)
  }
  if (!(this_form %in% c('12_English', '12_Bilingual_English', '12_Bilingual_Spanish',
                         '18_English', '18_Bilingual_English', '18_Bilingual_Spanish',
                         '24_English', '24_Bilingual_English', '24_Bilingual_Spanish',
                         'Demographic_Questionnaire', 'Post-Visit_Notes'))) {
    warning('Form not found: `', this_form, '`.')
    return(NULL)
  }
  if (!is.character(form_measures_fn)) {
    warning('`form_measures_fn` must be a character string.')
    return(NULL)
  }
  if (!file.exists(form_measures_fn)) {
    warning('File not found: `', form_measures_fn, '`.')
    return(NULL)
  }
  
  form_measures_df <- readr::read_csv(form_measures_fn)
  if (dim(form_measures_df)[1] <= 0) {
    warning('Error reading data for form: `', this_form, '`.')
    return(NULL)
  } else {
    form_measures_df %>%
      dplyr::filter(., kobo_form == this_form) %>%
      dplyr::select(., measure_type)    
  }
}