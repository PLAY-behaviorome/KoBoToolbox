# kobo helper functions

# Lists unique KB form "fragments" in the forms-measures CSV database
list_kobo_form_frags <- function(csv_fn = 'csv/form-measures.csv') {
  if (!file.exists(csv_fn)) {
    stop(paste0('Kobo form and measures file not found: ', csv_fn))
  }
  
  fm <- readr::read_csv(csv_fn)
  if (!is.data.frame(fm)) {
    stop(paste0('Cannot parse form-measures.csv into data frame'))
  }
  
  unique(fm$kobo_form)
}

# Returns the exported KB xlsx form filename that matches the form fragment
return_matching_kobo_form <- function(kobo_form_frag = "18_English", 
                                    xlsx_dir = '~/Box/PLAY-Project@/PLAY_Questionnaires (temporary)/KBTB Files'){
  
  if (!dir.exists(xlsx_dir)) {
    stop(paste0('Kobo xlsx directory not found: ', xlsx_dir))
  }
  
  fl <- list.files(xlsx_dir, kobo_form_frag, full.names = TRUE)
  unlist(fl)
}

extract_measures_for_form <- function(this_form = '18_English', 
                                               csv_fn = 'csv/form-measures.csv') {
  
  if (!file.exists(csv_fn)) {
    stop(paste0('Kobo form and measures file not found: ', csv_fn))
  }
  
  fm <- readr::read_csv(csv_fn)
  if (!is.data.frame(fm)) {
    stop(paste0('Cannot parse form-measures.csv into data frame'))
  }
  
  these_measures <- dplyr::filter(fm, kobo_form == this_form)
  dplyr::select(these_measures, measure_type, col_range)
}

extract_range_for_form_measure <- function(this_form = '18_English', this_measure = 'basic_demog') {
  require(tidyverse)
  
  these_measures <- extract_measures_for_form(this_form)
  
  this_cell_range <- these_measures %>%
    dplyr::filter(., measure_type == this_measure) %>%
    dplyr::select(., col_range) 
  
  these_measures
  as.character(this_cell_range)
}

extract_obs_for_measure <- function(this_measure = 'basic_demog', this_form = '18_English') {
  require(tidyverse)
  
  these_measures <- extract_measures_for_form(this_form)
  
  this_form_fn <- return_matching_kobo_form(this_form)
  if (length(this_form_fn) != 1) {
    stop('More than one file for this form.')
  }
  
  this_cell_range <- these_measures %>%
    dplyr::filter(., measure_type == this_measure) %>%
    dplyr::select(., col_range) 
  
  this_cell_range <- as.character(this_cell_range)
  if (this_cell_range == "") {
    stop("No valid cell range specified for form ", this_form, ' and measure ', this_measure)
  }
  
  df <- readxl::read_excel(this_form_fn, range = readxl::cell_cols(this_cell_range), sheet = 1)
  
  # handle special case of empty array
  if (dim(df)[1] == 0) {
    df[1,] = NA
  }
  df
}

clean_basic_demog <- function(df) {
  require(tidyverse)
  clean_df <- df %>%
    dplyr::rename(., start_time = 1,
                  end_time = 2,
                  todays_date = 5,
                  play_site_id = 6,
                  experimenter_name = 7,
                  session_date = 8,
                  site_child_id = 9,
                  # child_first = 10,
                  # child_middle = 11,
                  # child_last = 12,
                  child_dob = 13,
                  child_sex = 14,
                  child_birth_city = 15,
                  play_child_id = 29,
                  child_age_grp = 30,
                  langs_spoken_to_child = 31,
                  eng_to_child = 32,
                  span_to_child = 33,
                  instr_lang = 34) %>%
    dplyr::mutate(., play_child_id = stringr::str_replace(play_child_id, "/", "|")) %>%
    dplyr::select(., 1:2, 5:9, 13:15, 29:34)
  
  clean_df
}

clean_mb_cdi_eng_short <- function(df, omit_these = c(1:9)) {
  df_names <- stringr::str_split(string = names(df), pattern = "/")
  
  name_lengths <- unlist(lapply(df_names, length))
  df_names_trim <- unlist(mapply(`[`, df_names, name_lengths))
  
  # Now clean anomalous names
  df_names_trim <- df_names_trim %>%
    stringr::str_replace(., " \\([a-z ]+", "") %>% # parenthetical
    stringr::str_replace(., "\\)", "") %>% # parenthetical
    stringr::str_replace_all(., ' ', '_') %>% # spaces
    stringr::str_replace_all(., "[',]", "") %>% # apostrophe
    stringr::str_replace(., "...45", "_1") %>% # ellipsis
    stringr::str_replace(., "...49", "_2") %>% # ellipsis
    stringr::str_replace(., '...54', "") %>%
    stringr::str_replace(., "...58", "")
  
  na_names <- is.na(df_names_trim)
  df_names_trim[na_names] <- as.character(1:length(sum(na_names)))
  
  names(df) <- df_names_trim
  
  df <- df[-omit_these]
  
  df
}

clean_demog_quest <- function(df) {
  omit_cols <- c(5, 7:19, 24, 29:41, 43:44, 64:68, 81, 98:99, 
                 105:106, 125:126, 144:145, 149, 153:155, 159, 163:164, 169:170,
                 176:177, 189:228)
  
  demog_clean <- df %>%
    dplyr::rename(., start_time = start,
                  end_time = end,
                  call_date = c_today,
                  last_update = 4,
                  # expt_inst = 5,
                  recruiter_name = 6,
                  # expt_say = 7,
                  # family_phone = 8,
                  # family_email = 9,
                  # address_quest = 10,
                  # family_address_1 = 11,
                  # family_address_2 = 12,
                  # family_city = 13,
                  # family_state = 14,
                  # expt_check = 15,
                  # expt_say = 16,
                  # child_first = 17,
                  # child_middle = 18,
                  # child_last = 19,
                  child_dob = 20,
                  child_mos_check = 21,
                  child_mos_trunc = 22,
                  child_weeks_cutoff = 23,
                  # skip 24
                  child_sex = 25,
                  child_birth_city = 26,
                  play_site_id = 27,
                  site_child_id = 28,
                  # skip 29:41
                  play_child_id = 42,
                  # skip 43:44
                  lang_at_home = 45, # should parse,
                  eng_at_home = 46,
                  span_at_home = 47,
                  oth_lang_at_home = 48,
                  oth_lang_which = 49,
                  lang_mom_to_child = 50,
                  eng_mom_to_child = 51,
                  span_mom_to_child = 52,
                  oth_mom_to_child = 53,
                  oth_lang_mom_to_child_which = 54,
                  ineligible_mom_lang_continue = 55,
                  ineligible_mom_lang_recontact = 56,
                  lang_oth_to_child = 57,
                  eng_oth_to_child = 58,
                  span_oth_to_child = 59,
                  oth_oth_to_child = 60, # What languages do other household members...
                  any_oth_lang_to_child = 61,
                  ineligible_other_lang_continue = 62,
                  ineligible_other_lang_recontact = 63,
                  # skip 64:68 checks
                  know_due_date = 69,
                  born_on_term = 70,
                  ineligible_notterm_continue = 71,
                  ineligible_notterm_recontact = 72,
                  ineligible_term_unclear_continue = 73,
                  ineligible_term_unclear_recontact = 74,
                  child_due_date = 75,
                  child_birthage = 76,
                  ineligible_preterm_wks_continue = 77,
                  ineligible_preterm_wks_recontact = 78,
                  birthwt_lbs = 79,
                  birthwt_oz = 80,
                  # skip 81 bw calculator
                  ineligible_birthwt_continue = 82,
                  ineligible_birtwt_recontact = 83,
                  birth_complications_yes_no = 84,
                  birth_complications_specify = 85,
                  ineligible_complications_continue = 86,
                  ineligible_complications_recontact = 87,
                  disability_hearing = 87,
                  disability_hearing_specify = 88,
                  ineligible_hearing_continue = 89,
                  disability_vision = 90,
                  disability_vision_specify = 91,
                  ineligible_vision_continue = 92,
                  illness_injury = 93,
                  illness_injury_specify = 94,
                  ineligible_illness_injury_continue = 95,
                  child_race = 96,
                  child_ethnicity = 97,
                  # skip 98:99
                  time_to_sleep = 100,
                  time_to_wake = 101,
                  nap_hrs = 102,
                  where_sleep = 103,
                  where_sleep_specify = 104,
                  # skip 105:106
                  relations_live_with_child = 107, # Need to parse
                  biofather_live_with_child = 108,
                  male_partner_live_with_child = 109,
                  mother_bio_live_with_child = 110,
                  female_partner_live_with_child = 111,
                  other_gender_partner_live_with_child = 112,
                  grandmother_live_with_child = 113,
                  grandfather_live_with_child = 114,
                  grgrandmother_live_with_child = 115,
                  grgrandfather_live_with_child = 116,
                  aunt_live_with_child = 117,
                  uncle_live_with_child = 118,
                  cousin_live_with_child = 119,
                  oth_relative_live_with_child = 120,
                  non_relative_live_with_child = 121,
                  no_one_else_lives_with_child = 122,
                  oth_relative_relation_to_child = 123,
                  oth_non_relative_relation_to_child = 124,
                  # skip 125:126
                  caregiver_dob = 127,
                  caregiver_child_biomom = 128,
                  caregiver_relation_to_child = 129,
                  caregiver_start_care = 130,
                  caregiver_childbirth_age = 131,
                  caregiver_age_at_childbirth = 132,
                  ineligible_caregiver_young_continue = 133,
                  caregiver_race = 134,
                  caregiver_ethnicity = 135,
                  caregiver_country_born = 136,
                  caregiver_country_specify = 137,
                  caregiver_when_to_us = 138,
                  caregiver_highest_grade = 139,
                  caregiver_pd_employment = 140,
                  caregiver_occupation = 141,
                  caregiver_n_jobs = 142,
                  caregiver_job_training = 143,
                  # skip 144:145
                  know_biodad_dob = 146,
                  biodad_dob = 147,
                  biodad_childbirth_age = 148,
                  # skip 149
                  ineligible_biodad_young_continue = 150,
                  biodad_race = 151,
                  biodad_ethnicity = 152,
                  # skip 153:155
                  know_biomom_dob = 156,
                  biomom_dob = 157,
                  biomom_childbirth_age = 158,
                  # skip 159
                  ineligible_biomom_young_continue = 160,
                  biomom_race = 161,
                  biomom_ethnicity = 162,
                  # skip 163:164
                  know_partner_dob = 165,
                  partner_dob = 166,
                  partner_race = 167,
                  partner_ethnicity = 168,
                  # skip 169:170
                  partner_highest_grade = 171,
                  partner_pd_employment = 172,
                  partner_occupation = 173,
                  partner_n_jobs = 174,
                  partner_job_training = 175,
                  # skip 176:177
                  nonparental_childcare = 178,
                  childcare_inhome = 179,
                  childcare_not_inhome = 180,
                  childcare_relative = 181,
                  childcare_center = 182,
                  childcare_none = 183,
                  childcare_location = 184,
                  childcare_hrs_per_wk = 185,
                  childcare_n_children = 186,
                  childcare_age_mos_began = 187,
                  childcare_provider_lang = 188
                  # skip 189:228
    ) %>%
    # The `play_child_id` field has a "/" character in some entries. The next line fixes that.
    dplyr::mutate(., play_child_id = stringr::str_replace(play_child_id, "/", "|")) %>%
    dplyr::select(., -all_of(omit_cols))
}

extract_clean_basic_demog <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'basic_demog', this_form = this_form)
  clean_basic_demog(df)
}

extract_clean_mb_cdi_eng_short <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'mb_cdi_eng_short', this_form = this_form)
  clean_mb_cdi_eng_short(df)
}

extract_clean_demo_quest <- function(this_form = 'Demographic_Questionnaire') {
  df <- extract_obs_for_measure(this_measure = 'demog_quest', this_form = this_form)
  clean_demog_quest(df)
}
  
make_exportable_df_mb_cdi_eng_short <- function(this_form = '12_English') {
  mb <- extract_clean_mb_cdi_eng_short(this_form)
  dm <- extract_clean_basic_demog(this_form)
  cbind(dm, mb)
}

save_mbcdi_file <- function(this_row, df, csv_path = 'csv') {
  if (!is.numeric(this_row)) {
    stop('`this_row` must be numeric.')
  }
  if (!is.data.frame(df)) {
    stop('`df` must be a data frame.')
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  
  this_session <- df[this_row,]
  
  if (!is_empty(this_session)) {
    session_fn <- paste0('PLAY-visit-MBCDI-eng-short-',
                         this_session$play_site_id, "-", 
                         this_session$site_child_id, '-', 
                         this_session$play_child_id, ".csv")
    readr::write_csv(this_session, paste0(csv_path, "/", session_fn))
    message(paste0('Saved ', paste0(csv_path, "/", session_fn)))    
  } else {
   message(paste0("Session ", this_row, ' had no data')) 
  }
}

save_session_file <- function(this_row, df, file_stem = 'PLAY-visit-demog-',
                              csv_path = 'csv') {
  if (!is.numeric(this_row)) {
    stop('`this_row` must be numeric.')
  }
  if (!is.data.frame(df)) {
    stop('`df` must be a data frame.')
  }
  if (!is.character(file_stem)) {
    stop(paste0('File name stem must be character'))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  
  this_session <- df[this_row,]
  
  if (!is_empty(this_session)) {
    session_fn <- paste0(file_stem,
                         this_session$play_site_id, "-", 
                         this_session$site_child_id, '-', 
                         this_session$play_child_id, ".csv")
    readr::write_csv(this_session, paste0(csv_path, "/", session_fn))
    message(paste0('Saved ', paste0(csv_path, "/", session_fn)))    
  } else {
    message(paste0("Session ", this_row, ' had no data')) 
  }
}

export_forms_mbcdi_eng_short <- function(df, csv_path = 'csv') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'PLAY-visit-MBCDI-eng-short-', csv_path)
}

export_forms_basic_demog <- function(df, csv_path = 'csv') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'PLAY-visit-basic-demog-', csv_path)
}

export_all_forms_mbcdi_eng_short <- function(csv_path = 'csv') {
  ff <- purrr::map(c('12_English', '18_English', '24_English',
                     '12_Bilingual_Spanish',
                     '12_Bilingual_English',
                     '24_Bilingual_Spanish'), make_exportable_df_mb_cdi_eng_short)
  purrr::map(ff, export_forms_mbcdi_eng_short, csv_path)
}

export_all_forms_basic_demog <- function(csv_path = 'csv') {
  ff <- purrr::map(c('12_English', '18_English', '24_English', 
                     '12_Bilingual_Spanish', 
                     '12_Bilingual_English',
                     '24_Bilingual_Spanish'), extract_clean_basic_demog)
  purrr::map(ff, export_forms_basic_demog, csv_path)
}


