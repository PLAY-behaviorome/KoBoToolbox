# kobo helper functions

# Lists unique KB form "fragments" in the forms-measures CSV database
list_kobo_form_frags <- function(csv_fn = 'csv/.analysis/form_measures.csv') {
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
  if (length(fl) > 0) {
    unlist(fl)    
  } else {
    warning('No form found `', kobo_form_frag, '`.')
    NULL
  }
}

extract_measures_for_form <- function(this_form = '12_English', 
                                               csv_fn = 'csv/.analysis/form_measures.csv') {
  
  if (!file.exists(csv_fn)) {
    stop(paste0('Kobo form `', this_form,'` and measures file `', csv_fn, '` not found.'))
  }
  
  fm <- readr::read_csv(csv_fn)
  if (!is.data.frame(fm)) {
    warning(paste0('Cannot parse, `', csv_fn, '` into data frame.'))
    NULL
  } else {
    these_measures <- dplyr::filter(fm, kobo_form == this_form)
    dplyr::select(these_measures, measure_type, col_range)    
  }
}

extract_range_for_form_measure <- function(this_form = '12_English', this_measure = 'basic_demog') {
  require(tidyverse)
  
  these_measures <- extract_measures_for_form(this_form)
  
  this_cell_range <- these_measures %>%
    dplyr::filter(., measure_type == this_measure) 
  
  if (dim(this_cell_range)[1] == 0) {
    warning(paste0('No valid cell range for measure `', this_measure, '` in form `', this_form, '`.'))
    ""
  } else {
    this_cell_range <- dplyr::select(this_cell_range, col_range)
    as.character(this_cell_range)
  }
}

extract_obs_for_measure <- function(this_measure = 'basic_demog', this_form = '12_English') {
  require(tidyverse)
  
  these_measures <- extract_measures_for_form(this_form)
  
  this_form_fn <- return_matching_kobo_form(this_form)
  if (length(this_form_fn) > 1) {
    stop('More than one file for this form. Is it open in another program (e.g., Excel)?')
  }
  
  this_cell_range <- extract_range_for_form_measure(this_form, this_measure)

  if (this_cell_range == "") {
    warning("No valid cell range specified for form `", this_form, '` and measure `', this_measure, '`.')
    NULL
  } else {
    df <- readxl::read_excel(this_form_fn, range = readxl::cell_cols(this_cell_range), sheet = 1)
    # handle special case of empty array
    if (dim(df)[1] == 0) {
      df[1,] = NA
    }
    df    
  }
}

# Specific cleaning functions for each measure

clean_basic_demog <- function(df = extract_obs_for_measure(), omit_these = c(3:4, 10:12, 16:28, 35:37),
                              add_site_id = TRUE,
                              drop_site_name = FALSE,
                              drop_experimenter_name = FALSE) {
  require(tidyverse)
  clean_df <- df %>%
    dplyr::rename(., start_time = 1,
                  end_time = 2,
                  # omit 3, 4
                  todays_date = 5,
                  play_site_name = 6,
                  experimenter_name = 7,
                  session_date = 8,
                  site_child_id = 9,
                  # child_first = 10,
                  # child_middle = 11,
                  # child_last = 12,
                  child_dob = 13,
                  child_sex = 14,
                  child_birth_city = 15,
                  # omit 16:28
                  play_child_id = 29,
                  child_age_grp = 30,
                  langs_spoken_to_child = 31,
                  eng_to_child = 32,
                  span_to_child = 33,
                  instr_lang = 34) %>%
    dplyr::mutate(., play_child_id = stringr::str_replace(play_child_id, "/", "|")) 
  
  if (add_site_id) {
    clean_df <- clean_df %>%
      dplyr::mutate(., play_site_id = get_play_site_codes(clean_df))
  }
  
  clean_df <- clean_df[-omit_these]
  
  if (drop_site_name) {
    clean_df <- clean_df %>%
      dplyr::select(., -play_site_name)
  }
  if (drop_experimenter_name) {
    clean_df <- clean_df %>%
      dplyr::select(., -experimenter_name)
  }
  
  # Hack to fix format for one of forms
  clean_df$site_child_id <- as.numeric(clean_df$site_child_id)
  
  clean_df
}

clean_mbcdi_eng_short <- function(df = extract_obs_for_measure('mbcdi_eng_short'), omit_these = c(1:9)) {
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

clean_dll_eng_short <- function(df = extract_obs_for_measure('dll_eng_short'), omit_these = c(1:2)) {
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
  
  df <- df %>%
    dplyr::rename(., dll_eng_short_comments = 77)
  
  df <- df[-omit_these]
  
  df
}

clean_dll_span_short_12 <- function(df = extract_obs_for_measure('dll_span_short', this_form = '12_Bilingual_Spanish'), omit_these = c(1:2)) {
  
  df_names <- names(df)
  df_names_trim <- df_names %>%
    stringr::str_remove_all(., 'Combined Questionnaires \\(PLAY\\)/Home Visit Questionnaires/MacArthur CDI/12 mo/MCDI \\(12 mo\\)') %>%
    stringr::str_remove_all(., '\\(English only\\)/Short Form Supplement \\(Spanish\\)/')
  df_names_trim <- stringr::str_remove_all(df_names_trim, '^ ')
  
  names(df) <- df_names_trim
  
  df <- df %>%
    dplyr::rename(., dll_eng_short_comments = 66)
  df <- df[-omit_these]
  df
}

clean_dll_span_short_24 <- function(df = extract_obs_for_measure('dll_span_short', this_form = '24_Bilingual_Spanish'), 
                                    omit_these = c(1:3)) {
  
  df_names <- names(df)
  
  df_names_trim <- df_names %>%
    stringr::str_remove_all(., 'Combined Questionnaires \\(PLAY\\)/Home Visit Questionnaires/MacArthur CDI/24 mo/MCDI \\(24 mo\\)') %>%
    stringr::str_remove_all(., '\\(Bilingual - Spanish first\\)/Short Form Supplement \\(Spanish\\)/') %>%
    stringr::str_remove_all(., '\\(Nota\\: Seleccione todas las palabras que la madre dice que') %>%
    stringr::str_remove_all(., ' \\$\\{child_first_name\\}') %>%
    stringr::str_remove_all(., ' DICE EN ESPAÑOL\\.\\)/')
  df_names_trim <- stringr::str_remove_all(df_names_trim, '^ ')
  
  names(df) <- df_names_trim
  
  df <- df %>%
    dplyr::rename(., dll_span_short_comments = `Spanish Short Form Matched Supplement Comments`)
  df <- df[-omit_these]
  
  df
}

clean_dll_eng_long_12 <- function(df = extract_obs_for_measure('dll_eng_long', this_form = '12_English'), 
                                  omit_these = c(1:2)) {
  df_names <- names(df)
  
  df_names_trim <- df_names %>%
    stringr::str_remove_all(., 'Combined Questionnaires \\(PLAY\\)/Home Visit Questionnaires/MacArthur CDI/12 mo/MCDI \\(12 mo\\)') %>%
    stringr::str_remove_all(., '\\(English only\\)/Long Form Supplement \\(English\\)/') #%>%
  df_names_trim <- stringr::str_remove_all(df_names_trim, '^ ')
  
  names(df) <- df_names_trim
  
  df <- df[-omit_these]
  
  df
  
}

clean_dll_eng_long_18_24 <- function(df = extract_obs_for_measure('dll_eng_long', this_form = '18_English'), 
                                     omit_these = c(1:3)) {
  df_names <- names(df)
  
  df_names_trim <- df_names %>%
    stringr::str_remove_all(., 'Combined Questionnaires \\(PLAY\\)/Home Visit Questionnaires/MacArthur CDI/18 mo/MCDI \\(18 mo\\)') %>%
    stringr::str_remove_all(., 'Combined Questionnaires \\(PLAY\\)/Home Visit Questionnaires/MacArthur CDI/24 mo/MCDI \\(24 mo\\)') %>%
    stringr::str_remove_all(., '\\(English only\\)/Long Form Supplement \\(English\\)/') %>%
    stringr::str_remove_all(., '\\(Note\\: Select all the words that the mother says') %>%
    stringr::str_remove_all(., ' \\$\\{child_first_name\\}') %>%
    stringr::str_remove_all(., ' SAYS\\.\\)/') %>%
    stringr::str_remove_all(., '...[0-9]+')
    
  df_names_trim <- stringr::str_remove_all(df_names_trim, '^ ')
  
  names(df) <- df_names_trim
  
  df <- df[-omit_these]
  
  df
  
}

clean_dll_biling_long_18_24 <- function(df = extract_obs_for_measure('dll_biling_long', this_form = '24_Bilingual_Spanish'), 
                                     omit_these = c(1, 2, 7, 8)) {
  df_names <- names(df)
  
  df_names_trim <- df_names %>%
    stringr::str_remove_all(., 'Combined Questionnaires \\(PLAY\\)/Home Visit Questionnaires/MacArthur CDI/18 mo/MCDI \\(18 mo\\)') %>%
    stringr::str_remove_all(., 'Combined Questionnaires \\(PLAY\\)/Home Visit Questionnaires/MacArthur CDI/24 mo/MCDI \\(24 mo\\)') %>%
    stringr::str_remove_all(., '\\(Bilingual - Spanish first\\)/Long Form Supplement \\(English\\)/') %>%
    stringr::str_remove_all(., '\\(Note\\: Select all the words that the mother says') %>%
    stringr::str_remove_all(., ' \\$\\{child_first_name\\}') %>%
    stringr::str_remove_all(., ' SAYS\\.\\)/') %>%
    stringr::str_remove_all(., '\\"Las primeras palabras van a ser SONIDOS. ¿Alguna vez ha escuchado a decir o hacer los siguientes SONIDOS por si solo') %>%
    stringr::str_remove_all(., '/a\\?/') %>%
    stringr::str_remove_all(., '...[0-9]+')
  
  df_names_trim <- stringr::str_remove_all(df_names_trim, '^ ')
  
  names(df) <- df_names_trim
  
  df <- df[-omit_these]
  
  df
}

clean_demog_quest <- function(df = extract_obs_for_measure('demog_quest', 'Demographic_Questionnaire'),
                              omit_these = c(5, 7:19, 24, 29:41, 43:44, 64:68, 81, 98:99, 
                                             105:106, 125:126, 144:145, 149, 153:155, 159, 163:164, 169:170,
                                             176:177, 189:228)) {
  # omit_cols <- c(5, 7:19, 24, 29:41, 43:44, 64:68, 81, 98:99, 
  #                105:106, 125:126, 144:145, 149, 153:155, 159, 163:164, 169:170,
  #                176:177, 189:228)
  
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
                  play_site_name = 27,
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
    dplyr::select(., -all_of(omit_these))
}

clean_health <- function(df, omit_non_answers = TRUE) {
  require(tidyverse) # for pipe
  
  if (!is.data.frame(df)) {
    stop('`df` must be data frame')
  }
  if (is.null(df)) {
    NULL
  }
  
  omit_cols = c(1, 14, 33, 43, 60:62)
  
  health_clean <- df %>%
    dplyr::rename(., breastfeed_ever = 2,
                  breastfeed_mos = 3,
                  how_fed_last_7d = 4,
                  how_fed_last_7d_breastfed = 5,
                  how_fed_last_7d_formula = 6,
                  how_fed_last_7d_cows_milk = 7,
                  how_fed_last_7d_refused = 8,
                  how_fed_last_7d_dont_know = 9,
                  how_fed_last_7d_nondairy = 10,
                  how_fed_last_7d_none_above = 11,
                  age_mos_1st_solid_food = 12,
                  ecls_b_a_nutrition_comments = 13,
                  # skip 14
                  sleep_position = 15,
                  health_rating = 16,
                  age_mos_last_wellbaby = 17,
                  vaccinated_last_4w = 18,
                  specialist_care = 19,
                  specialist_care_specify = 20,
                  hearing_tested = 21,
                  hearing_tested_birth_hosp = 22,
                  hearing_tested_after_home = 23,
                  hearing_tested_no = 24,
                  hearing_tested_refused = 25,
                  hearing_tested_dont_know = 26,
                  vision_tested = 27,
                  vision_tested_birth_hosp = 28,
                  vision_tested_after_home = 29,
                  vision_tested_no = 30,
                  vision_tested_refused = 31,
                  vision_tested_dont_know = 32,
                  # skip 33
                  food_med_allergies = 34,
                  ear_infection = 35,
                  asthma = 36,
                  respiratory_illness = 37,
                  gi_illness = 38,
                  eclsb_a_genl_health_injury_comments = 39,
                  injury_n_times = 40,
                  injury_describe = 41,
                  eclsb_a_genl_health_comments = 42,
                  # skip 43
                  prenatal_care = 44,
                  prenatal_care_comments = 45,
                  smoke_while_pregnant = 46,
                  smoke_packs_daily_1st_tri = 47,
                  smoke_packs_daily_2nd_tri = 48,
                  smoke_packs_daily_3rd_tri = 49,
                  smoke_now = 50,
                  smoke_packs_daily_now = 51,
                  smoking_in_house = 52,
                  smoking_in_car = 53,
                  ecls_b_a_smoking_comments = 54,
                  drink_while_pregnant = 55,
                  drinks_weekly_1st_tri = 56,
                  drinks_weekly_2nd_tri = 57,
                  drinks_weekly_3rd_tri = 58,
                  eclsb_a_drinking_comments = 59,
                  # skip 60:62
                  # These are text + numeric; might want to recode
                  feel_anxious = 63,
                  cant_stop_worrying = 64,
                  little_interest_pleasure = 65,
                  feel_depressed = 66,
                  ecls_b_f_phq_comments = 67
     )
                  
  if (omit_non_answers) {
    health_clean <- dplyr::select(health_clean, -all_of(omit_cols))
  }
  health_clean
}

clean_ecbq <- function(df, omit_non_answers = TRUE) {
    require(tidyverse) # for pipe
    
    if (!is.data.frame(df)) {
      stop('`df` must be data frame')
    }
    if (is.null(df)) {
      NULL
    }
    
    omit_cols = c(1:3)
    
    ecbq_clean <- df %>%
      dplyr::rename(., cling_on_stranger_approach = 4,
                    easily_irritated = 5,
                    seek_familiar_child = 6,
                    decide_quickly = 7,
                    enjoy_sung_to = 8,
                    take_chances = 9,
                    play_10_mins = 10,
                    respond_while_engaged = 11,
                    excited_abt_visit = 12,
                    fiddle_w_hair = 13,
                    like_rowdy = 14,
                    hug_eager_getaway = 15,
                    get_involved_new = 16,
                    tire_quickly = 17,
                    pay_attention_when_called = 18,
                    irritated_by_clothes = 19,
                    bothered_by_noise = 20,
                    full_of_energy = 21,
                    fear_noisy_vehicles = 22,
                    most_active = 23,
                    stop_forbidden = 24,
                    no_sadly_fearful = 25,
                    down_blue = 26,
                    run_indoors = 27,
                    excitement_new_toy = 28,
                    tantrum = 29,
                    wait_patiently = 30,
                    smile_when_rocked = 31,
                    mold_when_held =32,
                    interact_w_fam_adult = 33,
                    care_with_breakable = 34,
                    not_enter_new_place = 35,
                    cry_3mins = 36,
                    easily_soothed = 37,
                    find_other_activity = 38,
                    enjoy_diff_people = 39,
                    ebcq_comments = 40
                    )
      
    if (omit_non_answers) {
      ecbq_clean <- dplyr::select(ecbq_clean, -all_of(omit_cols))
    }
    ecbq_clean
}

clean_media_use <- function(df, omit_non_answers = TRUE) {
  require(tidyverse) # for pipe
  
  if (!is.data.frame(df)) {
    stop('`df` must be data frame')
  }
  if (is.null(df)) {
    NULL
  }
  
  omit_cols = c(1, 25:26)  
  
  media_clean <- df %>%
    dplyr::rename(., 
                  have_devices = 2,
                  have_tv = 3,
                  have_dvd = 4,
                  have_computer = 5,
                  have_smartphone = 6,
                  have_tablet = 7,
                  have_ed_game = 8,
                  have_game_console = 9,
                  child_used_tv = 10,
                  how_use_tv = 11,
                  child_used_dvd = 12,
                  how_use_dvd = 13,
                  child_use_computer = 14,
                  how_use_computer = 15,
                  child_use_smartphone = 16,
                  how_use_smartphone = 17,
                  child_use_tablet = 18,
                  how_use_tablet = 19,
                  child_use_ed_game = 20,
                  how_use_ed_game = 21,
                  child_use_game_console = 22,
                  how_use_game_console = 23,
                  tv_on_no_watching = 24,
                  # skip 25-26,
                  use_device_meals = 27,
                  use_device_playtime = 28,
                  use_device_bedtime = 29,
                  use_device_travel = 30,
                  media_use_comments = 31)
  
  if (omit_non_answers) {
    media_clean <- dplyr::select(media_clean, -all_of(omit_cols))
  }
  media_clean  
}

clean_pets <- function(df, omit_non_answers = TRUE) {
  require(tidyverse) # for pipe
  
  if (!is.data.frame(df)) {
    stop('`df` must be data frame')
  }
  if (is.null(df)) {
    NULL
  }
  
  omit_cols = c()  
  
  pets_clean <- df %>%
    dplyr::rename(., 
                  pets_at_home = 1,
                  type_number = 2,
                  where_live = 3,
                  pets_comments = 4)
  
  if (omit_non_answers) {
    pets_clean <- dplyr::select(pets_clean, -all_of(omit_cols))
  }
  pets_clean  
}

clean_household_labor <- function(df, omit_non_answers = TRUE) {
  require(tidyverse) # for pipe
  
  if (!is.data.frame(df)) {
    stop('`df` must be data frame')
  }
  if (is.null(df)) {
    NULL
  }
  
  omit_cols = c(1:4, 9, 14, 19, 24, 29, 34, 39)  
  
  household_labor_clean <- df %>%
    dplyr::rename(.,
                  # Skip 1:4
                  laundry_caregiver = 5,
                  laundry_partner = 6,
                  laundry_other = 7,
                  laundry_who_else = 8,
                  # skip 9
                  cleaning_caregiver = 10,
                  cleaning_partner = 11,
                  cleaning_other = 12,
                  cleaning_who_else = 13,
                  # skip 14
                  dishes_caregiver = 15,
                  dishes_partner = 16,
                  dishes_other = 17,
                  dishes_who_else = 18,
                  # skip 19
                  cooking_caregiver = 20,
                  cooking_partner = 21,
                  cooking_other = 22,
                  cooking_who_else = 23,
                  # skip 24
                  feed_child_caregiver = 25,
                  feed_child_partner = 26,
                  feed_child_other = 27,
                  feed_child_who_else = 28,
                  # skip 29
                  dropoff_child_caregiver = 30,
                  dropoff_child_partner = 31,
                  dropoff_child_other = 32,
                  dropoff_child_who_else = 33,
                  # skip 34
                  to_bed_child_caregiver = 35,
                  to_bed_child_partner = 36,
                  to_bed_child_other = 37,
                  to_bed_child_who_else = 38,
                  # skip 39
                  discipline_child_caregiver = 40,
                  discipline_child_partner = 41,
                  discipline_child_other = 42,
                  discipline_child_who_else = 43,
                  household_labor_comments = 44)
  
  if (omit_non_answers) {
    household_labor_clean <- dplyr::select(household_labor_clean, -all_of(omit_cols))
  }
  household_labor_clean  
}

clean_typical_day <- function(df, omit_non_answers = TRUE) {
  require(tidyverse) # for pipe
  
  if (!is.data.frame(df)) {
    stop('`df` must be data frame')
  }
  if (is.null(df)) {
    NULL
  }
  
  omit_cols = c(9)  
  
  typical_day_clean <- df %>%
    dplyr::rename(., 
                  typical_day = 1,
                  day_how_different = 2,
                  activities_similar = 3,
                  activities_different = 4,
                  typical_night_morning = 5,
                  night_morning_how_different = 6,
                  unusual_feelings = 7,
                  feelings_how_different = 8)
                  # Skip 9)
  
  if (omit_non_answers) {
    typical_day_clean <- dplyr::select(typical_day_clean, -all_of(omit_cols))
  }
  typical_day_clean  
}

#-------------------------------------------------------------------
# Clean questions

# Clean KoBoToolbox headers to create a nice list of questions.
clean_qs_basic_demog <- function(df) {
  require(tidyverse)
  
  df_names <- stringr::str_split(string = names(df), pattern = "/")
  
  omit_qs <- c(3:4, 35:37)
  
  name_lengths <- unlist(lapply(df_names, length))
  df_names_trim_1 <- unlist(mapply(`[`, df_names, 1))
  # Looks like item 2 contains most of the question info
  df_names_trim_2 <- unlist(mapply(`[`, df_names, 2))
  df_names_trim_3 <- unlist(mapply(`[`, df_names, 3))
  
  qs <- tibble::tibble(question = paste(df_names_trim_1, df_names_trim_2, df_names_trim_3, sep = " "))
  qs <- qs %>%
    dplyr::mutate(., question = stringr::str_remove(question, "Combined Questionnaires \\(PLAY\\)")) %>%
    dplyr::mutate(., question = stringr::str_remove(question, "^ ")) %>%
    dplyr::mutate(., question = stringr::str_remove(question, " $")) %>%
    dplyr::mutate(., question = stringr::str_remove(question, "\\:")) %>%
    dplyr::mutate(., question = stringr::str_remove(question, "NA "))
  
  # Seems not to work if this final str_remove() is part of the above chain
  qs <- qs %>%
    dplyr::mutate(., question = stringr::str_remove(question, " NA$"))
    
  qs <- qs[-omit_qs,]
  l_qs <- dim(qs)[1]
  qs <- qs %>%
    dplyr::mutate(., q_index = 1:l_qs) %>%
    dplyr::select(q_index, question)
  qs
}

clean_qs_pets <- function(df) {
  require(tidyverse)
  
  df_names <- stringr::str_split(string = names(df), pattern = "/")
  
  omit_qs <- c()
  df_names_trim_4 <- unlist(mapply(`[`, df_names, 4))
  
  qs <- tibble::tibble(question = df_names_trim_4)
  qs <- qs %>%
    dplyr::mutate(., question = stringr::str_remove_all(question, '\\"')) %>%
    dplyr::mutate(., question = stringr::str_remove(question, '\\:'))

  # qs <- qs[-omit_qs,]
  l_qs <- dim(qs)[1]
  qs <- qs %>%
    dplyr::mutate(., q_index = 1:l_qs) %>%
    dplyr::select(q_index, question)
  qs
}

clean_qs_typical_day <- function(df) {
  require(tidyverse)
  
  df_names <- stringr::str_split(string = names(df), pattern = "/")
  df_names
  
  omit_qs <- c(9)
  df_names_trim_4 <- unlist(mapply(`[`, df_names, 4))
  df_names_trim_5 <- unlist(mapply(`[`, df_names, 5))

  qs <- tibble::tibble(question = paste(df_names_trim_4, df_names_trim_5, sep = " "))
  qs <- qs %>%
    dplyr::mutate(., question = stringr::str_remove_all(question, '\\"')) %>%
    dplyr::mutate(., question = stringr::str_remove(question, '\\:')) %>%
    dplyr::mutate(., question = stringr::str_remove_all(question, " NA"))

  qs <- qs[-omit_qs,]
  l_qs <- dim(qs)[1]
  qs <- qs %>%
    dplyr::mutate(., q_index = 1:l_qs) %>%
    dplyr::select(q_index, question)
  qs
}

#-------------------------------------------------------------------
# Extract and clean questions

extract_clean_qs_basic_demog <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'basic_demog', this_form = this_form)
  clean_qs_basic_demog(df)
}

extract_clean_qs_pets <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'pets', this_form = this_form)
  clean_qs_pets(df)
}

extract_clean_qs_typical_day <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'typical_day', this_form = this_form)
  clean_qs_typical_day(df)
}

#-------------------------------------------------------------------
# Extract and clean by measure, given form

extract_clean_basic_demog <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'basic_demog', this_form = this_form)
  df <- clean_basic_demog(df)
  # if (drop_site_info) {
  #   df <- df %>%
  #     dplyr::select(., -play_site_name)
  # }
  df
}

extract_clean_mbcdi_eng_short <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'mbcdi_eng_short', this_form = this_form)
  clean_mbcdi_eng_short(df)
}

extract_clean_dll_eng_short <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'dll_eng_short', this_form = this_form)
  clean_dll_eng_short(df)
}

extract_clean_dll_span_short <- function(this_form = '12_Bilingual_Spanish') {
  
  df <- extract_obs_for_measure(this_measure = 'dll_span_short', this_form = this_form)
  if (this_form == '12_Bilingual_Spanish' || this_form == '12_Bilingual_English') {
    if (!is.null(df)) {
      clean_dll_span_short_12(df, omit_these = c(1,2))      
    } else {
      warning('Cannot extract `dll_span_short` for form `', this_form, '`.')
      NULL
    }
  } else if (this_form == '18_Bilingual_Spanish' || this_form == '12_Bilingual_English') {
    
  } else if (this_form == '24_Bilingual_Spanish' || this_form == '24_Bilingual_English') {
    if (!is.null(df)) {
      clean_dll_span_short_24(df, omit_these = c(1:3))      
    } else {
      warning('Cannot extract `dll_span_short` for form `', this_form, '`.')
      NULL
    }
  } else {
    warning(paste0('Invalid form designation ', this_form))
    NULL
  }
}

extract_clean_dll_eng_long <- function(this_form = '12_English') {
  
  df <- extract_obs_for_measure(this_measure = 'dll_eng_long', this_form = this_form)
  if (this_form == '12_English') {
    if (!is.null(df)) {
      clean_dll_eng_long_12(df, omit_these = c(1,2))      
    } else {
      warning('Cannot extract `dll_eng_long` for form `', this_form, '`.')
      NULL
    }
  } else if (this_form == '18_English' || this_form == '24_English') {
    if (!is.null(df)) {
      clean_dll_eng_long_18_24(df, omit_these = c(1:3))      
    } else {
      warning('Cannot extract `dll_eng_long` for form `', this_form, '`.')
      NULL
    }
  } else {
    warning(paste0('No DLL English Long measure for form `', this_form, '`.'))
    NULL
  }
}

extract_clean_dll_biling_long <- function(this_form = '24_Bilingual_Spanish') {
  
  df <- extract_obs_for_measure(this_measure = 'dll_biling_long', this_form = this_form)
  if (this_form == '12_Bilingual_Spanish') {
    if (!is.null(df)) {
      clean_dll_biling_long_12(df, omit_these = c(1,2))      
    } else {
      warning('Cannot extract `dll_biling_long` for form `', this_form, '`.')
      NULL
    }
  } else if (this_form == '18_Bilingual_Spanish' || this_form == '24_Bilingual_Spanish') {
    if (!is.null(df)) {
      clean_dll_biling_long_18_24(df, omit_these = c(1, 2, 7, 8))      
    } else {
      warning('Cannot extract `dll_biling_long` for form `', this_form, '`.')
      NULL
    }
  } else {
    warning(paste0('No DLL English Long measure for form `', this_form, '`.'))
    NULL
  }
}

extract_clean_demo_quest <- function(this_form = 'Demographic_Questionnaires') {
  df <- extract_obs_for_measure(this_measure = 'demog_quest', this_form = this_form)
  clean_demog_quest(df)
}

extract_clean_health <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'health', this_form = this_form)
  clean_health(df)
}

extract_clean_ecbq <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'ecbq_very_short', this_form = this_form)
  clean_ecbq(df)
}

extract_clean_media <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'media_use', this_form = this_form)
  clean_media_use(df)
}

extract_clean_pets <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'pets', this_form = this_form)
  clean_pets(df)
}

extract_clean_household_labor <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'household_labor', this_form = this_form)
  clean_household_labor(df)
}

extract_clean_typical_day <- function(this_form = '12_English') {
  df <- extract_obs_for_measure(this_measure = 'typical_day', this_form = this_form)
  clean_typical_day(df)
}

#-------------------------------------------------------------------
# Make exportable data frames with reference demographics

make_exportable_mbcdi_eng_short <- function(this_form = '12_English') {
  dm <- extract_clean_mbcdi_eng_short(this_form)
  hd <- extract_clean_basic_demog(this_form)
  if (dim(dm)[1] == dim(hd)[1]) {
    cbind(dm, hd)    
  } else {
    warning(paste0("MBCDI English short and demographics do not align for form ", this_form))
    NULL
  }
}

make_exportable_dll_eng_short <- function(this_form = '12_English') {
  dm <- extract_clean_basic_demog(this_form)
  hd <- extract_clean_dll_eng_short(this_form)
  
  if (dim(dm)[1] == dim(hd)[1]) {
    cbind(dm, hd)    
  } else {
    warning(paste0("DLL English Short and demographics do not align for form ", this_form))
    NULL
  }
}

make_exportable_dll_span_short <- function(this_form = '12_Bilingual_Spanish') {
  dm <- extract_clean_basic_demog(this_form)
  hd <- extract_clean_dll_span_short(this_form)
  
  if (dim(dm)[1] == dim(hd)[1]) {
    cbind(dm, hd)    
  } else {
    warning(paste0("DLL Spanish Short and demographics do not align for form ", this_form))
    NULL
  }
}

make_exportable_dll_biling_long <- function(this_form = '24_Bilingual_Spanish') {
  dm <- extract_clean_basic_demog(this_form)
  hd <- extract_clean_dll_span_short(this_form)
  
  if (dim(dm)[1] == dim(hd)[1]) {
    cbind(dm, hd)    
  } else {
    warning(paste0("DLL Bilingual Long and demographics do not align for form ", this_form))
    NULL
  }
}

make_exportable_health <- function(this_form = '12_English') {
  dm <- extract_clean_basic_demog(this_form)
  hd <- extract_clean_health(this_form)
  
  if (dim(dm)[1] == dim(hd)[1]) {
    cbind(dm, hd)    
  } else {
    warning(paste0("Health data and demographics do not align for form ", this_form))
    NULL
  }
}

make_exportable_ecbq <- function(this_form = '12_English') {
  dm <- extract_clean_basic_demog(this_form)
  df <- extract_clean_ecbq(this_form)
  
  if (dim(dm)[1] == dim(df)[1]) {
    cbind(dm, df)    
  } else {
    warning(paste0("ECBQ data and demographics do not align for form ", this_form))
    NULL
  }
}

make_exportable_media <- function(this_form = '12_English') {
  dm <- extract_clean_basic_demog(this_form)
  df <- extract_clean_media(this_form)
  
  if (dim(dm)[1] == dim(df)[1]) {
    cbind(dm, df)    
  } else {
    warning(paste0("Media use data and demographics do not align for form ", this_form))
    NULL
  }
}

make_exportable_pets <- function(this_form = '12_English') {
  dm <- extract_clean_basic_demog(this_form)
  df <- extract_clean_pets(this_form)
  
  if (dim(dm)[1] == dim(df)[1]) {
    cbind(dm, df)    
  } else {
    warning(paste0("Pets data and demographics do not align for form ", this_form))
    NULL
  }
}

make_exportable_household_labor <- function(this_form = '12_English') {
  dm <- extract_clean_basic_demog(this_form)
  df <- extract_clean_household_labor(this_form)
  
  if (dim(dm)[1] == dim(df)[1]) {
    cbind(dm, df)    
  } else {
    warning(paste0("Household labor data and demographics do not align for form ", this_form))
    NULL
  }
}

make_exportable_typical_day <- function(this_form = '12_English') {
  dm <- extract_clean_basic_demog(this_form)
  df <- extract_clean_typical_day(this_form)
  
  if (dim(dm)[1] == dim(df)[1]) {
    cbind(dm, df)    
  } else {
    warning(paste0("Typical day data and demographics do not align for form ", this_form))
    NULL
  }
}

#-------------------------------------------------------------------
# File saving functions

# save_mbcdi_file <- function(this_row, df, csv_path = 'csv') {
#   if (!is.numeric(this_row)) {
#     stop('`this_row` must be numeric.')
#   }
#   if (!is.data.frame(df)) {
#     stop('`df` must be a data frame.')
#   }
#   if (!dir.exists(csv_path)) {
#     stop(paste0('CSV path not found: ', csv_path))
#   }
#   
#   this_session <- df[this_row,]
#   
#   if (!is_empty(this_session)) {
#     session_fn <- paste0('PLAY-visit-MBCDI-eng-short-',
#                          this_session$play_site_name, "-", 
#                          this_session$site_child_id, '-', 
#                          this_session$play_child_id, ".csv")
#     readr::write_csv(this_session, paste0(csv_path, "/", session_fn))
#     message(paste0('Saved ', paste0(csv_path, "/", session_fn)))    
#   } else {
#    message(paste0("Session ", this_row, ' had no data')) 
#   }
# }

save_session_file <- function(this_row, df, file_stem = 'file',
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
    session_fn <- paste0(this_session$play_site_id, '_', file_stem, '.csv')
    # session_fn <- paste0(file_stem,
    #                      this_session$play_site_name, "-", 
    #                      this_session$site_child_id, '-', 
    #                      this_session$play_child_id, ".csv")
    readr::write_csv(this_session, paste0(csv_path, "/", session_fn))
    message(paste0('Saved ', paste0(csv_path, "/", session_fn)))    
  } else {
    message(paste0("Session ", this_row, ' had no data')) 
  }
}

#-------------------------------------------------------------------
# Export forms given aggregate data frame for measure

export_forms_basic_demog <- function(df = extract_clean_basic_demog(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, file_stem = 'basic_demog', csv_path)
  #readr::write_csv(df, paste0('csv/aggregate/', 'PLAY_all_basic_demog.csv'))
}

export_forms_mbcdi_eng_short <- function(df = make_exportable_mbcdi_eng_short(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'MBCDI_eng_short', csv_path)
  #readr::write_csv(df, paste0('csv/aggregate/', 'PLAY_all_MBCDI_eng_short.csv'))
}

export_forms_dll_eng_short <- function(df = make_exportable_dll_eng_short(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'DLL_eng_short', csv_path)
  #readr::write_csv(df, paste0('csv/aggregate/', 'PLAY_all_DLL_eng_short.csv'))
}

export_forms_dll_span_short <- function(df = make_exportable_dll_span_short(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'DLL_span_short', csv_path)
  #readr::write_csv(df, paste0('csv/aggregate/', 'PLAY_all_DLL_span_short.csv'))
}

export_forms_dll_biling_long <- function(df = make_exportable_dll_biling_long(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'DLL_biling_long', csv_path)
  #readr::write_csv(df, paste0('csv/aggregate/', 'PLAY_all_DLL_span_short.csv'))
}

export_forms_health <- function(df = make_exportable_health(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'health', csv_path)
  #readr::write_csv(df, paste0('csv/aggregate/', 'PLAY_all_health.csv'))
}

export_forms_ebcq <- function(df = make_exportable_ecbq(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'ECBQ', csv_path)
  #readr::write_csv(df, paste0('csv/aggregate/', 'PLAY_all_ECBQ.csv'))
}

export_forms_media <- function(df = make_exportable_media(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'media', csv_path)
  #readr::write_csv(df, paste0('csv/aggregate/', 'PLAY_all_media.csv'))
}

export_forms_pets <- function(df = make_exportable_pets(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'pets', csv_path)
  #readr::write_csv(df, paste0('csv/aggregate/', 'PLAY_all_pets.csv'))
}

export_forms_household_labor <- function(df = make_exportable_household_labor(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'household_labor', csv_path)
  #readr::write_csv(df, paste0('csv/aggregate/', 'PLAY_all_household_labor.csv'))
}

export_forms_typical_day <- function(df = make_exportable_typical_day(), csv_path = 'csv/by_session') {
  if (!is.data.frame(df)) {
    stop(paste0('`df` must be a data frame' ))
  }
  if (!dir.exists(csv_path)) {
    stop(paste0('CSV path not found: ', csv_path))
  }
  file_list <- 1:dim(df)[1]
  purrr::map(file_list, save_session_file, df, 'typical_day', csv_path)
  #readr::write_csv(df, paste0('csv/by_form/', 'PLAY_all_typical_day.csv'))
}

#-------------------------------------------------------------------
# Export all forms for given measure

export_all_forms_mbcdi_eng_short <- function(these_forms = c('12_English', '18_English', '24_English',
                                                             '12_Bilingual_Spanish',
                                                             '12_Bilingual_English',
                                                             '24_Bilingual_Spanish'),
                                             csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, make_exportable_mbcdi_eng_short)
  purrr::map(ff, export_forms_mbcdi_eng_short, csv_path)
}

export_all_forms_dll_eng_short <- function(these_forms = c('12_English', '18_English', '24_English',
                                                           '12_Bilingual_Spanish',
                                                           '12_Bilingual_English',
                                                           '24_Bilingual_Spanish'),
                                           csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, make_exportable_dll_eng_short)
  purrr::map(ff, export_forms_dll_eng_short, csv_path)
}

export_all_forms_dll_span_short <- function(these_forms = c('12_Bilingual_Spanish',
                                                            '12_Bilingual_English',
                                                            '24_Bilingual_Spanish'),
                                            csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, make_exportable_dll_span_short)
  purrr::map(ff, export_forms_dll_span_short, csv_path)
}

export_all_forms_dll_biling_long <- function(these_forms = c('12_Bilingual_Spanish',
                                                            '12_Bilingual_English',
                                                            '24_Bilingual_Spanish'),
                                            csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, make_exportable_dll_biling_long)
  purrr::map(ff, export_forms_dll_biling_long, csv_path)
}

export_all_forms_basic_demog <- function(these_forms = c('12_English', '18_English', '24_English', 
                                                         '12_Bilingual_Spanish', 
                                                         '12_Bilingual_English',
                                                         '24_Bilingual_Spanish'),
                                         csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, extract_clean_basic_demog)
  purrr::map(ff, export_forms_basic_demog, csv_path)
}

export_all_forms_health <- function(these_forms = c('12_English', '18_English', '24_English', 
                                                    '12_Bilingual_Spanish', 
                                                    '12_Bilingual_English',
                                                    '24_Bilingual_Spanish'),
                                    csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, make_exportable_health)
  purrr::map(ff, export_forms_health, csv_path)
}

export_all_forms_ecbq <- function(these_forms = c('12_English', '18_English', '24_English', 
                                                  '12_Bilingual_Spanish', 
                                                  '12_Bilingual_English',
                                                  '24_Bilingual_Spanish'),
                                  csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, make_exportable_ecbq)
  purrr::map(ff, export_forms_ebcq, csv_path)
}

export_all_forms_media <- function(these_forms = c('12_English', '18_English', '24_English', 
                                                   '12_Bilingual_Spanish', 
                                                   '12_Bilingual_English',
                                                   '24_Bilingual_Spanish'),
                                   csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, make_exportable_media)
  purrr::map(ff, export_forms_media, csv_path)
}

export_all_household_labor <- function(these_forms = c('12_English', '18_English', '24_English', 
                                                       '12_Bilingual_Spanish', 
                                                       '12_Bilingual_English',
                                                       '24_Bilingual_Spanish'),
                                       csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, make_exportable_household_labor)
  purrr::map(ff, export_forms_household_labor, csv_path)
}

export_all_forms_pets <- function(these_forms = c('12_English', '18_English', '24_English', 
                                                  '12_Bilingual_Spanish', 
                                                  '12_Bilingual_English',
                                                  '24_Bilingual_Spanish'),
                                  csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, make_exportable_pets)
  purrr::map(ff, export_forms_pets, csv_path)
}

export_all_forms_typical_day <- function(these_forms = c('12_English', '18_English', '24_English', 
                                                         '12_Bilingual_Spanish', 
                                                         '12_Bilingual_English',
                                                         '24_Bilingual_Spanish'),
                                         csv_path = 'csv/by_session') {
  ff <- purrr::map(these_forms, make_exportable_typical_day)
  purrr::map(ff, export_forms_typical_day, csv_path)
}

#-------------------------------------------------------------------
# Export all forms for all measures

export_all_measures_all_forms <- function() {
  export_all_forms_basic_demog()
  export_all_forms_mbcdi_eng_short()
  export_all_forms_dll_eng_short()
  export_all_forms_dll_span_short()
  export_all_forms_dll_biling_long()
  export_all_forms_health()
  export_all_forms_ecbq()
  export_all_forms_media()
  export_all_household_labor()
  export_all_forms_pets()
  export_all_forms_typical_day()
}

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
      df <- extract_clean_form_measure(this_form, this_measure)
      if (save_regenerated) {
        readr::write_csv(df, this_fn)
        message("  Saved file `", this_fn, '`.')
      }
      df
    }
  } else {
    if (force_regeneration) {
      message("  Regenerating data for form `", this_form, '` and measure `', this_measure, '`.')
      df <- extract_clean_form_measure(this_form, this_measure)
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

extract_clean_form_measure <- function(this_form = '12_English', this_measure = 'basic_demog') {
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
  if (this_measure == 'dll_biling_long') {
    df <- extract_clean_dll_biling_long(this_form)
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
  require(tidyverse)
  
  this_play_session_id %>%
    stringr::str_remove(., stringr::str_extract(this_play_session_id, '[0-9]{5}$')) %>%
    stringr::str_remove(., 'PLAY_')  
}

load_databrary_session <- function(this_play_session_id = 'PLAY_89938196') {
  require(tidyverse)
  
  if (stringr::str_detect(this_play_session_id, 'NODB')) {
    warning('No Databrary session for `', this_play_session_id, '`.')
    return(NULL)
  }
  
  this_session <- session_from_play_id(this_play_session_id)
  this_vol <- vol_from_play_id(this_play_session_id)
  db_sessions <- databraryapi::download_session_csv(vol_id = as.numeric(this_vol))
  
  if (is.null(db_sessions)) {
    message('Cannot access Databrary session for `', this_play_session_id, '`. Are you logged in to Databrary?' )
    NULL
  } else {
    this_db_session <- db_sessions %>%
      dplyr::filter(., vol_id == this_vol,
                    session_id == this_session)
    this_db_session    
  }
}

# compare Kb data to that on Databrary

kb_eq_db_dob_by_id <- function( this_play_session_id = 'PLAY_89938196', this_form = '18_English') {
  db_df <- load_databrary_session(this_play_session_id)
  kb_df <- find_session_in_kbform(this_form, this_session = session_from_play_id(this_play_session_id),
                                  this_vol = vol_from_play_id(this_play_session_id))
  kb_df$child_dob == db_df$participant.birthdate
}

kb_eq_db_dob <- function(this_form = '18_English', this_play_session_id = 'PLAY_89938196') {
  db_df <- load_databrary_session(this_play_session_id)
  kb_df <- find_session_in_kbform(this_form, this_session = session_from_play_id(this_play_session_id),
                                  this_vol = vol_from_play_id(this_play_session_id))
  kb_df$child_dob == db_df$participant.birthdate
}

kb_eq_db_session_date_by_id <- function(this_play_session_id = 'PLAY_89938196', this_form = '18_English') {
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
  
  tolower(kb_df$child_sex) == tolower(db_df$participant.gender)
}

kb_eq_db_sex_by_id <- function(this_play_session_id = 'PLAY_89938196', this_form = '18_English') {
  db_df <- load_databrary_session(this_play_session_id)
  kb_df <- find_session_in_kbform(this_form, this_session = session_from_play_id(this_play_session_id),
                                  this_vol = vol_from_play_id(this_play_session_id))
  
  tolower(kb_df$child_sex) == tolower(db_df$participant.gender)
}
