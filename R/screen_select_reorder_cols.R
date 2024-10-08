screen_select_reorder_cols <- function(df) {
  stopifnot(is.data.frame(df))
  
  df |>
    dplyr::rename(submit_date = start) |>
    dplyr::select(submit_date,
                  site_id,
                  subject_number,
                  play_id,
                  child_age_mos,
                  child_sex,
                  child_bornonduedate,
                  child_onterm,
                  child_birthage,
                  child_weight_pounds,
                  child_weight_ounces,
                  child_birth_complications,
                  child_birth_complications_specify,
                  child_hearing_disabilities,                
                  child_hearing_disabilities_specify,        
                  child_vision_disabilities,                 
                  child_vision_disabilities_specify,
                  child_major_illnesses_injuries,         
                  child_illnesses_injuries_specify,
                  child_developmentaldelays,           
                  child_developmentaldelays_specify,
                  child_sleep_time,
                  child_wake_time,
                  child_nap_hours,
                  child_sleep_location,
                  mom_bio,
                  mom_childbirth_age,
                  mom_race,
                  mom_birth_country,
                  mom_birth_country_specify,
                  mom_education,
                  mom_employment,
                  mom_occupation,
                  mom_jobs_number,
                  mom_training,
                  biodad_childbirth_age,
                  biodad_race,
                  contains("language_spoken"),
                  contains("childcare_"))
}
