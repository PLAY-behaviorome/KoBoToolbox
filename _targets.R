# _targets.R

library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("readr", "dplyr", "ggplot2", "purrr", "tools",
                            "httr", "stringr"))
update_interval_days <- 3
list(
  tar_target(clean_home_visit, FALSE),
  tar_target(clean_screening, FALSE),
  tar_target(
    clean_home_visit_data,
    clean_data_dir(clean_home_visit, home_visit_dir_xlsx)
  ),
  tar_target(
    clean_demog_data,
    clean_data_dir(clean_screening, screening_dir_xlsx)
  ),
  tar_target(kb_df, list_kobo_data()),
  tar_target(kb_demog, dplyr::filter(
    kb_df, stringr::str_detect(title, "Demographic")
  )),
  tar_target(screening_dir_xlsx, "data/xlsx/screening"),
  tar_target(screening_dir_csv, "data/csv/screening"),
  tar_target(
    screening_downloads,
    retrieve_kobo_xlsx(kb_demog, screening_dir_xlsx),
    cue = tarchetypes::tar_cue_age(name = screening_downloads, age = as.difftime(update_interval_days, units = "days"))
  ),
  tar_target(
    screening_xlsx_fns,
    list.files(
      screening_dir_xlsx,
      pattern = "[0-9]+.*\\.xlsx",
      full.names = TRUE
    )
  ),
  tar_target(
    screening_csv_fns,
    list.files(screening_dir_csv, pattern = "[0-9]+.*\\.csv", full.names = TRUE)
  ),
  # tar_target(screening_1, file.path(screening_dir_xlsx, "275882_PLAY_Demographic_Questionnaire.xlsx"),
  #            format = "file"),
  # tar_target(screening_2, file.path(screening_dir_xlsx, "334134_PLAY_Demographic_Questionnaire_Spanish.xlsx"),
  #            format = "file"),
  # tar_target(screening_3, file.path(screening_dir_xlsx, "359546_PLAY_Demographic_Questionnaire.xlsx"),
  #            format = "file"),
  # tar_target(screening_1_csv, file.path(screening_dir_csv, "275882_PLAY_Demographic_Questionnaire.csv"),
  #            format = "file"),
  # tar_target(screening_2_csv, file.path(screening_dir_csv, "334134_PLAY_Demographic_Questionnaire_Spanish.csv"),
  #            format = "file"),
  # tar_target(screening_3_csv, file.path(screening_dir_csv, "359546_PLAY_Demographic_Questionnaire.csv"),
  #            format = "file"),
  tar_target(
    screening_xlsx_to_csv,
    load_xlsx_save_many_csvs(screening_dir_xlsx, screening_dir_csv, "Demographic")
  ),
  tar_target(demog_submissions, clean_merge_demog(screening_csv_fns)),
  tar_target(
    demog_submissions_w_n_calls,
    add_n_calls_to_demog(demog_submissions)
  ),
  tar_target(
    calls_plot,
    plot_call_timeseries(demog_submissions_w_n_calls)
  ),
  tar_target(bar_plot, plot_calls_by_site(demog_submissions_w_n_calls)),
  tar_target(kb_home, dplyr::filter(
    kb_df, stringr::str_detect(title, "Home")
  )),
  tar_target(home_visit_dir_xlsx, "data/xlsx/home_visit"),
  tar_target(home_visit_dir_csv, "data/csv/home_visit"),
  tar_target(
    home_visit_xlsx_fns,
    list.files(
      home_visit_dir_xlsx,
      pattern = "[0-9]+.*\\.xlsx",
      full.names = TRUE
    )
  ),
  tar_target(
    home_visit_csv_fns,
    list.files(
      home_visit_dir_csv,
      pattern = "[0-9]+.*\\.csv",
      full.names = TRUE
    )
  ),
  tar_target(
    home_visit_downloads,
    retrieve_kobo_xlsx(kb_home, home_visit_dir_xlsx),
    cue = tarchetypes::tar_cue_age(name = home_visit_downloads, age = as.difftime(update_interval_days, units = "days"))
  ),
  tar_target(home_visit_renamed, rename_home_xlsx(home_visit_dir_xlsx)),
  tar_target(
    home_visit_xlsx_to_csv,
    load_xlsx_save_many_csvs(home_visit_dir_xlsx, home_visit_dir_csv, "Home")
  )
)