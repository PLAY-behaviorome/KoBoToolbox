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
    cue = tarchetypes::tar_cue_age(
      name = screening_downloads,
      age = as.difftime(update_interval_days, units = "days")
    )
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
  # Home visit
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
  # Home visit downloads from KoBoToolbox
  tar_target(
    home_visit_downloads,
    retrieve_kobo_xlsx(kb_home, home_visit_dir_xlsx),
    cue = tarchetypes::tar_cue_age(
      name = home_visit_downloads,
      age = as.difftime(update_interval_days, units = "days")
    )
  ),
  tar_target(home_visit_renamed, rename_home_xlsx(home_visit_dir_xlsx)),
  tar_target(
    home_visit_xlsx_to_csv,
    load_xlsx_save_many_csvs(home_visit_dir_xlsx, home_visit_dir_csv, "Home")
  ),
  tar_target(
    home_visit_csvs,
    list.files(home_visit_dir_csv, '^[0-9]+_PLAY.*\\csv', full.names = TRUE)
  ),
  # Non-MB-CDI CSVs
  tar_target(
    home_visit_non_mbcdi,
    purrr::map(
      home_visit_csvs,
      open_split_save,
      csv_save_dir = home_visit_dir_csv,
      these_questions = 'non_mbcdi'
    )
  ),
  # MB-CDI CSVs
  tar_target(
    home_visit_mbcdi,
    purrr::map(
      home_visit_csvs,
      open_split_save,
      csv_save_dir = home_visit_dir_csv,
      these_questions = 'mbcdi'
    )
  ),
  # De-identified CSVs
  tar_target(
    home_visit_non_mbcdi_csvs,
    list.files(home_visit_dir_csv, '^[0-9]+_non_mbcdi.*\\csv', full.names = TRUE)
  ),
  tar_target(
    home_visit_remove_identifiers,
    purrr::map(
      home_visit_non_mbcdi_csvs,
      open_deidentify_save,
      csv_save_dir = home_visit_dir_csv,
      these_questions = 'non_mbcdi'
    )
  ),
  # Merge non-MB-CDI datafiles
  tar_target(
    files_w_288_cols,
    list.files(
      home_visit_dir_csv,
      "2[3458]_non_mbcdi.*_deidentified\\.csv",
      full.names = TRUE
    )
  ),
  tar_target(
    df_merge_288_cols,
    make_aggregate_data_file(files_w_288_cols)
  ),
  tar_target(
    files_w_287_cols_1,
    list.files(
      home_visit_dir_csv,
      "2[69]_non_mbcdi.*_deidentified\\.csv",
      full.names = TRUE
    )
  ),
  tar_target(
    df_merge_287_cols_1,
    make_aggregate_data_file(files_w_287_cols_1)
  ),
  tar_target(
    files_w_287_cols_2,
    list.files(
      home_visit_dir_csv,
      "(740627|740630|740631)_non.*_deidentified\\.csv",
      full.names = TRUE
    )
  ),
  tar_target(
    df_merge_287_cols_2,
    make_aggregate_data_file(files_w_287_cols_2)
  )
)