# _targets.R

library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("readr", "dplyr", "ggplot2", "purrr", "tools",
                            "httr", "stringr"))
update_interval <- 2
update_interval_units <- "mins"

list(
  tar_target(kb_df, list_kobo_data()),
  
  tar_target(kb_screen, dplyr::filter(
    kb_df, stringr::str_detect(title, "Demographic")
  )),
  
  tar_target(kb_home, dplyr::filter(
    kb_df, stringr::str_detect(title, "Home")),
    
  tar_target(kb_post_visit, dplyr::filter(kb_df, stringr::str_detect(title, "Post\\-Visit"))),
  
  tar_target(
    screen_df,
    make_screening_df(kb_screen, "data/xlsx/screening", "data/csv/screening"),
    cue = tarchetypes::tar_cue_age(
      name = demog_submissions,
      age = as.difftime(update_interval, units = update_interval_units)
    ))
  # tar_target(
  #   demog_submissions_w_n_calls,
  #   add_n_calls_to_demog(demog_submissions)
  # ),
  # tar_target(
  #   calls_plot,
  #   plot_call_timeseries(demog_submissions_w_n_calls)
  # ),
  # tar_target(bar_plot, plot_calls_by_site(demog_submissions_w_n_calls)),
  # Home visit
  # tar_target(kb_home, dplyr::filter(
  #   kb_df, stringr::str_detect(title, "Home")
  # )),
  # tar_target(home_visit_dir_xlsx, "data/xlsx/home_visit/raw", format = "file"),
  # tar_target(home_visit_dir_xlsx_std, "data/xlsx/home_visit/std_name", format = "file"),
  # tar_target(home_visit_dir_csv, "data/csv/home_visit/raw", format = "file"),
  # tar_target(
  #   home_visit_xlsx_fns,
  #   list.files(
  #     home_visit_dir_xlsx,
  #     pattern = "[0-9]+.*\\.xlsx",
  #     full.names = TRUE
  #   )
  # ),
  # tar_target(
  #   home_visit_csv_fns,
  #   list.files(
  #     home_visit_dir_csv,
  #     pattern = "[0-9]+.*\\.csv",
  #     full.names = TRUE
  #   )
  # ),
  # # Home visit downloads from KoBoToolbox
  # tar_target(
  #   home_visit_downloads,
  #   retrieve_kobo_xlsx(kb_home, home_visit_dir_xlsx),
  #   cue = tarchetypes::tar_cue_age(
  #     name = home_visit_downloads,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  # tar_target(home_visit_renamed, rename_home_xlsx(home_visit_dir_xlsx, home_visit_dir_xlsx_std)),
  # tar_target(
  #   home_visit_xlsx_to_csv,
  #   load_xlsx_save_many_csvs(home_visit_dir_xlsx_std, home_visit_dir_csv, "Home"),
  #   cue = tarchetypes::tar_cue_age(
  #     name = screening_xlsx_to_csv,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  # tar_target(
  #   home_visit_csvs,
  #   list.files(home_visit_dir_csv, '^[0-9]+_PLAY.*\\csv', full.names = TRUE)
  # ),
  # # Non-MB-CDI CSVs
  # tar_target(
  #   home_visit_non_mbcdi,
  #   purrr::map(
  #     home_visit_csvs,
  #     open_split_save,
  #     csv_save_dir = "data/csv/home_visit/non_mbcdi",
  #     these_questions = 'non_mbcdi'
  #   ),
  #   cue = tarchetypes::tar_cue_age(
  #     name = home_visit_non_mbcdi,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  # # MB-CDI CSVs
  # tar_target(
  #   home_visit_mbcdi,
  #   purrr::map(
  #     home_visit_csvs,
  #     open_split_save,
  #     csv_save_dir = "data/csv/home_visit/non_mbcdi",
  #     these_questions = 'mbcdi'
  #   ),
  #   cue = tarchetypes::tar_cue_age(
  #     name = home_visit_mbcdi,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  # # De-identified CSVs
  # tar_target(
  #   home_visit_non_mbcdi_csvs,
  #   list.files(home_visit_dir_csv, '^[0-9]+_non_mbcdi.*\\csv', full.names = TRUE)
  # ),
  # tar_target(
  #   home_visit_remove_identifiers,
  #   purrr::map(
  #     home_visit_non_mbcdi_csvs,
  #     open_deidentify_save,
  #     csv_save_dir = "data/csv/home_visit/non_mbcdi",
  #     these_questions = 'non_mbcdi'
  #   ),
  #   cue = tarchetypes::tar_cue_age(
  #     name = home_visit_remove_identifiers,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  # # Merge non-MB-CDI datafiles
  # tar_target(
  #   files_w_288_cols,
  #   list.files(
  #     "data/csv/home_visit/non_mbcdi",
  #     "2[3458]_non_mbcdi.*_deidentified\\.csv",
  #     full.names = TRUE
  #   )
  # ),
  # tar_target(
  #   df_merge_288_cols,
  #   make_aggregate_data_file(files_w_288_cols),
  #   cue = tarchetypes::tar_cue_age(
  #     name = df_merge_288_cols,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  # tar_target(
  #   files_w_287_cols_1,
  #   list.files(
  #     home_visit_dir_csv,
  #     "2[69]_non_mbcdi.*_deidentified\\.csv",
  #     full.names = TRUE
  #   )
  # ),
  # tar_target(
  #   df_merge_287_cols_1,
  #   make_aggregate_data_file(files_w_287_cols_1),
  #   cue = tarchetypes::tar_cue_age(
  #     name = df_merge_287_cols_1,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  # tar_target(
  #   files_w_287_cols_2,
  #   list.files(
  #     home_visit_dir_csv,
  #     "(740627|740630|740631)_non.*_deidentified\\.csv",
  #     full.names = TRUE
  #   )
  # ),
  # tar_target(
  #   df_merge_287_cols_2,
  #   make_aggregate_data_file(files_w_287_cols_2),
  #   cue = tarchetypes::tar_cue_age(
  #     name = df_merge_287_cols_2,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  # tar_target(
  #   df_merged_recent,
  #   rbind(
  #     clean_dfs(df_merge_287_cols_2),
  #     clean_dfs(df_merge_287_cols_1),
  #     clean_dfs(df_merge_288_cols)
  #   ),
  #   cue = tarchetypes::tar_cue_age(
  #     name = df_merged_recent,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  # tar_target(db_login_id, Sys.getenv("DATABRARY_LOGIN")),
  # # Post-visit notes
  # tar_target(kb_post_visit, dplyr::filter(
  #   kb_df, stringr::str_detect(title, "Post\\-Visit")
  # )),
  # tar_target(post_visit_dir_xlsx, "data/xlsx/post_visit"),
  # tar_target(
  #   post_visit_downloads,
  #   retrieve_kobo_xlsx(kb_post_visit, post_visit_dir_xlsx),
  #   cue = tarchetypes::tar_cue_age(
  #     name = post_visit_downloads,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  # tar_target(post_visit_dir_csv, "data/csv/post_visit"),
  # tar_target(
  #   post_visit_xlsx_to_csv,
  #   load_xlsx_save_many_csvs(post_visit_dir_xlsx, post_visit_dir_csv, "Post\\-Visit"),
  #   cue = tarchetypes::tar_cue_age(
  #     name = post_visit_xlsx_to_csv,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # )
))