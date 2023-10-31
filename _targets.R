# _targets.R

library(targets)
library(tarchetypes)

source("R/_OLD/functions.R")
fl <-
  list.files("R",
             "^kobo_|^file_|^screen_|^ecbq_|^health_|^databrary|^home|^export",
             full.names = TRUE)
purrr::walk(fl, source)

# Log in to Databrary
databraryr::login_db(email = Sys.getenv("DATABRARY_LOGIN"), store = TRUE)

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(databraryr))

tar_option_set(
  packages = c(
    "readr",
    "dplyr",
    "ggplot2",
    "purrr",
    "tools",
    "httr",
    "stringr",
    "databraryr"
  )
)

update_interval <- 2
update_interval_units <- "days"

list(
  tar_target(
    kb_df,
    list_kobo_data(),
    cue = tarchetypes::tar_cue_age(
      name = kb_df,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    kb_home,
    dplyr::filter(kb_df,
                  stringr::str_detect(title, "Home")),
    cue = tarchetypes::tar_cue_age(
      name = kb_home,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  # Download screening/demographic survey
  tar_target(
    kb_screen_df,
    kobo_list_data_filtered("[Dd]emographic"),
    cue = tarchetypes::tar_cue_age(
      name = kb_screen,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    screen_download,
    screen_download_convert(kb_screen_df, "data/xlsx/screening", "data/csv/screening"),
    cue = tarchetypes::tar_cue_age(
      name = screen_df,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  # Post-visit surveys
  tar_target(
    kb_post_visit_df,
    kobo_list_data_filtered("Post\\-Visit"),
    cue = tarchetypes::tar_cue_age(
      name = kb_post_visit,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    post_visit_download,
    kobo_retrieve_save_many_xlsx(kb_post_visit_df, "data/xlsx/post_visit"),
    cue = tarchetypes::tar_cue_age(
      name = screen_df,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    post_visit_csvs,
    load_xlsx_save_many_csvs("data/xlsx/post_visit", "data/csv/post_visit", "PLAY_Post"),
    cue = tarchetypes::tar_cue_age(
      name = post_visit_csvs,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  # Home visit data
  tar_target(
    home_visit_downloads,
    retrieve_kobo_xlsx(kb_home, "data/xlsx/home_visit/raw"),
    cue = tarchetypes::tar_cue_age(
      name = home_visit_downloads,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    home_visit_renamed,
    rename_home_xlsx(home_visit_downloads,
                     "data/xlsx/home_visit/std_name"),
    cue = tarchetypes::tar_cue_age(
      name = home_visit_renamed,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    home_visit_xlsx_to_csv,
    load_xlsx_save_many_csvs_2(home_visit_renamed, "data/csv/home_visit/raw"),
    cue = tarchetypes::tar_cue_age(
      name = home_visit_xlsx_to_csv,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  # Non-MB-CDIs
  tar_target(
    home_visit_non_mbcdi,
    split_non_mbcdi_csvs(home_visit_xlsx_to_csv,
                         "data/csv/home_visit/non_mbcdi"),
    cue = tarchetypes::tar_cue_age(
      name = home_visit_non_mbcdi,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    home_visit_remove_identifiers,
    purrr::map_chr(
      home_visit_non_mbcdi,
      open_deidentify_save,
      csv_save_dir = "data/csv/home_visit/non_mbcdi/deid",
      these_questions = 'non_mbcdi'
    ),
    cue = tarchetypes::tar_cue_age(
      name = home_visit_remove_identifiers,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  # Merge non-MB-CDI datafiles
  tar_target(
    files_288_cols,
    stringr::str_detect(
      home_visit_remove_identifiers,
      "2[3458]_non_mbcdi.*_deidentified\\.csv"
    ),
    cue = tarchetypes::tar_cue_age(
      name = files_288_cols,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    files_287_cols_1,
    stringr::str_detect(
      home_visit_remove_identifiers,
      "2[69]_non_mbcdi.*_deidentified\\.csv"
    ),
    cue = tarchetypes::tar_cue_age(
      name = files_287_cols_1,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    files_287_cols_2,
    stringr::str_detect(
      home_visit_remove_identifiers,
      "(740627|740630|740631)_non.*_deidentified\\.csv"
    ),
    cue = tarchetypes::tar_cue_age(
      name = files_287_cols_2,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    df_merge_288_cols,
    make_aggregate_data_file(home_visit_remove_identifiers[files_288_cols]),
    cue = tarchetypes::tar_cue_age(
      name = df_merge_288_cols,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    df_merge_287_cols_1,
    make_aggregate_data_file(home_visit_remove_identifiers[files_287_cols_1]),
    cue = tarchetypes::tar_cue_age(
      name = df_merge_287_cols_1,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    df_merge_287_cols_2,
    make_aggregate_data_file(home_visit_remove_identifiers[files_287_cols_2]),
    cue = tarchetypes::tar_cue_age(
      name = df_merge_287_cols_2,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    home_visit_df,
    rbind(
      clean_dfs(df_merge_287_cols_2),
      clean_dfs(df_merge_287_cols_1),
      clean_dfs(df_merge_288_cols)
    ),
    cue = tarchetypes::tar_cue_age(
      name = home_visit_df,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    files_274_cols,
    stringr::str_detect(home_visit_remove_identifiers, "/(307736|331453)"),
    cue = tarchetypes::tar_cue_age(
      name = files_274_cols,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    df_merge_274_cols,
    make_aggregate_data_file(home_visit_remove_identifiers[files_274_cols]),
    cue = tarchetypes::tar_cue_age(
      name = df_merge_274_cols,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    home_visit_w_databrary_df,
    add_databrary_info_to_home_visit_df(home_visit_df, vb = TRUE),
    cue = tarchetypes::tar_cue_age(
      name = home_visit_w_databrary_df,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  # MB-CDI CSVs
  tar_target(
    home_visit_mbcdi,
    split_mbcdi_csvs(home_visit_xlsx_to_csv,
                     "data/csv/home_visit/mbcdi")
  ),
  # ECBQ data
  tar_target(
    ecbq_wide_df,
    ecbq_clean_make_agg_df(),
    cue = tarchetypes::tar_cue_age(
      name = ecbq_wide_df,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  # Health data
  tar_target(
    health_df,
    health_clean_make_agg_df(),
    cue = tarchetypes::tar_cue_age(
      name = health_df,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  # Databrary session info
  # tar_target(
  #   play_databrary_sess_df,
  #   make_site_session_summary_multiple(play_vols),
  #   cue = tarchetypes::tar_cue_age(
  #     name = play_databrary_sess_df,
  #     age = as.difftime(update_interval, units = update_interval_units)
  #   )
  # ),
  tar_target(
    play_vols_df,
    readr::read_csv("data/csv/_meta/play_site_vols.csv",
                    show_col_types = FALSE)
  ),
  # Export Databrary session CSVs
  tar_target(
    databrary_session_csvs,
    purrr::walk(play_vols_df$site_id, databrary_get_save_session_csv),
    cue = tarchetypes::tar_cue_age(
      name = databrary_session_csvs,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  # Export site-specific CSVs
  tar_target(export_all_site_csvs,
             purrr::walk(play_vols_df$site_id, export_site_csvs, vb = FALSE),
             cue = tarchetypes::tar_cue_age(
               name = export_all_site_csvs,
               age = as.difftime(update_interval, units = update_interval_units)
             ))
)
