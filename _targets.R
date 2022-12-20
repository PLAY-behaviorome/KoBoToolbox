# _targets.R

library(targets)
library(tarchetypes)
source("R/functions.R")
suppressPackageStartupMessages(library(tidyverse))
tar_option_set(packages = c("readr", "dplyr", "ggplot2", "purrr", "tools",
                            "httr", "stringr"))
update_interval <- 2
update_interval_units <- "days"

list(
  tar_target(kb_df, list_kobo_data()),
  tar_target(kb_screen, dplyr::filter(
    kb_df, stringr::str_detect(title, "Demographic")
  )),
  tar_target(kb_home, dplyr::filter(
    kb_df, stringr::str_detect(title, "Home")
  )),
  tar_target(kb_post_visit, dplyr::filter(
    kb_df, stringr::str_detect(title, "Post\\-Visit")
  )),
  # Make data frame from screening/demographic survey
  tar_target(
    screen_df,
    make_screening_df(kb_screen, "data/xlsx/screening", "data/csv/screening"),
    cue = tarchetypes::tar_cue_age(
      name = demog_submissions,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  # Make data frame from post-visit surveys
  tar_target(
    post_visit_df,
    make_post_visit_df(kb_post_visit,
                       "data/xlsx/post_visit",
                       "data/csv/post_visit")
  ),
  ###### Home visit data
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
    split_non_mbcdi_csvs(
      list.files("data/csv/home_visit/raw", full.names = TRUE),
      "data/csv/home_visit/non_mbcdi"
    ),
    cue = tarchetypes::tar_cue_age(
      name = home_visit_non_mbcdi,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    home_visit_remove_identifiers,
    purrr::map(
      list.files("data/csv/home_visit/non_mbcdi", "\\.csv", full.names = TRUE),
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
    df_merge_288_cols,
    make_aggregate_data_file(
      list.files(
        "data/csv/home_visit/non_mbcdi/deid",
        "2[3458]_non_mbcdi.*_deidentified\\.csv",
        full.names = TRUE
      )
    ),
    cue = tarchetypes::tar_cue_age(
      name = df_merge_288_cols,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(
    df_merge_287_cols_1,
    make_aggregate_data_file(
      list.files(
        "data/csv/home_visit/non_mbcdi/deid",
        "2[69]_non_mbcdi.*_deidentified\\.csv",
        full.names = TRUE
      )
    ),
    cue = tarchetypes::tar_cue_age(
      name = df_merge_287_cols_1,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  ),
  tar_target(df_merge_287_cols_2,
             make_aggregate_data_file(
               list.files(
                 "data/csv/home_visit/non_mbcdi/deid",
                 "(740627|740630|740631)_non.*_deidentified\\.csv",
                 full.names = TRUE
               )
             )),
  tar_target(df_home_visit,
             rbind(
               clean_dfs(df_merge_287_cols_2),
               clean_dfs(df_merge_287_cols_1),
               clean_dfs(df_merge_288_cols)
             )),
  # MB-CDI CSVs
  tar_target(
    home_visit_mbcdi,
    split_mbcdi_csvs(
      list.files("data/csv/home_visit/raw", full.names = TRUE),
      "data/csv/home_visit/mbcdi"
    ),
    cue = tarchetypes::tar_cue_age(
      name = home_visit_mbcdi,
      age = as.difftime(update_interval, units = update_interval_units)
    )
  )
)