screening_extract_databrary_info <- function(csv_fn, vb = FALSE) {
  assertthat::is.string(csv_fn)
  assertthat::is.readable(csv_fn)
  
  suppressPackageStartupMessages(require(readr))
  suppressPackageStartupMessages(require(dplyr))
  
  # Bring data about PLAY volumes on Databrary into workspace
  if (!("PLAY_VOLS" %in% ls())) {
    if (vb)
      message("'PLAY_VOLS' not in local environment. Sourcing.")
    source(file.path(here::here(), "R", "PLAY_VOLS.R"))
  }
  
  df <-
    readr::read_csv(csv_fn,
                    col_types = readr::cols(.default = 'c'),
                    show_col_types = FALSE)
  
  kobo_form_num <-
    stringr::str_extract(basename(csv_fn), pattern = "^[0-9]+")
  
  if (assertthat::not_empty(df)) {
    if (kobo_form_num == "275882") {
      db_df <- df |>
        dplyr::select(
          `play_phone_questionnaire/group_siteinfo/subject_number`,
          `play_phone_questionnaire/group_siteinfo/site_id`,
          `play_phone_questionnaire/group_siteinfo/concat2`
        ) |>
        dplyr::rename(
          "sub_num" = "play_phone_questionnaire/group_siteinfo/subject_number",
          "site_id" = "play_phone_questionnaire/group_siteinfo/site_id",
          "play_guid" = "play_phone_questionnaire/group_siteinfo/concat2"
        ) |>
        dplyr::mutate(
          site_id = dplyr::recode(
            site_id,
            NYU = "NYUNI",
            new_york_unive = "NYUNI",
            georgetown_uni = "GEORG",
            GTN = "GEORG",
            UCR = "UCRIV"
          )
        ) |>
        dplyr::filter(!stringr::str_detect(site_id, '_of__')) |>
        dplyr::mutate(sub_num = stringr::str_pad(sub_num, 3, "left", "0"))
    } else {
      db_df <- df |>
        dplyr::select(
          `play_demo_questionnaire/group_siteinfo/subject_number`,
          `play_demo_questionnaire/group_siteinfo/site_id`,
          `play_demo_questionnaire/play_id`
        ) |>
        dplyr::rename(
          "sub_num" = "play_demo_questionnaire/group_siteinfo/subject_number",
          "site_id" = "play_demo_questionnaire/group_siteinfo/site_id",
          "play_guid" = "play_demo_questionnaire/play_id"
        )
    }
    db_df |>
      dplyr::mutate(db_session = paste0("PLAY_", site_id, "_", sub_num)) |>
      dplyr::arrange(site_id, sub_num)
  } else {
    if (vb)
      message("Can't read '", csv_fn, "' as data frame.")
    NULL
  }
}

lookup_db_vol_id <- function(i = 1, df, PLAY_VOLS) {
  assertthat::is.number(i)
  assertthat::assert_that(i >= 1)
  assertthat::not_empty(df)
  
  this_row <- df[i, ]
  this_site_id <- this_row$site_id
  
  found_site <-
    dplyr::filter(PLAY_VOLS, stringr::str_detect(play_site_id, this_site_id))
}