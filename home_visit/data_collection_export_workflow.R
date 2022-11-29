###################################################################
#' Runs sequence of download, clean, and export scripts.
#'
#'
rmarkdown::render("home_visit/01-download_from_KoBo.Rmd")

rmarkdown::render("home_visit/02-save_raw_csvs.Rmd")

rmarkdown::render("home_visit/03-split_mbcdi_others.Rmd")

rmarkdown::render("home_visit/04-remove_identifiers.Rmd")

rmarkdown::render("home_visit/05-conduct_initial_qa.Rmd")

rmarkdown::render("home_visit/06-make_aggregate_csv.Rmd")

rmarkdown::render(
  "home_visit/07-merge_databrary_info.Rmd",
  params = list(databrary_login = "rogilmore@psu.edu")
)

rmarkdown::render("home_visit/08-clean_aggregate.Rmd")

# Copy `csv/release_1.0/aggregate/PLAY_non_MBCDI_all_share.csv` to `https://nyu.databrary.org/volume/1280`

rmarkdown::render("visualize/release_1.0_survey_viz_slides.Rmd",
                  params = list(use_databrary = FALSE))
