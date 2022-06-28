# Run KoBoToolbox data collection workflow

rmarkdown::render("01-download_from_KoBo.Rmd")

rmarkdown::render("02-save_raw_csvs.Rmd")

rmarkdown::render("03-split_mbcdi_others.Rmd")

rmarkdown::render("04-remove_identifiers.Rmd")

rmarkdown::render("05-conduct_initial_qa.Rmd")

rmarkdown::render("06-make_aggregate_csv.Rmd", params = list(databrary_login = "rogilmore@psu.edu"))

rmarkdown::render("07-clean_aggregate.Rmd")

# At this point, copy `csv/release_1.0/aggregate/PLAY_non_MBCDI_all_share.csv` to `https://nyu.databrary.org/volume/1280`

rmarkdown::render("release_1.0_survey_viz_slides.Rmd")