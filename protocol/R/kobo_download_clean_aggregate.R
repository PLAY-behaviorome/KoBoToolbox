###################################################################
#' Runs sequence of download, clean, and export scripts.
#'
#'
rmarkdown::render("01-download_from_KoBo.Rmd")
rmarkdown::render("02-save_raw_csvs.Rmd")
rmarkdown::render("03-split_mbcdi_others.Rmd")
rmarkdown::render("04-remove_identifiers.Rmd")
rmarkdown::render("05-conduct_initial_qa.Rmd")
rmarkdown::render("06-make_aggregate_csv.Rmd")
rmarkdown::render("07-merge_databrary_info.Rmd", params=list(databrary_login="rogilmore@psu.edu"))
rmarkdown::render("08-clean_aggregate.Rmd")