# Exports the demographic/screening data from KoBoToolbox
# Saves the exported XLSX forms, raw/uncleaned CSVs, and the deidentified CSVs

rmarkdown::render("demog_screening/01-download_demo_from_KoBo.Rmd")

rmarkdown::render("demog_screening/02-save_raw_demographic_csvs.Rmd")

rmarkdown::render("demog_screening/04-remove_demo_identifiers.Rmd")
