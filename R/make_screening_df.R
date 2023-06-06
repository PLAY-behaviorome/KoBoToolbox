################################################################################
#' Downloads the XLSX files, converts them to CSVs, copies the CSVs to a 
#' specified target directory, then opens the CSVs and combines them into 
#' a single data frame
#' @param kb_screen Data frame with file information about the screening/demog
#' survey data. This comes from the larger data frame of all KBT forms, but
#' filtered to include 'Demographic' in the file name.
#' @param xlsx_dir Directory where the XLSX format screening/demog surveys are
#' stored. Default is "data/xlsx/screening".
#' @param csv_dir Directory where the CSV file should be saved. Default is
#' "data/csv/screening".
#' @return NULL
make_screening_df <- function(kb_screen, xlsx_dir, csv_dir) {
  stopifnot(is.data.frame(kb_screen))
  stopifnot(dir.exists(xlsx_dir))
  stopifnot(dir.exists(csv_dir))
  
  retrieve_kobo_xlsx(kb_screen, xlsx_dir)
  load_xlsx_save_many_csvs(xlsx_dir, csv_dir, "Demographic")
  screen_csv_fns <-
    list.files(csv_dir, pattern = "[0-9]+.*\\.csv", full.names = TRUE)
  clean_merge_demog(screen_csv_fns)
}