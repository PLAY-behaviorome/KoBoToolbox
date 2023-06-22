#-------------------------------------------------------------------------------
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
#' @returns Data frame with the merged screening/demographic survey data.
screening_download_make_csvs <-
  function(kb_screen = kobo_list_data_filtered("[Dd]emographic"),
           xlsx_dir = "data/xlsx/screening",
           csv_dir = "data/csv/screening") {
    stopifnot(is.data.frame(kb_screen))
    stopifnot(dim(kb_screen) > 0)
    stopifnot(is.character(xlsx_dir))
    stopifnot(dir.exists(xlsx_dir))
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    kobo_retrieve_save_many_xlsx(kb_screen, xlsx_dir)
    
    file_load_xlsx_save_many_csv(xlsx_dir, csv_dir, "Demographic")
    
    # screen_csv_fns <-
    #   list.files(csv_dir, pattern = "[0-9]+.*\\.csv", full.names = TRUE)
    # screening_clean_merge(screen_csv_fns)
  }