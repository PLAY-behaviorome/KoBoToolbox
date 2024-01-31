#-------------------------------------------------------------------------------
#' Downloads the XLSX files, converts them to CSVs, copies the CSVs to a
#' specified target directory, then opens the CSVs and combines them into
#' a single data frame.
#' 
#' @param kb_home Data frame with file information about the home visit
#' survey data. This comes from the larger data frame of all KBT forms, but
#' filtered to include 'Home' in the file name.
#' @param xlsx_dir Directory where the XLSX format home visit surveys are
#' stored. Default is "data/xlsx/home_visit".
#' @param csv_dir Directory where the CSV file should be saved. Default is
#' "data/csv/home_visit".
#' @param save_form Logical value. Save the KBT "form" file or not.
#' 
#' @returns Data frame with the merged screening/demographic survey data.
#' 
#' @export
home_download_convert <-
  function(kb_home = kobo_list_data_filtered("Home"),
           xlsx_dir = "data/xlsx/home_visit",
           csv_dir = "data/csv/home_visit",
           save_form = TRUE) {
    
    # source("R/kobo_list_data_filtered.R")
    # source("R/kobo_retrieve_save_many_xlsx.R")
    # source("R/file_load_xlsx.save_csv.R")
    # source("R/file_load_xlsx_save_many_csv.R")
    
    stopifnot(is.data.frame(kb_home))
    stopifnot(dim(kb_home) > 0)
    stopifnot(is.character(xlsx_dir))
    stopifnot(dir.exists(xlsx_dir))
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    kobo_retrieve_save_many_xlsx(kb_home, xlsx_dir)
    
    file_load_xlsx_save_many_csv(xlsx_dir, csv_dir, "Home")
    
  }