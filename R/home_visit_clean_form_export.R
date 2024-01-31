#'
#'@param fn XLSX form file name.
#'@param out_dir Output directory for CSV file
home_visit_clean_form_export <-
  function(fn = "data/xlsx/home_visit/raw/form/331848_PLAY_Home_Questionnaires_12_English-forms.xlsx",
           out_dir = "data/csv/home_visit/dd",
           overwrite = TRUE) {
    assertthat::assert_that(file.exists(fn))
    assertthat::assert_that(dir.exists(out_dir))
    
    # Open form as data frame
    df <- readxl::read_xlsx(fn)
    
    # Drop columns
    df <- df |>
      dplyr::select(c('type', 'name', 'label'))
    
    # Delete non-data rows
    type_not_data <-
      c('begin_group', 'note', 'end_group', 'acknowledge')
    
    # Clean labels
   df <- df |>
     dplyr::mutate(label = stringr::str_remove(label, "^[A-Z]{1}[0-9]?[a-z]?\\.")) |>
     dplyr::mutate(label = stringr::str_remove_all(label, '["]')) |>
     dplyr::mutate(label = stringr::str_remove(label, "^ "))

    # Export
    out_fn <-
      file.path(out_dir, paste0(tools::file_path_sans_ext(basename(fn)), ".csv"))
    
    if (file.exists(out_fn)) {
      message("File exists: ", out_fn)
      if (overwrite) {
        message("Overwriting file: ", out_fn)
        df <- df |>
          dplyr::filter(!(type %in% type_not_data)) |>
          readr::write_csv(out_fn)
      } else {
        message("File not overwritten.")
        return(NULL)
      }
    } else {
      df |>
        dplyr::filter(!(type %in% type_not_data)) |>
        readr::write_csv(out_fn)
    }
    
    out_fn
  }
