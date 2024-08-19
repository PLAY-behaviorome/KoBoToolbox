#'
#'@param fn XLSX form file name.
#'@param out_dir Output directory for CSV file
screen_clean_form_export <-
  function(fn = "data/xlsx/screening/form/275882_PLAY_Demographic_Questionnaire-forms.xlsx",
           out_dir = "data/csv/screening/dd",
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
      c('start', 
        'end', 
        'begin_group', 
        'note', 
        'end_group', 
        'acknowledge')
    
    name_not_data <-
      c('c_today', 
        'check_childage', 
        'check_childage_months', 
        'check_childage_weeks',
        'confirm_child_birthdate', 
        'endscreener_preterm1', 
        'endscreener_preterm3',
        'endscreener_preterm2', 
        'endscreener_birthweight',
        'endscreener_birth_newborn_comp',
        'endscreener_hearing',
        'endscreener_vision',
        'endscreener_illnesses_injuries',
        'endscreener_momage',
        'endscreener_biodad_age',
        'endscreener_biomom_age')
    
    # Clean labels
   df <- df |>
     dplyr::mutate(label = stringr::str_remove(label, "^[A-Z]{1}[0-9]?[a-z]?\\.")) |>
     dplyr::mutate(label = stringr::str_remove_all(label, '["]')) |>
     dplyr::mutate(label = stringr::str_remove(label, "^ "))
   
   # Remove non-data
   df <- df |>
     dplyr::filter(!(type %in% type_not_data)) |>
     dplyr::filter(!(name %in% name_not_data))

    # Export
    out_fn <-
      file.path(out_dir, paste0(tools::file_path_sans_ext(basename(fn)), ".csv"))
    
    if (file.exists(out_fn)) {
      message("File exists: ", out_fn)
      if (overwrite) {
        message("Overwriting file: ", out_fn)
        readr::write_csv(df, out_fn)
      } else {
        message("File not overwritten.")
        return(NULL)
      }
    } else {
      readr::write_csv(df, out_fn)
    }
    
    out_fn
  }
