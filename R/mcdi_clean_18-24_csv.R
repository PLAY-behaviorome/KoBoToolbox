mcdi_clean_18_24_csv <- function(csv_fn) {
  
  #new_csv_fn <- mcdi_convert_understands(csv_fn)
  
  df <- readr::read_csv(csv_fn,
                        col_types = readr::cols(.default = 'c'),
                        show_col_types = FALSE)
  
  df |>
    mcdi_select_basename() |>
    mcdi_modify_18_24_var_names() |>
    mcdi_trim_meta_fields()

}
