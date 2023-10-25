mcdi_clean_12_csv <- function(csv_fn) {
  
  new_csv_fn <- mcdi_convert_understands(csv_fn)
  
  df <- readr::read_csv(new_csv_fn,
                        col_types = readr::cols(.default = 'c'),
                        show_col_types = FALSE)
  
  df |>
    mcdi_select_basename() |>
    mcdi_trim_meta_fields() |>
    mcdi_modify_var_names()
}
