screen_clean_raw_csv <- function(fn) {
  stopifnot(is.character(fn))
  stopifnot(file.exists(fn))

  # Import all as character to avoid type guess conflicts
  df <-
    readr::read_csv(fn,
                    col_types = readr::cols(.default = 'c'),
                    show_col_types = FALSE)

  message("Cleaning '", fn, "'.")

  df |>
    screen_remove_variable_headers() |>
    # added screen_add_fips on 2024-09-13
    screen_add_fips() |>
    screen_remove_identifiers() |>
    screen_remove_metadata_fields() |>
    screen_remove_databrary_fields()
}
