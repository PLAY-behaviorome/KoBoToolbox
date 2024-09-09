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
    # geo$make_addresses("new") |>
    # geo$get_multiple_census_geos() |>
    screen_remove_identifiers() |>
    screen_remove_metadata_fields() |>
    screen_remove_databrary_fields()
}
