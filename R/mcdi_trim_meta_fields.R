mcdi_trim_meta_fields <- function(df) {
  assertthat::assert_that(is.data.frame(df))
  
  df |>
    dplyr::select(-contains("note"),
                  -contains("instructions"),
                  -contains("comments"),
                  -contains("continue"),
                  -contains("vocab"),
                  -contains("mcdi"))
}
