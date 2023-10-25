mcdi_select_basename <- function(df) {
  assertthat::assert_that(is.data.frame(df))
  
  names(df) <- basename(names(df))
  df
}