mcdi_modify_dupes <- function(df, dupe = 'leg') {
  assertthat::assert_that(is.data.frame(df))
  assertthat::is.string(dupe)
  
  dup_index <- seq_along(df)[stringr::str_detect(names(df), dupe)]
  for (i in 1:length(dup_index)) {
    this_dup <- dup_index[i]
    names(df)[this_dup] <- paste0(dupe, "_", i)
  }
  df
}