screen_remove_databrary_fields <- function(df) {
  stopifnot(is.data.frame(df))
  
  require(dplyr)
  
  dplyr::select(df,-contains('group_databrary'))
}
