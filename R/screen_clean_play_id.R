screen_clean_play_id <- function(df) {
  stopifnot(is.data.frame(df))
  
  df |>
    tidyr::unite(col = "play_id", c("play_id", "concat2"), na.rm = TRUE)
}
