screen_clean_raw_join <- function() {
  
  fl <- list.files(file.path(here::here(), "data/csv/screening"), full.names = TRUE)
  
  d1c <- screen_clean_raw_csv(fl[1])
  d2c <- screen_clean_raw_csv(fl[2])
  d3c <- screen_clean_raw_csv(fl[3])
  
  m1 <- dplyr::full_join(d3c, d2c)
  dplyr::full_join(m1, d1c)
}