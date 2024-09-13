hv <- readr::read_csv("data/csv/home_visit/agg/PLAY-non-mcdi-latest.csv")

session_fl <- list.files("data/csv/site_sessions", "\\.csv$", full.names = TRUE)

make_augmented_sess_df <- function(fn) {
  df <- readr::read_csv(fn, col_types = readr::cols(.default = "c"))
  dplyr::mutate(df, site_id = )
}
db <- purrr::map(list.files("data/csv/site_sessions", "\\.csv$", full.names = TRUE), readr::read_csv, col_types = readr::cols(.default = "c"))

db |> purrr::list_rbind() -> db_agg