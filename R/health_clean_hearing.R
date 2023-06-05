health_clean_hearing <- function(df) {
  hearing_df <- df |>
    dplyr::select(contains("hearing_tested"),
                  -contains("birthhospital"),
                  -contains("afterhome"),
                  -contains("no"),
                  -contains("refused"),
                  -contains("donotknow"))
  names(hearing_df) <- gsub("group_general_health/",
                           "",
                           names(hearing_df))
  hearing_df
}