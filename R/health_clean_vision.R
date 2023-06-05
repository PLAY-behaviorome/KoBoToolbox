health_clean_vision <- function(df) {
  vision_df <- df |>
    dplyr::select(contains("vision_tested"),
                  -contains("birthhospital"),
                  -contains("afterhome"),
                  -contains("no"),
                  -contains("refused"),
                  -contains("donotknow"))
  names(vision_df) <- gsub("group_general_health/",
                           "",
                           names(vision_df))
  vision_df
}