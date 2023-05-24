#' Generate a barplot of a single question from the
#' Rothbart Early Childhood Questionnaire (EBQ)
#' 
#' @param df A dataframe with aggregate (across participants) EBQ data
ebq_plot <- function(df) {
  
  stopifnot(is.data.frame(df))
  require(ggplot2)
  
  df <- df |>
    dplyr::filter(!is.na(rothbart_unfamiliarperson)) |>
    dplyr::mutate(rothbart_unfamiliarperson = factor(
      rothbart_unfamiliarperson,
      c(
        "never",
        "very_rarely",
        "less_than_half",
        "about_half",
        "more_than_half",
        "almost_always",
        "always"
      ),
      ordered = TRUE
    ))
  
  ggplot(df) +
    aes(rothbart_unfamiliarperson) +
    geom_bar() +
    facet_grid(rows = ~ age_group) +
    # https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
    scale_x_discrete(guide = guide_axis(angle = 90))
}
