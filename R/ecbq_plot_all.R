#' Generate a barplot of all 36 questions from the
#' Rothbart Early Childhood Questionnaire (ECBQ), Very Short
#' https://research.bowdoin.edu/rothbart-temperament-questionnaires/instrument-descriptions/the-early-childhood-behavior-questionnaire/
#'
#' @param df A dataframe with aggregate (across participants) ECBQ data
ecbq_plot_all <- function(df) {
  stopifnot(is.data.frame(df))
  require(ggplot2)
  
  if (!("question" %in% names(df))) {
    # Make longer so we can plot multiple questions
    df <- ecbq_make_longer(df)
  }
  
  df |>
    # Clean question names and responses
    ecbq_shorten_q_names() |>
    ecbq_make_resps_ordinal() |>
    # Plot
    ggplot() +
    aes(rating, fill = age_group) +
    ggplot2::geom_bar() +
    facet_wrap( ~ question, nrow = 12) +
    # https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    theme(axis.title.x = element_blank())
}
