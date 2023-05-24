#' Generate a barplot of a single question from the
#' Rothbart Early Childhood Questionnaire (EBQ)
#' 
#' @param df A dataframe with aggregate (across participants) EBQ data
ebq_plot_all <- function(df) {
  
  stopifnot(is.data.frame(df))
  require(ggplot2)
  
  # Make longer so we can plot multiple questions
  df <- df |>
    tidyr::pivot_longer(cols = contains('rothbart'), 
                        names_to = 'question', values_to = 'rating') |>
    dplyr::mutate(rating = factor(
      rating,
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
    )) |>
    dplyr::filter(!is.na('rating'),
                  !stringr::str_detect(question, 'comments')) |>
    dplyr::arrange(question)
  
  df
  # ggplot(df) +
  #   aes(rating, fill = age_group) +
  #   geom_bar() +
  #   facet_wrap(~ question) +
  #   # https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
  #   scale_x_discrete(guide = guide_axis(angle = 90))
}
