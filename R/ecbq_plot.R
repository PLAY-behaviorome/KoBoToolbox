#' Generate a barplot of a single question from the
#' Rothbart Early Childhood Questionnaire (ECBQ)
#'
#' @param df A data frame with aggregate (across participants) ecbq data
ecbq_plot <- function(var_lbl = "rothbart_unfamiliarperson", df) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(var_lbl))
  
  # df |> select(participant_id, child_sex, age_group, tidyr::matches(var_lbl))
  # for matching by variable name
  
  require(ggplot2)
  
  df <-
    df |> dplyr::select(participant_id,
                 child_sex,
                 age_group,
                 {{ var_lbl }})
  
  df <- df |>
    dplyr::filter(!is.na(.data[[var_lbl]])) |>
    dplyr::mutate(rating = factor(
      .data[[var_lbl]],
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
    aes(x = rating, fill = child_sex) +
    geom_bar() +
    facet_grid(cols = vars(age_group), rows = vars(child_sex)) +
    # https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    theme(legend.position = "none") +
    ggtitle(paste0("Ratings of '", {{var_lbl}}, "'")) +
    xlab("")
}
