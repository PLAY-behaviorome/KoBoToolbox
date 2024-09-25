div_labor_plot <- function(var_lbl = "laundry_self",
                           df,
                           levels = c(0:4),
                           labels = c("Never",
                                      "Some of the time",
                                      "About half the time",
                                      "Most of the time",
                                      "All of the time")) {
  
  assertthat::is.string(var_lbl)
  assertthat::assert_that(is.data.frame(df))
  
  df <- df |>
    dplyr::select(child_sex, age_group, {{ var_lbl }})
  
  df <-  df |>
    dplyr::filter(!is.na(.data[[var_lbl]])) |>
    dplyr::mutate(rating = factor(
      .data[[var_lbl]],
      levels = levels,
      labels = labels,
      ordered = TRUE
    ))
  
  df |>
    ggplot() +
    aes(x = rating, color = child_sex, fill = child_sex) +
    geom_bar() +
    geom_bar() +
    facet_grid(cols = vars(age_group), rows = vars(child_sex)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90))
}

div_labor_plot_by_person <- function(var_prefix = "laundry",
                           df,
                           levels = c(0:4),
                           labels = c("Never",
                                      "Some of the time",
                                      "About half the time",
                                      "Most of the time",
                                      "All of the time")) {
  
  assertthat::is.string(var_prefix)
  assertthat::assert_that(is.data.frame(df))
  
  df <- df |>
    dplyr::select(child_sex, age_group, starts_with(var_prefix))
  
  names(df) <- stringr::str_remove_all(names(df), paste0(var_prefix, "_"))
  
  df <- df |>
    tidyr::pivot_longer(cols = 3:5,
                        names_to = "person",
                        values_to = "rating")
  
  df <-  df |>
    dplyr::filter(!is.na(rating)) |>
    dplyr::mutate(rating = factor(
      rating,
      levels = levels,
      labels = labels,
      ordered = TRUE
    ))

  df |>
    ggplot() +
    aes(x = rating, color = child_sex, fill = child_sex) +
    geom_bar() +
    geom_bar() +
    facet_grid(cols = vars(age_group), rows = vars(person)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90))
}
