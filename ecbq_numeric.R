ecbq_numeric <- function(var_lbl = "unfamiliarperson",
                         df,
                         levels = c(1:7),
                         labels = c(
                           "never",
                           "veryrarely",
                           "lessthanhalf",
                           "abouthalf",
                           "morethanhalf",
                           "almostalways",
                           "always"
                         )) {
  
  assertthat::is.string(var_lbl)
  assertthat::assert_that(is.data.frame(df))
  
  df <- df |>
    dplyr::select({{ var_lbl }}) |>
    dplyr::filter(!is.na(.data[[var_lbl]])) |>
    dplyr::mutate("num_{{var_lbl}}" := 
                    dplyr::case_match({{var_lbl}},
                      "never" ~ 1,
                      "veryrarely" ~ 2,
                      "lessthanhalf" ~ 3,
                      "abouthalf" ~ 4,
                      "morethanhalf" ~ 5,
                      "almostalways" ~ 6,
                      "always" ~ 7,
                      "na" ~ NA))
  
      df
}