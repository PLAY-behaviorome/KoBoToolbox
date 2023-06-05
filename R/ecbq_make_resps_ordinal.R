#' Make an ECBQ data frame 'longer' for comparative plotting across questions
#'
#' @param df A 'long' ECBQ data frame, e.g., from ecbq_clean_make_agg_df()
ecbq_make_resps_ordinal <-
  function(df) {
    stopifnot(!is.null(df))
    stopifnot(is.data.frame(df))
    stopifnot("question" %in% names(df))
    
    df |>
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
      ))
  }