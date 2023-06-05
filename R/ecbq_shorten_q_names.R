#' Make an ECBQ data frame 'longer' for comparative plotting across questions
#'
#' @param df A 'long' ECBQ data frame, e.g., from ecbq_clean_make_agg_df()
#' @param q_pattern A string to replace in the values for the 'question' field. 
#' Default is 'rothbart_'.
#' @param q_replace String to replace for values in `q_pattern`. Default is "", 
#' but "ecbq_" is a logical alternative.
ecbq_shorten_q_names <-
  function(df,
           q_pattern = "rothbart_",
           q_replace = "") {
    stopifnot(!is.null(df))
    stopifnot(is.data.frame(df))
    stopifnot("question" %in% names(df))
    
    df |>
      dplyr::mutate(question = stringr::str_replace_all(question, q_pattern, q_replace))
  }