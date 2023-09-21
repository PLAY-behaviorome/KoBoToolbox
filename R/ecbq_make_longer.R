#' Make an ECBQ data frame 'longer' for comparative plotting across questions
#' 
#' @param ecbq_df A 'wide' ECBQ data frame, e.g., from ecbq_clean_make_agg_df()
ecbq_make_longer <- function(ecbq_df) {
  stopifnot(!is.null(ecbq_df))
  stopifnot(is.data.frame(ecbq_df))
  
  ecbq_df |>
    tidyr::pivot_longer(
      cols = contains('rothbart'),
      names_to = 'question',
      values_to = 'rating'
    ) |>
    dplyr::filter(!is.na('rating'),!stringr::str_detect(question, 'comments')) |>
    dplyr::arrange(question)
}