screen_remove_metadata_fields <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(dplyr[select, contains])
  
  df |>
    dplyr::select(
      -dplyr::contains('note'),-dplyr::contains('instructions'),-dplyr::contains('acknowledge'),-dplyr::contains('screener'),-dplyr::contains('__'),-dplyr::contains('meta/instanceID'),-dplyr::contains('_uuid'),-dplyr::contains('_submission_time'),-dplyr::contains('_index'),-dplyr::contains('_parent_index'),-dplyr::contains('_tags'),-dplyr::contains('_version_'),
      # used to compute guid-dplyr::contains('day'),-dplyr::contains('concat1'),-dplyr::contains('Participant_ID_concat2'),
      # other meta-`_id`,-start,-end,-update_date,
      # Spanish-language version-dplyr::contains('NOTA')
    )
}
