###################################################################
#' Normalizes the file/form names of files on KoBoToolbox
#'
#' @param form_index An integer value indicating which row (KoBoToolbox form)
#' should be retrieved and saved. The default value for testing purposes is 13.
#' @param kb_df A data frame with the available KoBoToolbox data, typically
#' output of list_kobo_data(). The default is to call list_kobo_data().
#' @return An array of strings
create_cleaned_augmented_form_name <-
  function(form_index = 13,
           kb_df = list_kobo_data()) {
    if (!is.numeric(form_index)) {
      stop('`form_index` must be a number')
    }
    if (form_index <= 0) {
      stop('`form_index` must be > 0')
    }
    if (is.null(kb_df)) {
      message('Unable to return list of KoBoToolbox forms.')
      NULL
    } else {
      paste0(
        extract_kb_form_id(form_index, kb_df),
        "_",
        clean_kobo_form_name(extract_kb_form_name(form_index, kb_df))
      )
    }
  }
