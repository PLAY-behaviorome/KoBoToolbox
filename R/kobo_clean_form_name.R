#-------------------------------------------------------------------------------
#' Remove spaces and parentheses from a KoBoToolbox form name.
#'
#' @param kb_form_name A form name.
#' @return A string containing the reformatted filename.
kobo_clean_form_name <- function(kb_form_name) {
  if (!is.character(kb_form_name)) {
    stop('`kb_form_name` must be a character string')
  }
  
  fn <-
    stringr::str_replace_all(kb_form_name, pattern = "[ ]+", "_")
  stringr::str_replace_all(fn, pattern = '[\\(,\\)]', "")
}