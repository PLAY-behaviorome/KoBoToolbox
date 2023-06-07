################################################################################
#' Takes an array of strings with language input responses and returns
#' a data fram about what language(s) are in the array.
#'
#' @param lang_str A an array of strings with language names, e.g., from the
#' Screening/Demographic survey.
#' @param contex_str A string indicating whether the language context was in the
#' home 'home' or to the child 'to_child'. These are appended to the output
#' variable names.
#' @returns A data frame with information about the language(s) are in the array.
screening_clean_lang_data <-
  function(lang_str, context_str = "home") {
    stopifnot(is.character(lang_str))
    stopifnot(is.character(context_str))
    
    if (!(context_str %in% c('home', 'to_child'))) {
      message("`context_str` not `home` or `to_child`.")
    }
    
    suppressPackageStartupMessages(require(stringr))
    suppressPackageStartupMessages(require(tibble))
    
    english_spoken <- str_detect(lang_str, "[Ee]nglish")
    spanish_spoken <- str_detect(lang_str, "[Ss]panish")
    other_spoken <- str_detect(lang_str, "[Oo]nglish")
    n_langs_spoken <-
      sum(english_spoken, spanish_spoken, other_spoken)
    
    df <-
      tibble::tibble(lang_spoken = lang_str,
                     english_spoken,
                     spanish_spoken,
                     other_spoken,
                     n_langs_spoken)
    names(df) <- paste0(names(df), "_", context_str)
    df
  }
