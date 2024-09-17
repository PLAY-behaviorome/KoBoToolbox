###################################################################
#' Standardizes the format of the home visit questionnaire XLSX
#' data files.
#'
#' @param in_dir Directory where the XLSX files are stored
#' @return Results of `file.rename()`
rename_home_xlsx <- function(fl, out_dir) {
  require(stringr)
  require(purrr)
  #  stopifnot(is.character(fl))
  stopifnot(dir.exists(out_dir))
  
  # fl <- list.files(in_dir, full.names = TRUE)
  # fl_home_old <- unlist(fl[stringr::str_detect(fl, 'Home')])
  fl_home_old <- fl[stringr::str_detect(fl, 'Home')]
  fl_home_new <-
    purrr::map_chr(fl_home_old, make_standard_form_name)
  fl_home_new <- file.path(out_dir, fl_home_new)
  file.rename(fl_home_old, fl_home_new)
  fl_home_new
}

###################################################################
#' Helper function called by rename_home_xlsx() to create standard
#' file name.
#'
#' @param fn Old file name
#' @return New file name and path
make_standard_form_name <- function(fn) {
  stopifnot(is.character(fn))
  
  this_dir <- dirname(fn)
  this_fn <- basename(fn)
  form_id <- stringr::str_extract(this_fn, '[0-9]+')
  fn <- paste0(
    form_id,
    "_PLAY_HomeQuestionnares",
    "_",
    extract_age_group_from_name(this_fn),
    "_",
    form_language(this_fn),
    '.xlsx'
  )
  #  file.path(this_dir, fn)
  fn
}

###################################################################
#' Helper function to extract the age group from a file name.
#'
#' @param form_name File (form) name.
#' @return Age group as a string
extract_age_group_from_name <- function(form_name) {
  stopifnot(is.character(form_name))
  
  age_grps <-
    stringr::str_match(form_name, "[ _\\(]+(12|18|24)[ _]+")
  age_grps[, 2]
}

###################################################################
#' Helper function to detect whether a file/form contains data from
#' a bilingual participant family based on the file/form name.
#'
#' @param form_name
#' @return Logical vector of names that contain the detected string.
form_is_bilingual <- function(form_name) {
  stopifnot(is.character(form_name))
  
  stringr::str_detect(form_name, "[Bb]ilingual")
}

###################################################################
#' Helper function to detect whether a file/form contains data from
#' a Spanish-speaking family.
#'
#' @param form_name
#' @return Logical vector of names that contain the detected string.
form_is_spanish <- function(form_name) {
  stopifnot(is.character(form_name))
  
  stringr::str_detect(form_name, "[Ss]panish")
}

###################################################################
#' Helper function to determine the language spoken by the participant
#' family based on information in the file/form name.
#'
#' @param form_name
#' @return String with appropriate language indicator.
form_language <- function(form_name) {
  stopifnot(is.character(form_name))
  
  is_bilingual <- form_is_bilingual(form_name)
  is_spanish <- form_is_spanish(form_name)
  form_lang <- rep("english", length(form_name))
  form_lang[is_bilingual & is_spanish] <- "bilingual_spanish"
  form_lang[is_bilingual & !is_spanish] <- "bilingual_english"
  form_lang
}
