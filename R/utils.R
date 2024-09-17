# R/utils.R

find_str_in_file <- function(fn, targ_str) {
  stopifnot(is.character(targ_str))
  stopifnot(is.character(fn))
  stopifnot(file.exists(fn))
  
  message("In: '", fn, "'")
  system(paste0("grep ", targ_str, " ", fn))
}

remove_technology_use_scale <- function(df) {
  dplyr::select(df, -contains('technology_use_scale'))
}

remove_doctor_told_you <- function(df) {
  dplyr::select(df, -contains('doctor_told_you'))
}

remove_databrary_fields <- function(df) {
  dplyr::select(df, -contains('group_databrary'))
}

reconcile_typicalday <- function(df) {
  names(df) <- stringr::str_replace_all(names(df), 'typicalday', 'typical_day')
  df
}

remove_permissive_locomotor_milestones_label <- function(df) {
  old_names <- names(df)
  new_names <- old_names
  contains_locomotor <-
    stringr::str_detect(new_names, pattern = "locomotor_milestones.*health|division|rothbart|mediause|pets|typical|acknowledge")
  new_names[contains_locomotor] <-
    stringr::str_remove(new_names[contains_locomotor], "group_locomotor_milestones\\.")
  names(df) <- new_names
  df
}

remove_X_meta_cols <- function(df) {
  dplyr::select(df, -contains("X_"), -contains("meta.instanceID"))
}

remove_redundant_group_labels <- function(df) {
  names(df) <- stringr::str_remove_all(names(df), 'group_homevisitquestionnaires\\.')
  names(df) <- stringr::str_remove_all(names(df), 'group_combinedquestionnaires\\.')
  names(df) <- stringr::str_remove_all(names(df), "group_")
  df
}

clean_dfs <- function(df) {
  df %>%
    reconcile_typicalday() %>%
    remove_technology_use_scale() %>%
    remove_doctor_told_you() %>%
    remove_permissive_locomotor_milestones_label() %>%
    remove_databrary_fields() %>%
    remove_X_meta_cols() %>%
    remove_redundant_group_labels()
}