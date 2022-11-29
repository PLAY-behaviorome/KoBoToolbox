# Cleaning raw questions

# Remove initial text and create new `edited_name` variable
drop_prelude_demo_quest <- function(df) {
  require(tidyverse)
  
  df$edited_name <- df$orig_name
  
  df <- df %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'PLAY Demographic Questionnaire/')) %>%
#    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'Last updated\\: March 4, 2020')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'Experimenter\\: __')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'A. Contact Information/')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'A[12]\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'Home Address/__A3\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'B[1-6]\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'C. Data Collection Site Information/')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'C[1-3]\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'C[456]?[a]?\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'C. Data Collection Site Information/')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'D. Child Information/')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'D[1-9]?[abc]?\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'E. Family Structure/')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'E[1-9]?[abc]?\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'F. Mother Information/')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'F[1-9]?[abc]?\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'G. Biological Mother Information/')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'G[1-9]?[abc]?\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'H[1-9]?[abc]?\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'I. Non\\-Biological Parent Partner Information/')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'I[1-9]?[abc]?\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'J. General Partner Information/')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'J[1-9]?[abc]?\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'K. Child Care Arrangements/')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'K[1-9]?[abc]?\\. ')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, '\\"')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, '[_]+')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, '\\.\\.\\.[0-9]+'))
  
  df
}

drop_prelude_12_English <- function(df) {
  require(tidyverse)
  
  df$edited_name <- df$orig_name
  
  df <- df %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'Combined Questionnaires \\(PLAY\\)/')) %>%
    #    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'Last updated\\: March 4, 2020')) %>%
    dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'Home Visit Questionnaires/'))
    # dplyr::mutate(., edited_name = stringr::str_remove_all(edited_name, 'A[1-9]+\\. '))

  df
}

# For the in-home visit questionnaires, the hierarchical structure can be exploited for 
# information about measures

add_measure_type <- function(df) {
  require(tidyverse)
  
  df <- df %>%
    dplyr::mutate(., measure_type = stringr::str_extract(edited_name, '^[a-zA-Z ]+'))
  df
}

clean_home_visit_demo <- function(df) {
  df1 <- drop_prelude_12_English(df) 
  add_measure_type(df1)
}
