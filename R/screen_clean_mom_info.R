screen_clean_mom_info <- function(df) {
  stopifnot(is.data.frame(df))
  
  box::use(tidyr[unite])
  
  df |>
    tidyr::unite(col = "mom_childbirth_age", 
                 c("group_mominfo/mom_childbirth_age",
                   "parent_information/mother_information/mother_childbirth_age"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_race",
                 c("group_mominfo/mom_race", "parent_information/mother_information/mother_race"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_bio", c("group_mominfo/mom_biological", "group_mominfo/mom_relation"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_birth_country", c("group_mominfo/mom_birth_country", 
                                              "parent_information/mother_information/mother_birth_country"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_birth_country_specify", c("group_mominfo/specify_mom_birth_country",
                                                      "parent_information/mother_information/specify_mother_birth_country"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_education", c("group_mominfo/mom_education", "parent_information/mother_information/mother_education"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_employment", c("group_mominfo/mom_employment",
                                           "parent_information/mother_information/mother_employment"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_occupation", c("group_mominfo/mom_occupation", 
                                           "parent_information/mother_information/mother_occupation"), na.rm = TRUE) |>
    tidyr::unite(col = "mom_jobs_number", c("group_mominfo/mom_jobs_number",
                                            "parent_information/mother_information/mother_jobs_number"),
                 na.rm = TRUE) |>
    tidyr::unite(col = "mom_training", c("group_mominfo/mom_training", "parent_information/mother_information/mother_training"),
                 na.rm = TRUE)
}
