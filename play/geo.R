#-------------------------------------------------------------------------------
# geo.R
#
# A set of functions used to process PLAY project data and gather geographic-
# related information about the participants and families.
# 
# These functions make use of the `box` package.
#
# To invoke the `geo` module, execute `box::use(./play/geo)` in the root
# directory.

#-------------------------------------------------------------------------------
#' Make a data frame for the address extraction workflow
#' 
#' 
make_addr_df <- function(csv_dir = "data/csv/screening") {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  box::use(purrr[map, list_rbind])
  box::use(readr[read_csv])
  
  fns <- list.files(csv_dir, full.names = TRUE)
  
  df1 <- readr::read_csv(fns[1], show_col_types = FALSE)
  df2 <- readr::read_csv(fns[2], show_col_types = FALSE)
  df3 <- readr::read_csv(fns[3], show_col_types = FALSE)
  
  ad1 <- make_addresses(df1, 'old')
  ad2 <- make_addresses(df2, 'new')
  ad3 <- make_addresses(df3, 'new')
  
  rbind(ad1, ad2, ad3)
}

#-------------------------------------------------------------------------------
#' Make a data frame of recruiting call addresses.
#'
#'@param i Row index into the data frame
#'@param df Data frame
#'@param survey_type Are the data coming from the original screening/demographic
#'survey or the 'new' one?
#'@returns A data frame or tibble with address information.
make_address <- function(i, df, survey_type = 'new') {
  stopifnot(is.numeric(i))
  stopifnot(i >= 1)
  stopifnot(is.data.frame(df))
  stopifnot(is.character(survey_type))
  stopifnot(survey_type %in% c('new', 'old'))
  
  box::use(dplyr[select])
  box::use(tibble[tibble])
  
  this_row <- df[i,]
  if (survey_type == 'new') {
    out_df <- this_row |>
      dplyr::select(
        site_id = `play_demo_questionnaire/group_siteinfo/site_id`,
        sub_num = `play_demo_questionnaire/group_siteinfo/subject_number`,
        addr1 = `play_demo_questionnaire/group_contact_info/group_address/parent_address_1`,
        addr2 = `play_demo_questionnaire/group_contact_info/group_address/parent_address_2`,
        city = `play_demo_questionnaire/group_contact_info/group_address/city`,
        state = `play_demo_questionnaire/group_contact_info/group_address/state`
      )
  } else {
    out_df <- this_row |>
      dplyr::select(
        site_id = `play_phone_questionnaire/group_siteinfo/site_id`,                                           
        sub_num = `play_phone_questionnaire/group_siteinfo/subject_number`,
        addr1 = `play_phone_questionnaire/group_contact_info/group_address/parent_address_1`,
        addr2 = `play_phone_questionnaire/group_contact_info/group_address/parent_address_2`,
        city = `play_phone_questionnaire/group_contact_info/group_address/city`,
        state = `play_phone_questionnaire/group_contact_info/group_address/state`
      )
  }
  
  if ((!is.na(out_df$addr1) &&
       (!is.na(out_df$city) && (!is.na(out_df$state))))) {
    addr <- with(out_df, paste0(addr1, ", ", city, ", ", state))
    tibble(singlelineaddr = addr, 
           site_id = out_df$site_id, 
           sub_id = out_df$sub_num,
           state = out_df$state)
  } else {
    NULL
  }
}

#-------------------------------------------------------------------------------
#' Make a new data frame with address information from the demographic/survey
#' data.
#' @param df A data frame with PLAY demographic/survey data.
#' @param survey_type The type ("new", "old") of demographic survey.
#' @returns A data frame/tibble with the address information in a single line.
make_addresses <- function(df, survey_type) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(survey_type))
  
  box::use(purrr[map_df])

  purrr::map_df(1:dim(df)[1], make_address, df, survey_type)
}

#-------------------------------------------------------------------------------
#' Acquire geographic info for a data frame of addresses from the U.S. Census.
#'
#'@param addrs A data frame with a single field with each address in a single
#'string.
#'@returns A data frame of geographic info from the U.S. Census.
get_multiple_census_geos <- function(addrs, show_qa_data = TRUE, 
                                     add_sub_site = FALSE) {
  stopifnot(is.data.frame(addrs))
  
  box::use(tidygeocoder[geocode])
  
  census_data <- addrs |> 
    dplyr::select(singlelineaddr) |>
    tidygeocoder::geocode(
    address = singlelineaddr,
    method = "census",
    full_results = TRUE,
    api_options = list(census_return_type = 'geographies')
  )
  
  if (show_qa_data) {
    n_addr <- dim(census_data)[1]
    n_match <- sum(census_data$match_indicator == "Match")
    message("Matched ", n_match, " addresses out of ", n_addr, " provided")
  }
  
  # Add state explicitly to look up states in county
  census_data$state = addrs$state
  
  # Optionally add site and sub ids for linking to other PLAY data
  if (add_sub_site) {
    census_data$site_id = addrs$site_id
    census_data$sub_id = addrs$sub_id
    
  }
  
  census_data
}

#-------------------------------------------------------------------------------
plot_screening_sites <- function(geo_df) {
  
  box::use(ggmap[get_map])
  box::use(dplyr[filter])
  box::use(ggplot2[...])

  geo_df <- geo_df |>
    dplyr::filter(!is.na(lat), !is.na(long))
  
  ggmap::get_map("USA", zoom = 4) |>
    ggmap::ggmap() +
    ggplot2::geom_point(data = geo_df, aes(x = long, y = lat))
}

#-------------------------------------------------------------------------------
get_county_in_state <- function(state_fips = "42", county_fips = "027") {
  
  box::use(tidycensus)
  box::use(dplyr[filter])
  
  tidycensus$fips_codes |>
    dplyr::filter(state_code == state_fips,
                  county_code == county_fips) |>
    dplyr::select(county)
}
