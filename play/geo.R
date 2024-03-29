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
  
  fns <-
    list.files(csv_dir, pattern = "Demographic_Questionnaire", full.names = TRUE)
  
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
  
  out_df <- df[i, ]
  if ((!is.na(out_df$parent_address_1) &&
       (!is.na(out_df$city) && (!is.na(out_df$state))))) {
    out_df$singlelineaddr <- with(out_df, paste0(parent_address_1, ", ", city, ", ", state))
  } else {
    out_df$singlelineaddr <- NA
  }
  out_df
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
#'@param show_qa_data Print feedback to console about success of address lookup.
#'Default is TRUE.
#'@param add_sub_site Add participant site_id and sub_num for merging with other
#'survey data. Default is TRUE.
#'@param return_fields Which Census geo codes to return. Possible values are
#'lat, long, id, match_indicator, match_type, matched_address, tiger_line_id,
#'tiger_side, state_fips, county_fips, census_tract, census_block, state.
#'For privacy reasons, we typically only return state_fips and county_fips
#'@returns A data frame of geographic info from the U.S. Census.
get_multiple_census_geos <- function(df,
                                     show_qa_data = TRUE,
                                     add_sub_site = FALSE,
                                     return_fields = c("state_fips", 
                                                       "county_fips")) {
  stopifnot(is.data.frame(df))
  
  box::use(tidygeocoder[geocode])
  box::use(dplyr[select])
  
  census_data <- df |>
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
  
  # # Add state explicitly to look up states in county
  # census_data$state = addrs$state
  # 
  # # Optionally add site and sub ids for linking to other PLAY data
  # if (add_sub_site) {
  #   census_data$site_id = addrs$site_id
  #   census_data$sub_num = addrs$sub_num
  # }
  
  census_df <- census_data |>
    dplyr::select(return_fields)
  cbind(df, census_df)
}

#-------------------------------------------------------------------------------
plot_screening_sites <- function(geo_df) {
  box::use(ggmap[get_map])
  box::use(dplyr[filter])
  box::use(ggplot2[...])
  
  geo_df <- geo_df |>
    dplyr::filter(!is.na(lat),!is.na(long))
  
  ggmap::get_map("USA", zoom = 4) |>
    ggmap::ggmap() +
    ggplot2::geom_point(data = geo_df, aes(x = long, y = lat))
}

#-------------------------------------------------------------------------------
get_county_in_state <-
  function(state_fips = "42",
           county_fips = "027") {
    box::use(tidycensus)
    box::use(dplyr[filter])
    
    tidycensus$fips_codes |>
      dplyr::filter(state_code == state_fips,
                    county_code == county_fips) |>
      dplyr::select(county)
  }

