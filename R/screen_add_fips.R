screen_add_fips <- function(df) {
  stopifnot(is.data.frame(df))
  
  have_valid_addr <- !is.na(df$parent_address_1) &
    !is.na(df$city) & !is.na(df$state)
  
  single_addr <- df |>
    dplyr::filter(have_valid_addr) |>
    dplyr::mutate(singlelineaddress = paste0(parent_address_1, ",", city, ",", state)) |>
    dplyr::select(singlelineaddress)
  single_addr
  
  census_full <- single_addr |>
    tidygeocoder::geocode(
      address = singlelineaddress,
      method = "census",
      full_results = TRUE,
      api_options = list(census_return_type = 'geographies')
    )
  
  # Initialize for NA/NULL values
  df$state_fips = NA
  df$county_fips = NA
  
  df |>
    dplyr::filter(have_valid_addr) |>
    dplyr::mutate(fips_state = census_full$state_fips,
                  fips_county = census_full$county_fips)
}
