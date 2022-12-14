make_address <- function(i, df) {
  require(dplyr)
  require(tibble)
  this_row <- df[i, ]
  out_df <- this_row %>%
    dplyr::select(
      .,
      addr1 = `play_demo_questionnaire/group_contact_info/group_address/parent_address_1`,
      addr2 = `play_demo_questionnaire/group_contact_info/group_address/parent_address_2`,
      city = `play_demo_questionnaire/group_contact_info/group_address/city`,
      state = `play_demo_questionnaire/group_contact_info/group_address/state`
    )
  if ((!is.na(out_df$addr1) && (!is.na(out_df$city) && (!is.na(out_df$state))))) {
    addr <- with(out_df, paste0(addr1, ", ", city, ", ", state))
    tibble(singlelineaddr = addr)
  } else {
    NULL
  }
}

make_addresses <- function(df) {
  require(purrr)
  purrr::map_df(1:dim(df)[1], make_address, df)
}

get_single_census_geo <- function(addr) {
  require(tidygeocoder)
  require(tibble)
  addr %>%
    tidygeocoder::geocode(
      address = singlelineaddr,
      method = "census",
      full_results = TRUE,
      api_options = list(census_return_type = 'geographies')
    )
}

get_multiple_census_geos <- function(addrs) {
  require(tidygeocoder)
  addrs %>% tidygeocoder::geocode(
    address = singlelineaddr,
    method = "census",
    full_results = TRUE,
    api_options = list(census_return_type = 'geographies')
  )
}

# For mapping see <https://mhallwor.github.io/_pages/Tidyverse_intro>
# <https://www.infoworld.com/article/3644848/astonishingly-easy-mapping-in-r-with-mapview.html>
# https://bookdown.org/rdpeng/RProgDA/mapping.html

plot_screening_sites <- function(geo_df) {
  require(ggmap)
  require(dplyr)
  require(ggplot2)
  
  usa <- ggmap::get_map("USA", zoom=4)
  usa %>%
    ggmap::ggmap() +
    ggplot2::geom_point(data = geo_df, aes(x = long, y = lat))
}