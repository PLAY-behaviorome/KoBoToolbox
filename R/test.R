library(tibble)
library(tidygeocoder)

address_single <- tibble::tibble(singlelineaddress = c(
  "608 E Prospect Ave, State College, PA",
  "600 Peachtree Street NE, Atlanta, Georgia"
))

census_s1 <- address_single %>%
  tidygeocoder::geocode(address = singlelineaddress, method = "census", verbose = TRUE)

census_s1

census_full1 <- address_single %>% geocode(
  address = singlelineaddress,
  method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies')
)

census_full1