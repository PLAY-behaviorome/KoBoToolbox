#' Export Site-specific Data CSVs.
#' 
#' @param site_id A string. Five character site identifier.
#' @param vb Logical value. Show or do not show verbose output. Default is FALSE.
#' @returns Saves separate CSV files to screening/agg/by-site and home_visit/agg/by-site directories
export_site_csvs <- function(site_id, vb = FALSE) {
  
  # Check parameters
  assertthat::is.string(site_id)
  
  assertthat::assert_that(is.logical(vb))
  
  deps <- c("screen_select_site_data", "home_select_site_data", 
            "home_mbcdi_select_site_data" )
  unsourced_deps <- deps[!(deps %in% ls())]
  source_fns <- paste0("R/", unsourced_deps, ".R")
  
  purrr::walk(source_fns, source)
  
  if (vb) message("Generating CSVs for site: ", site_id)
  screen_select_site_data(site_id, vb = vb)
  home_select_site_data(site_id, vb = vb)
  home_mbcdi_select_site_data(site_id, vb = vb)
}
