###################################################################
#' Check Enrollment Status of Screened PLAY Prospective Participant
#' @param site_id The five character PLAY site identifier; defaults to "NYUNI" for testing
#' @param subject_number The three digit number for this particular participant
screening_check_enrollment <- function(site_id = "NYUNI",
                                       subject_number = "017",
                                       play_site_csv = file.path(here::here(),
                                                                 "data", "csv", "play_site_vols.csv"),
                                       vb = TRUE) {
  stopifnot(is.character(site_id))
  stopifnot(is.character(play_site_csv))
  stopifnot(file.exists(play_site_csv))
  
  # Look up vol_id for this site_id
  sites <- readr::read_csv(play_site_csv, show_col_types = FALSE)
  this_site <- sites |>
    dplyr::filter(stringr::str_equal(play_site_id,
                                     paste0("PLAYProject_", site_id)))
  if (is.null(this_site)) {
    if (vb) message("No matching site found for site_id='", site_id, "'.")
    return(NULL)
  }
  this_vol_id <- this_site$play_vol_id
  if (vb) message("this_vol_id='", this_vol_id, "'")
  
  # Retrieve CSV for the site
  this_csv <-
    databraryr::download_session_csv(
      vol_id = this_site$play_vol_id,
      file_name = tempfile(),
      target_dir = tempdir(),
      vb = TRUE
    )
  # Filter CSV for specific participant
  
  # Extract relevant data elements
  
  # Return data elements
}