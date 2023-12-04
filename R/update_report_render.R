###################################################################
#' Updates data and renders report
update_report_render <- function(src_dir = 'src',
                                 open_rpt = TRUE,
                                 rpt_URL = 'docs/index.html') {
  # stopifnot(is.character(src_dir))
  # stopifnot(dir.exists(src_dir))
  # stopifnot(is.logical(open_rpt))
  # stopifnot(is.character(rpt_URL))
  # stopifnot(file.exists(rpt_URL))
  
  assertthat::is.string(src_dir)
  assertthat::is.dir(src_dir)
  
  assertthat::assert_that(is.logical(open_rpt))
  
  assertthat::is.string(rpt_URL)
  assertthat::is.readable(rpt_URL)
  
  message("\n\n-------Updating data-------")
  library(targets)
  tar_make()
  
  message("\n\n-------Rendering report-------")
  bookdown::render_book(src_dir)
  
  if (open_rpt)
    browseURL(rpt_URL)
}
