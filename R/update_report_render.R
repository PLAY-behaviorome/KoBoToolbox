###################################################################
#' Updates data and renders report
update_report_render <- function(src_dir = 'src',
                                 open_rpt = TRUE,
                                 rpt_URL = 'docs/index.html') {
  
  assertthat::is.string(src_dir)
  assertthat::is.dir(src_dir)
  
  assertthat::assert_that(is.logical(open_rpt))
  
  assertthat::is.string(rpt_URL)
  assertthat::is.readable(rpt_URL)
  
  message("\n-------Updating data-------")
  library(targets)
  tar_make()
  
  message("\n-------Rendering report-------")
  bookdown::render_book(src_dir)
  
  if (open_rpt)
    browseURL(rpt_URL)
}
