###################################################################
#' Updates data and renders report
report_update_render <- function(src_dir = 'src',
                                 open_rpt = TRUE,
                                 rpt_URL = 'docs/index.html') {
  stopifnot(is.character(src_dir))
  stopifnot(dir.exists(src_dir))
  stopifnot(is.logical(open_rpt))
  stopifnot(is.character(rpt_URL))
  stopifnot(file.exists(rpt_URL))
  
  suppressPackageStartupMessages(require(targets))
  
  targets::tar_make()
  
  bookdown::render_book(src_dir)
  
  if (open_rpt)
    browseURL(rpt_URL)
}
