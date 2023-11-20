###################################################################
post_visit_make_df <- function(kb_post_visit, xlsx_dir, csv_dir) {
  stopifnot(is.data.frame(kb_post_visit))
  stopifnot(dir.exists(xlsx_dir))
  stopifnot(dir.exists(csv_dir))

  retrieve_kobo_xlsx(kb_post_visit, xlsx_dir)
  load_xlsx_save_many_csvs(xlsx_dir, csv_dir, "Post\\-Visit")
  #clean_merge_post_visit()
  post_visit_csv <- list.files(csv_dir, "Post\\-Visit", full.names = TRUE)
  purrr::map(post_visit_csv, readr::read_csv, col_types = readr::cols(.default = 'c'),
             show_col_types = FALSE) |>
    purrr::list_rbind()
  # readr::read_csv(file.path(csv_dir, "361981_PLAY_Post-Visit_Notes.csv"),
  #                 show_col_types = FALSE)
}