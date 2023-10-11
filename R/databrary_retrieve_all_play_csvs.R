databrary_retrieve_all_play_csvs <-
  function(save_fn = "data/csv/play_sessions.csv", vb = FALSE) {
    assertthat::is.string(save_fn)
    assertthat::is.writeable(save_fn)
    
    play <- readr::read_csv("data/csv/play_site_vols.csv",
                            show_col_types = FALSE)
    
    if (vb) message("Retrieving n= ", dim(play)[1], " session CSVs from Databrary.")
    df <-
      purrr::map(play$play_vol_id, databrary_retrieve_vol_sess, .progress = TRUE) |>
      purrr::list_rbind()
    
    if (assertthat::not_empty(df)) {
      readr::write_csv(df, save_fn)
      message("Saved '", save_fn, "'.")
    } else {
      warning("Unable to save '", save_fn, "'.")
      NULL
    }
  }