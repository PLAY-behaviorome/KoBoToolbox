home_mbcdi_select_site_data <-
  function(this_site_id = "UCSCR",
           in_dir = "data/csv/home_visit/agg",
           out_dir = "data/csv/home_visit/agg/by-site",
           vb = FALSE) {
    assertthat::is.string(this_site_id)
    
    assertthat::is.string(in_dir)
    assertthat::is.readable(in_dir)

    assertthat::is.string(out_dir)
    assertthat::is.readable(out_dir)
    
    #--------------------------------------------------------------------------
    # Helper function
    process_single_mcdi_file <-
      function(in_fn = "mcdi_english_12_combined.csv",
               this_site_id = "UCSCR",
               in_dir = "data/csv/home_visit/agg",
               out_dir = "data/csv/home_visit/agg/by-site",
               out_fn = paste0("PLAY-", tools::file_path_sans_ext(in_fn)),
               vb = FALSE) {
        out_fn <- paste0(out_dir, "/", out_fn, "-", this_site_id, ".csv")
        
        in_fn_full <- paste0(in_dir, "/", in_fn)
        
        if (vb)
          message("Reading MCDI data file: '", in_fn_full, "'")
        df <-
          readr::read_csv(
            paste0(in_dir, "/", in_fn),
            col_types = readr::cols(.default = 'c'),
            show_col_types = FALSE
          )
        
        if (is.data.frame(df)) {
          if (vb)
            message("Filtering for data from site: ", this_site_id)
          df |>
            dplyr::filter(site_id == this_site_id) |>
            readr::write_csv(out_fn)
          out_fn
        } else {
          message("No data returned from '", in_fn_full, "'")
        }
      }
    
    purrr::walk(
      list.files(in_dir, "mcdi_english"),
      process_single_mcdi_file,
      this_site_id = this_site_id,
      in_dir = in_dir,
      out_dir = out_dir,
      vb = vb
    )
  }
#--------------------------------------------------------------------------
