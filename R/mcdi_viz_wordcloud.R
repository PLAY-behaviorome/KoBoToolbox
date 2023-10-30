mcdi_viz_wordcloud <- function(wd_ct,
                               n_participants,
                               quantile = .25,
                               my_colors = RColorBrewer::brewer.pal(12, 'Paired')) {
  
  assertthat::is.number(quantile)
  assertthat::assert_that(quantile > 0)
  assertthat::assert_that(quantile < 1)
  
  #n_words <- (dim(wd_ct)[1])
  my_thresh <- (round(n_participants*(quantile), 0))
  
  if ((my_thresh) > max(wd_ct$n)) {
    message("No words meet criterion: n = ", my_thresh, " 'know/say' out of n = ", n_participants, ' participants')
    message("Try quantile < ", round(max(wd_ct$n)/n_participants, 2))
    return(NULL)
  }
  
  wordcloud::wordcloud(
    words = wd_ct$word,
    freq = wd_ct$n,
    min.freq = my_thresh,
    colors = my_colors
  )
}