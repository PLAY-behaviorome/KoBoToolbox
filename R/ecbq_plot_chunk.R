ecbq_plot_chunk <- function(var, q, dataset) {
  a <- knitr::knit_expand(
    text = c(
      "## Barplot for {{this_var}}",
      "\n",
      q,
      "\n",
      ecbq_plot(var_lbl = var, df = dataset),
      "\n"
    ),
    this_var = var
  )
  
  cat(a, sep = "\n")
}