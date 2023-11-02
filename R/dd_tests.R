add_unique_vals_2_dd <- function(i, df, dd) {
  unique_vals <- unique(df[,i]) |> c()
  
  dd_row <- find_matching_dd_row(names(df)[i], dd)
  dd[dd_row, 'unique_vals'] <- as.list(unique_vals)
  dd
}

find_matching_dd_row <- function(var_name, dd) {
  match_index <- (1:dim(dd)[1])[dd$item == var_name]
  match_index
}

