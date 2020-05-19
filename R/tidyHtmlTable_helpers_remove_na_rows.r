# Removes rows containing NA values in any mapped columns from the tidy dataset
remove_na_rows <- function(x, ...) {
  cols <- as.character(get_col_vars(...))
  na.log <- x %>%
    dplyr::select(cols) %>%
    is.na

  na.row.sums <- na.log %>%
    rowSums

  keep.idx <- na.row.sums == 0
  removed <- sum(na.row.sums > 0)

  if (removed != 0) {
    na.col.sums <- na.log %>%
      colSums
    na.cols <- colnames(na.log)[na.col.sums > 0]
    warning(paste0("NA values were detected in the following columns of ",
                   "the tidy dataset: ",
                   paste(na.cols, collapse = ", "), ". ",
                   removed, " row(s) in the tidy dataset were removed."))
  }
  return(x %>% dplyr::filter(keep.idx))
}