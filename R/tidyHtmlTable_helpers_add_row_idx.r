add_row_idx <- function(x,
                        rnames,
                        rgroup = NULL,
                        tspanner = NULL) {

  cols <- c(tspanner, rgroup, rnames)

  row_idx_df <- x %>%
    get_row_tbl(rnames = rnames,
                rgroup = rgroup,
                tspanner = tspanner)

  out <- suppressWarnings(
    x %>%
      dplyr::left_join(row_idx_df, by = cols)
  )
  return(out)
}
