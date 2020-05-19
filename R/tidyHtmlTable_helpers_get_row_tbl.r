get_row_tbl <- function(x,
                        rnames,
                        rgroup = NULL,
                        tspanner = NULL) {

  cols <- c(tspanner, rgroup, rnames)

  out <- x %>%
    dplyr::select(cols) %>%
    unique %>%
    dplyr::arrange_at(cols) %>%
    # This is necessary in order to not generate NA values when setting
    # hidden elements to ""
    dplyr::mutate_if(is.factor, as.character)

  out$r_idx <- 1:nrow(out)
  return(out)
}