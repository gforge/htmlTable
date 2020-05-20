getRowTbl <- function(x) {
  cols <- c("tspanner", "rgroup", "rnames")

  out <- do.call(cbind, x %>% extract(cols)) %>%
    dplyr::arrange_at(vars(any_of(cols))) %>%
    # This is necessary in order to not generate NA values when setting
    # hidden elements to ""
    dplyr::mutate_if(is.factor, as.character)

  out$r_idx <- 1:nrow(out)
  return(out)
}