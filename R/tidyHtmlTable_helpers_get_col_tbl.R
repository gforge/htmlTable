get_col_tbl <- function(x,
                        header,
                        cgroup1 = NULL,
                        cgroup2 = NULL) {

  cols <- c(cgroup2, cgroup1, header)

  out <- x %>%
    dplyr::select(cols) %>%
    unique %>%
    dplyr::arrange_at(cols) %>%
    # This is necessary in order to not generate NA values when setting
    # hidden elements to ""
    dplyr::mutate_if(is.factor, as.character)

  out$c_idx <- 1:nrow(out)
  return(out)
}
