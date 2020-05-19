add_col_idx <- function(x,
                        header,
                        cgroup1 = NULL,
                        cgroup2 = NULL) {

  cols <- c(cgroup2, cgroup1, header)

  col_idx_df <- x %>%
    get_col_tbl(header = header,
                cgroup1 = cgroup1,
                cgroup2 = cgroup2)

  out <- suppressWarnings(
    x %>%
      dplyr::left_join(col_idx_df, cols)
  )
  return(out)
}