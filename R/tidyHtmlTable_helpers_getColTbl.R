getColTbl <- function(x) {
  out <- prExtractElementsAndConvertToTbl(x, elements = c("cgroup", "header")) %>%
    dplyr::arrange() %>%
    # This is necessary in order to not generate NA values when setting
    # hidden elements to ""
    dplyr::mutate_if(is.factor, as.character)

  out$c_idx <- 1:nrow(out)
  return(out)
}
