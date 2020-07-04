#' A simple function for joining two tables by their
#' intersected columns
#'
#' @param x `data.frame`
#' @param y `data.frame`
#' @return `data.frame`
innerJoinByCommonCols <- function(x, y) {
  by <- intersect(names(x), names(y))
  dplyr::inner_join(x, y, by = by)
}
