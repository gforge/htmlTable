#' A simple function for joining two tables by their
#' intersected columns
#'
#' @param x \code{data.frame}
#' @param y \code{data.frame}
#' @return \code{data.frame}
innerJoinByCommonCols <- function(x, y) {
  by <- intersect(names(x), names(y))
  dplyr::inner_join(x, y, by = by)
}
