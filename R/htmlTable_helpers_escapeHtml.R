#' Remove html entities from table
#'
#' Removes the htmlEntities from table input data. Note that
#' this also replaces $ signs in order to remove the MathJax
#' issue.
#'
#' @importFrom htmltools htmlEscape
#'
#' @inheritParams htmlTable
#' @return `x` without the html entities
#' @family hidden helper functions for htmlTable
prEscapeHtml <- function(x) {
  attributes_x <- attributes(x)
  x <- lapply(x, htmlEscape)
  x <- lapply(x, function(x) str_replace_all(x, "\\$", "&#36;"))
  attributes(x) <- attributes_x
  return(x)
}
