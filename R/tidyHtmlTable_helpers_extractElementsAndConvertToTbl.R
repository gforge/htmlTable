#' Extract the elements and generate a table with unique elements
#'
#' @param x \code{list} with columns to be joined
#' @param elements \code{char} vector with the elements to select
prExtractElementsAndConvertToTbl <- function(x, elements) {
  x[elements] %>%
    prBindDataListIntoColumns() %>%
    dplyr::distinct()
}
