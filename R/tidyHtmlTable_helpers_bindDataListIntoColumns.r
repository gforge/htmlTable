#' Merge columns into a tibble
#'
#' Almost the same as \code{\link[tibble]{tibble}} but it solves the issue
#' with some of the arguments being columns and some just being vectors.
#'
#' @param dataList \code{list} with the columns/data.frames
#' @return \code{data.frame} object
prBindDataListIntoColumns <- function(dataList) {
  stopifnot(is.list(dataList))
  dataList %>%
    purrr::keep(~ !is.null(.)) %>%
    do.call(dplyr::bind_cols, .) %>%
    tibble::as_tibble()
}
