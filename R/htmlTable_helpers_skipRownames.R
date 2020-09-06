#' Returns if rownames should be printed for the htmlTable
#'
#' @inheritParams htmlTable
#' @keywords internal
prSkipRownames <- function(rnames) {
  if (missing(rnames) || is.null(rnames) || length(rnames) == 0) {
    return(TRUE)
  }

  if (length(rnames) == 1 && rnames == FALSE) {
    return(TRUE)
  }

  return(FALSE)
}
