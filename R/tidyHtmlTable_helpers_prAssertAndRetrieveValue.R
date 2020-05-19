prAssertAndRetrieveValue <- function(x,
                                     value,
                                     name = deparse(substitute(value)),
                                     maxCols = 1,
                                     optional = FALSE) {
  if (missing(value)) {
    if (is.null(x[[name]])) {
      if (optional) {
        return(NULL)
      }

      stop(
        "You have not provided an argument",
        " and the data frame does not have a '", name, "' column"
      )
    }
    value <- x$value
  } else {
    value <- dplyr::select(x, {{ value }})
    stopifnot(ncol(value) == maxCols)
    if (maxCols > 1) {
      return(value)
    }
    value <- value[[1]]
  }
  return(value)
}