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
    return(x[[name]])
  }

  # We are one-caller removed from the original call so we need to
  # do this nasty hack to get the parameter of the parent function
  orgName <- eval(substitute(substitute(value)), envir = parent.frame())
  value <- dplyr::select(x, {{orgName}})
  stopifnot(ncol(value) <= maxCols)
  if (maxCols > 1) {
    return(value)
  }

  return(value[[1]])
}
