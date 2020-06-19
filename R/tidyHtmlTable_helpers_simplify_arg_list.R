# Converts arguments from ... into a list and removes those that have been set
# to NULL
simplify_arg_list <- function(...) {
  x <- list(...)
  idx <- sapply(x, is.null)
  return(x[!idx])
}
