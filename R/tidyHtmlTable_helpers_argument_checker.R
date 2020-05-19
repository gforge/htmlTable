# Checks a variety of assumptions about input arguments and prepares an
# appropriate error message if those assumptions are violated
argument_checker <- function(x, ...) {

  # Check that all the input are characters
  all_args <- simplify_arg_list(...)
  idx <- which(!sapply(all_args, is.character))

  if (length(idx) > 0) {
    stop(
      "The following parameters must be of type character: ",
      paste(names(all_args)[idx], collapse = ", ")
    )
  }

  # Check that all of the arguments that would be used map columns to
  # character attributes are of length 1
  col_vars <- get_col_vars(...)

  idx <- which(sapply(col_vars, length) > 1)
  if (length(idx) > 0) {
    stop(
      "The following parameters must be of length 1: ",
      paste(names(col_vars)[idx], collapse = ", ")
    )
  }

  # Find column variables that are not columns in the dataset
  idx <- which(!(as.character(col_vars) %in% colnames(x)))
  if (length(idx) > 0) {
    stop(
      "The following arguments need values that correspond to column ",
      "names in x: ",
      paste0(names(col_vars), " = ",
        as.character(col_vars),
        collapse = ", "
      )
    )
  }
}