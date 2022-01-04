#' @export
#' @rdname txtRound
#' @section Tidy-select with `data.frame`:
#'
#' The `txtRound` can use `data.frame` for input. This allows us to use
#' [tidyselect](https://tidyselect.r-lib.org/articles/tidyselect.html)
#' patterns as popularized by **dplyr**.
#'
#' @examples
#'
#' # Using a data.frame directly
#' library(magrittr)
#' data("mtcars")
#' # If we want to round all the numerical values
#' mtcars %>%
#'   txtRound(digits = 1)
#'
#' # If we want only want to round some columns
#' mtcars %>%
#'   txtRound(wt, qsec_txt = qsec, digits = 1)
#' @importFrom methods formalArgs
txtRound.data.frame <- function(x, ..., digits = 0L){
  safeLoadPkg("tidyselect")
  vars <- tidyselect::eval_select(rlang::expr(c(...)), x)
  vars <- vars[!(names(vars) %in% formalArgs(txtRound.default))]

  if (length(vars) == 0) {
    vars <- sapply(x, is.numeric)
    vars <- sapply(names(vars)[vars], function(cn) which(cn == colnames(x)))
  }

  call <- as.list(match.call())

  # Drop function & x call arguments
  call[[1]] <- NULL
  call[[1]] <- NULL
  call <- Filter(function(argument_value) !is.language(argument_value) &&
                   (!is.name(argument_value)  ||
                      !(as.character(argument_value) %in% colnames(x))),
                 call)

  if (length(vars) > 0) {
    for (i in 1:length(vars)) {
      call$digits <- prPickDigits(colname = colnames(x)[vars[i]],
                                  colindex = i,
                                  total_cols = ncol(x),
                                  digits = digits)
      x[[names(vars)[i]]] <- do.call(txtRound,
                                     c(list(x = x[[vars[i]]]), call))
    }
  }

  return(x)
}


prPickDigits <- function(colname, colindex, total_cols, digits) {
  if (length(digits) == 1 && is.numeric(digits)) return(digits)

  if (is.null(names(digits))) {
    if (total_cols == length(digits)) {
      return(digits[colindex])
    }
    stop("Either provide digits as a single numerical or",
         " a named vector/list that we can pick elements from")
  }

  stopifnot(all(sapply(digits, is.numeric)))

  if (colname %in% names(digits)) {
    return(digits[[colname]])
  }

  if (".default" %in% names(digits)) {
    return(digits[[".default"]])
  }

  stop("The column '", colname, "' (pos. ", colindex, ") was not among provided digits: '",
       paste(names(digits), collapse = "', '"), "' and no '.default' was found.")
}