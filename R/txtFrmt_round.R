#' A convenient rounding function
#'
#' Regular round often looses trailing 0:s as these are truncated, this function
#' converts everything to strings with all 0:s intact so that tables have the
#' correct representation, e.g. `txtRound(1.01, digits = 1)` turns into `1.0`.
#'
#' @param x The value/vector/data.frame/matrix to be rounded
#' @param digits The number of digits to round each element to. For `matrix`
#'  or `data.frame` input you can provide a `vector`/`list`. An unnamed `vector`/`list`
#'  must equal the length of the columns to round. If you provide a named vector you
#'  can provide specify per column the number of digits, and then use `.default`
#'  for those columns that we don't need to have separate values for.
#' @param digits.nonzero The number of digits to keep if the result is close to
#'  zero. Sometimes we have an entire table with large numbers only to have a
#'  few but interesting observation that are really interesting
#' @param txt.NA The string to exchange `NA` with
#' @param dec The decimal marker. If the text is in non-English decimal
#'  and string formatted you need to change this to the appropriate decimal
#'  indicator. The option for this is `htmlTable.decimal_marker`.
#' @param scientific If the value should be in scientific format.
#' @param txtInt_args A list of arguments to pass to [txtInt()] if that is to be
#'  used for large values that may require a thousands separator. The option
#'  for this is `htmlTable.round_int`. If `TRUE` it will activate the `txtInt`
#'  functionality.
#' @param ... Passed to next method
#' @return `matrix/data.frame`
#'
#' @examples
#' # Basic usage
#' txtRound(1.023, digits = 1)
#' # > "1.0"
#'
#' txtRound(pi, digits = 2)
#' # > "3.14"
#'
#' txtRound(12344, digits = 1, txtInt_args = TRUE)
#' # > "12,344.0"
#'
#' @export
#' @rdname txtRound
#' @importFrom stringr str_split str_replace
#' @family text formatters
txtRound <- function(x, ...){
  UseMethod("txtRound")
}

#' @export
#' @rdname txtRound
txtRound.default = function(x,
                            digits = 0,
                            digits.nonzero = NA,
                            txt.NA = "",
                            dec = getOption("htmlTable.decimal_marker", default = "."),
                            scientific = NULL,
                            txtInt_args = getOption("htmlTable.round_int",
                                                    default = NULL),
                            ...){
  if (length(digits) != 1 & length(digits) != length(x))
    stop("You have ",
         length(digits),
         " digits specifications but a vector of length ",
         length(x),
         ": ",
         paste(x, collapse = ", "))

  if (isTRUE(txtInt_args)) {
    txtInt_args = getOption("htmlTable.round_int", default = list())
  }

  if (length(x) > 1) {
    return(mapply(txtRound.default,
                  x = x,
                  digits = digits,
                  digits.nonzero = digits.nonzero,
                  txt.NA = txt.NA,
                  dec = dec,
                  txtInt_args = rep(list(txtInt_args), times = length(x)),
                  ...))
  }

  if (!is.na(digits.nonzero)) {
    if (!is.numeric(digits.nonzero)
        || floor(digits.nonzero) != digits.nonzero
    ) {
      stop("The digits.nonzero should be an integer, you provided: ", digits.nonzero)
    }
    if (digits.nonzero < digits) {
      stop("The digits.nonzero must be larger than digits, as it is used for allowing more 0 when encountering small numbers.",
           " For instance, if we have 10.123 we rarely need more than 10.1 in form of digits while a for a small number",
           " such as 0.00123 we may want to report 0.001 (i.e. digits = 1, digits.nonzero = 3)")
    }
  }

  dec_str <- sprintf("^[^0-9\\%s-]*([\\-]{0,1}(([0-9]*|[0-9]+[ 0-9]+)[\\%s]|)[0-9]+(e[+]{0,1}[0-9]+|))(|[^0-9]+.*)$",
                     dec, dec)
  if (is.na(x))
    return(txt.NA)
  if (!is.numeric(x) &&
      !grepl(dec_str, x))
    return(x)

  if (is.character(x) && grepl(dec_str, x)) {
    if (dec != ".")
      x <- gsub(dec, ".", x)
    if (grepl("[0-9.]+e[+]{0,1}[0-9]+", x) && is.null(scientific)) {
      scientific <- TRUE
    }

    # Select the first occurring number
    # remove any spaces indicating thousands
    # and convert to numeric
    x <-
      sub(dec_str, "\\1", x) %>%
      gsub(" ", "", .) %>%
      as.numeric
  }

  if (!is.na(digits.nonzero)) {
    decimal_position <- floor(log10(abs(x)))
    if (decimal_position < -digits && decimal_position >= -digits.nonzero) {
      digits <- -decimal_position
    }
  }

  if (round(x, digits) == 0)
    x <- 0

  if (!is.null(scientific) && scientific) {
    x <- round(x, digits)
    return(format(x, scientific = TRUE))
  }

  ret <- sprintf(paste0("%.", digits, "f"), x)
  if (is.null(txtInt_args)) {
    return(ret)
  }

  stopifnot(is.list(txtInt_args))

  separator <- str_replace(ret, "^[0-9]*([.,])[0-9]*$", "\\1")
  # There is no decimal
  if (separator == ret) {
    int_section <- as.numeric(ret)
    txtInt_args$x <- int_section
    return(do.call(txtInt, txtInt_args))
  }

  ret_sections <- str_split(ret, paste0("[", separator, "]"))[[1]]
  if (length(ret_sections) == 1) {
    return(ret)
  }

  if (length(ret_sections) != 2) {
    return(ret)
  }

  int_section <- as.numeric(ret_sections[1])
  txtInt_args$x <- int_section
  int_section <- do.call(txtInt, txtInt_args)
  return(paste0(int_section, separator, ret_sections[2]))
}

#' @rdname txtRound
#' @export
txtRound.table <- function(x, ...){
  if (is.na(ncol(x))) {
    dim(x) <- c(1, nrow(x))
  }
  return(txtRound.matrix(x, ...))
}

#' @rdname txtRound
#' @param excl.cols Columns to exclude from the rounding procedure when provided a matrix.
#'  This can be either a number or regular expression. Skipped if `x` is a vector.
#' @param excl.rows Rows to exclude from the rounding procedure when provided a matrix.
#'  This can be either a number or regular expression.
#' @export
#' @examples
#'
#' # Using matrix
#' mx <- matrix(c(1, 1.11, 1.25,
#'                2.50, 2.55, 2.45,
#'                3.2313, 3, pi),
#'              ncol = 3, byrow=TRUE)
#' txtRound(mx, digits = 1)
#' #> [,1]  [,2]  [,3]
#' #> [1,] "1.0" "1.1" "1.2"
#' #> [2,] "2.5" "2.5" "2.5"
#' #> [3,] "3.2" "3.0" "3.1"
txtRound.matrix <- function(x, digits = 0, excl.cols = NULL, excl.rows = NULL, ...){
  if (length(dim(x)) > 2)
    stop("The function only accepts vectors/matrices/data.frames as primary argument")

  rows <- 1L:nrow(x)
  if (!is.null(excl.rows)) {
    if (is.character(excl.rows)) {
      excl.rows <- grep(excl.rows, rownames(x))
    }

    if (length(excl.rows) > 0)
      rows <- rows[-excl.rows]
  }

  cols <- 1L:(ifelse(is.na(ncol(x)), 1, ncol(x)))
  if (!is.null(excl.cols)) {
    if (is.character(excl.cols)) {
      excl.cols <- grep(excl.cols, colnames(x))
    }

    if (length(excl.cols) > 0)
      cols <- cols[-excl.cols]
  }

  if (length(cols) == 0)
    stop("No columns to round")

  if (length(rows) == 0)
    stop("No rows to round")

  ret_x <- x
  for (i in 1:length(cols)) {
    col <- cols[i]
    col_digits <- prPickDigits(colname = colnames(x)[col],
                               colindex = i,
                               total_cols = length(cols),
                               digits = digits)

    ret_x[rows, col] <- mapply(txtRound,
                               x = x[rows, col],
                               digits = col_digits,
                               ...,
                               USE.NAMES = FALSE)
  }

  return(ret_x)
}
