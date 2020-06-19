#' Gets the last table number
#'
#' The function relies on \code{options("table_counter")}
#' in order to keep track of the last number.
#'
#' @param roman Whether or not to use roman numbers instead
#'  of arabic. Can also be set through \code{options(table_caption_no_roman = TRUE)}
#'
#' @export
#' @examples
#' org_opts <- options(table_counter=1)
#' tblNoLast()
#' options(org_opts)
#' @family table functions
#' @importFrom utils as.roman
tblNoLast <- function(roman = getOption("table_counter_roman",
                                        FALSE)){
  last_no <- getOption("table_counter")
  if (is.logical(last_no) ||
      is.null(last_no)){
    stop("You cannot call the get last figure number",
         " when there has been no prior figure registerd.",
         " In other words, you need to call the fiCapNo()",
         " on a figure before you call this function.",
         " If you want the next number then call figCapNoNext()",
         " instead of this function.")
  }

  if (roman)
    last_no <- as.character(as.roman(last_no))

  return(last_no)
}

#' Gets the next table number
#'
#' The function relies on \code{options("table_counter")}
#' in order to keep track of the last number.
#'
#' @inheritParams tblNoLast
#' @export
#' @examples
#' org_opts <- options(table_counter=1)
#' tblNoNext()
#' options(org_opts)
#' @family table functions
#' @importFrom utils as.roman
tblNoNext <- function(roman = getOption("table_counter_roman",
                                        FALSE)){
  last_no <- getOption("table_counter")
  if (is.logical(last_no)){
    if (last_no == FALSE)
      stop("You cannot call the get last figure number",
           " when you have explicitly set the fig_cap_no",
           " option to false.")
    last_no <- 0

  }else if (is.null(last_no)){
    last_no <- 0
  }

  next_no <- last_no + 1

  if (roman)
    next_no <- as.character(as.roman(next_no))

  return(next_no)
}
