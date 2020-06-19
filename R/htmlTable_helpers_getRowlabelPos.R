
#' Gets the rowlabel position
#'
#' @inheritParams htmlTable
#' @return \code{integer} Returns the position within the header rows
#'  to print the \code{rowlabel} argument
#' @keywords internal
#' @family hidden helper functions for htmlTable
prGetRowlabelPos <- function(cgroup = NULL, pos.rowlabel, header = NULL) {
  no_cgroup_rows <-
    ifelse(!is.null(cgroup),
      nrow(cgroup),
      0
    )
  no_header_rows <-
    no_cgroup_rows +
    (!is.null(header)) * 1
  if (is.numeric(pos.rowlabel)) {
    if (pos.rowlabel < 1) {
      stop("You have specified a pos.rowlabel that is less than 1: ", pos.rowlabel)
    } else if (pos.rowlabel > no_header_rows) {
      stop(
        "You have specified a pos.rowlabel that more than the max limit, ",
        no_header_rows,
        ", you have provided: ", pos.rowlabel
      )
    }
  } else {
    pos.rowlabel <- tolower(pos.rowlabel)
    if (pos.rowlabel %in% c("top")) {
      pos.rowlabel <- 1
    } else if (pos.rowlabel %in% c("bottom", "header")) {
      pos.rowlabel <- no_header_rows
    } else {
      stop(
        "You have provided an invalid pos.rowlabel text,",
        " only 'top', 'bottom' or 'header' are allowed,",
        " can't interpret '", pos.rowlabel, "'"
      )
    }
  }

  return(pos.rowlabel)
}
