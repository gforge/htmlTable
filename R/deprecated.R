# Deprecated function names

#' See \code{\link{txtMergeLines}}
#' 
#' @param ... passed onto \code{\link{txtMergeLines}}
#' @examples
#' splitLines4Table("hello", "world")
#' @keywords internal
#' @export
splitLines4Table <- function(...){
  warning("splitLines4Table is deprecated, use txtMergeLines() instead")
  txtMergeLines(...)
}

#' Deprecated use \code{\link{txtInt}} instead.
#'
#' @param ... Passed to \code{\link{txtInt}}
#'
#' @examples
#' outputInt(123456)
#'
#' @keywords internal
#' @export
outputInt <- function(...){
  warning("outputInt is deprecated, use txtInt() instead.")
  txtInt(...)
}


#' Deprecated use \code{\link{txtPval}} instead
#'
#' @param ... Currently only used for generating warnings of deprecated call
#' @examples 
#' pvalueFormatter(c(0.10234,0.010234, 0.0010234, 0.000010234))
#' @export
#' @keywords internal
pvalueFormatter <- function(...){
  warning("pvalueFormatter is deprecated, use txtPval() instead.")
  txtPval(...)
}