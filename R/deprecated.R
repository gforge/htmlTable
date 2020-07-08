# Deprecated function names

#' See [txtMergeLines()]
#'
#' @param ... passed onto [txtMergeLines()]
#' @examples
#' \dontrun{
#' # Deprecated function
#' splitLines4Table("hello", "world")
#' }
#' @keywords internal
#' @export
splitLines4Table <- function(...){
  warning("splitLines4Table is deprecated, use txtMergeLines() instead")
  txtMergeLines(...)
}

#' Deprecated use [txtInt()] instead.
#'
#' @param ... Passed to [txtInt()]
#'
#' @examples
#' \dontrun{
#' # Deprecated function
#' outputInt(123456)
#' }
#'
#' @keywords internal
#' @export
outputInt <- function(...){
  warning("outputInt is deprecated, use txtInt() instead.")
  txtInt(...)
}


#' Deprecated use [txtPval()] instead
#'
#' @param ... Currently only used for generating warnings of deprecated call
#' @examples
#' \dontrun{
#' # Deprecated function
#' pvalueFormatter(c(0.10234,0.010234, 0.0010234, 0.000010234))
#' }
#' @export
#' @keywords internal
pvalueFormatter <- function(...){
  warning("pvalueFormatter is deprecated, use txtPval() instead.")
  txtPval(...)
}
