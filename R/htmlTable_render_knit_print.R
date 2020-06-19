#' @rdname htmlTable
#' @importFrom knitr knit_print
#' @importFrom knitr asis_output
#' @export
knit_print.htmlTable <- function(x, ...) {
  asis_output(x)
}
