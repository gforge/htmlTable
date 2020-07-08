#' Collapse vector to string
#'
#' Merges all the values and outputs a string
#' formatted as '1st element', '2nd element', ...
#'
#' @param x The vector to collapse
#' @param collapse The string that separates each element
#' @param quotation_mark The type of quote to use
#' @return A string with `', '` separation
#' @importFrom stringr str_replace_all
#' @examples
#' vector2string(1:4)
#' vector2string(c("a", "b'b", "c"))
#' vector2string(c("a", "b'b", "c"), quotation_mark = '"')
#' @export
vector2string <- function(x,
                          quotation_mark = "'",
                          collapse = sprintf("%s, %s", quotation_mark, quotation_mark)) {
       paste0(
              quotation_mark,
              paste(sapply(x,
                     function(single_x) {
                            str_replace_all(
                                   single_x,
                                   quotation_mark,
                                   sprintf("\\\\%s", quotation_mark)
                            )
                     },
                     USE.NAMES = FALSE
              ),
              collapse = collapse
              ),
              quotation_mark
       )
}
