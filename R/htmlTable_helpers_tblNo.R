#' Gets the table counter string
#'
#' Returns the string used for htmlTable to number the different tables.
#' Uses options `table_counter`, `table_counter_str`,
#' and `table_counter_roman` to produce the final string. You
#' can set each option by simply calling `options()`.
#'
#' @param caption The caption if any
#' @return `string` Returns a string formatted according to
#'  the table_counter_str and table_counter_roman. The number is
#'  decided by the table_counter variable
#' @keywords internal
#' @family hidden helper functions for htmlTable
#' @importFrom utils as.roman
prTblNo <- function(caption = NULL) {
  tc <- getOption("table_counter", FALSE)
  if (tc == FALSE) {
    if (is.null(caption)) {
      return("")
    } else {
      return(caption)
    }
  }

  table_template <- getOption("table_counter_str", "Table %s: ")
  out <- sprintf(
    table_template,
    ifelse(getOption("table_counter_roman", FALSE),
      as.character(as.roman(tc)),
      as.character(tc)
    )
  )
  if (!is.null(caption)) {
    out <- paste(out, caption)
  }

  return(out)
}
