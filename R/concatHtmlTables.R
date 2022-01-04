#' Function for concatenating [htmlTable()]s
#'
#' @param tables A list of [htmlTable()]s to be concatenated
#' @param headers Either a string or a vector of strings that function as
#'  a header for each table. If none is provided it will use the names of
#'  the table list or a numeric number.
#' @return [htmlTable()] class object
#' @example inst/examples/concatHtmlTables_example.R
#' @export
concatHtmlTables <- function(tables, headers = NULL) {
  assert_list(tables)

  if (is.null(headers)) {
    if (!is.null(names(tables))) {
      headers = sprintf("<h1>%s</h1>", names(tables))
    } else {
      headers = sprintf("<h1>Table no. %d</h1>", 1:length(tables))
    }
  } else {
    headers = rep(headers, length.out = length(tables))
  }

  ret = paste(headers[1], tables[[1]])
  for (i in 2:length(tables)) {
    ret = paste0(
      ret,
      headers[i],
      tables[[i]]
    )
  }

  # Copy all the attributes from the first table
  attributes(ret) <- attributes(tables[[1]])
  class(ret) <- c('htmlTable', class(tables[[1]]))
  return(ret)
}
