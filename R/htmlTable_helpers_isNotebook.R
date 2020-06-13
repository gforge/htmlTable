#' Detects if the call is made from within an RStudio Rmd file or a file
#' with the html_notebook output set.
#' @importFrom rstudioapi isAvailable getActiveDocumentContext
#' @keywords internal
prIsNotebook <- function() {
  if (!isAvailable()) {
    return(FALSE)
  }

  ctxt <- getActiveDocumentContext()
  if (grepl("\\.Rmd$", ctxt$path)) {
    return(TRUE)
  }

  # Look for html_notebook within the header if the file hasn't been saved
  contents <- ctxt$contents
  header <- grep("^---$", contents)
  if (length(header) == 2) {
    return(any(grepl(
      "html_notebook$",
      contents[min(header):max(header)]
    )))
  }

  return(FALSE)
}
