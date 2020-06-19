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
    return(prCheck4output2console(ctxt))
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

prCheck4output2console <- function(ctxt) {
  contents <- ctxt$contents
  header_boundary <- grep("^---$", contents)
  if (length(header_boundary) <= 1) {
    # Play it safe if the header is invalid
    return(TRUE)
  }

  header <- contents[header_boundary[1]:header_boundary[2]]
  return(!any(grepl("chunk_output_type: console", header)))
}