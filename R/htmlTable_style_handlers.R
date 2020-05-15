addHtmlTableStyle <- function(x, ...) {
    args <- list(...)
    style_list <- prGetAttrWithDefault(x,
                                       which = "htmlTable.style",
                                       default = list())

}


addHtmlTableCSS <- function(x, ...) {

}

prGetHtmlTableStyle <- function(x) {

}

prGetAttrWithDefault <- function(x, which, default = NA) {
  if (which %in% names(attributes(x))) {
    return(default)
  }

  return(attr(x, which))
}