#' A merges lines while preserving the line break for HTML/LaTeX
#'
#' This function helps you to do a table header with multiple lines
#' in both HTML and in LaTeX. In HTML this isn't that tricky, you just use
#' the `<br />` command but in LaTeX I often find
#' myself writing `vbox`/`hbox` stuff and therefore
#' I've created this simple helper function
#'
#' @param ... The lines that you want to be joined
#' @param html If HTML compatible output should be used. If `FALSE`
#'  it outputs LaTeX formatting. Note if you set this to 5
#'  then the HTML5 version of *br* will be used: `<br>`
#'  otherwise it uses the `<br />` that is compatible
#'  with the XHTML-formatting.
#' @return string
#'
#' @examples
#' txtMergeLines("hello", "world")
#' txtMergeLines("hello", "world", html=FALSE)
#' txtMergeLines("hello", "world", list("A list", "is OK"))
#'
#' @family text formatters
#' @export
txtMergeLines <- function(..., html = 5){
  strings <- c()
  for (i in list(...)) {
    if (is.list(i)) {
      for (c in i)
        strings <- append(strings, i)
    }else{
      strings <- append(strings, i)
    }

  }
  if (length(strings) == 0) {
    return("")
  }

  if (length(strings) == 1) {
    strings <- gsub("\n", ifelse(html == 5, "<br>\n", "<br />\n"), strings)
    return(strings)
  }

  ret <- ifelse(html != FALSE, "", "\\vbox{")
  first <- TRUE
  for (line in strings) {
    line <- as.character(line)
    if (first)
      ret <- paste0(ret, ifelse(html != FALSE, line, sprintf("\\hbox{\\strut %s}", line)))
    else
      ret <- paste0(ret, ifelse(html != FALSE,
                                paste(ifelse(html == 5, "<br>\n", "<br />\n"),
                                      line),
                                sprintf("\\hbox{\\strut %s}", line)))
    first <- FALSE
  }
  ret <- ifelse(html, ret, paste0(ret, "}"))

  return(ret)
}

#' SI or English formatting of an integer
#'
#' English uses ',' between every 3 numbers while the
#' SI format recommends a ' ' if x > 10^4. The scientific
#' form 10e+? is furthermore avoided.
#'
#' @param x The integer variable
#' @param language The ISO-639-1 two-letter code for the language of
#'  interest. Currently only English is distinguished from the ISO
#'  format using a ',' as the separator.
#' @param html If the format is used in HTML context
#'  then the space should be a non-breaking space, `&nbsp;`
#' @param ... Passed to [base::format()]
#' @return `string`
#'
#' @examples
#' txtInt(123)
#'
#' # Supplying a matrix
#' txtInt(matrix(c(1234, 12345, 123456, 1234567), ncol = 2))
#'
#' # Missing are returned as empty strings, i.e. ""
#' txtInt(c(NA, 1e7))
#'
#' @family text formatters
#' @export
txtInt <- function(x,
                   language = getOption("htmlTable.language", default = "en"),
                   html = getOption("htmlTable.html", default = TRUE),
                   ...){
  if (length(x) > 1) {
    ret <- sapply(x, txtInt, language = language, html = TRUE, ...)
    if (is.matrix(x)) {
      ret <- matrix(ret, nrow = nrow(x))
      rownames(ret) <- rownames(x)
      colnames(ret) <- colnames(x)
    }
    return(ret)
  }
  if (is.na(x)) return('')

  if (abs(x - round(x)) > .Machine$double.eps^0.5 &&
        !"nsmall" %in% names(list(...)))
    warning("The function can only be served integers, '", x, "' is not an integer.",
            " There will be issues with decimals being lost if you don't add the nsmall parameter.")

  if (language == "en")
    return(format(x, big.mark = ",", scientific = FALSE, ...))

  if (x >= 10^4)
    return(format(x,
                  big.mark = ifelse(html, "&nbsp;", " "),
                  scientific = FALSE, ...))

  return(format(x, scientific = FALSE, ...))
}


#' Formats the p-values
#'
#' Gets formatted p-values. For instance
#' you often want `0.1234` to be `0.12` while also
#' having two values up until a limit,
#' i.e. `0.01234` should be `0.012` while
#' `0.001234` should be `0.001`. Furthermore you
#' want to have `< 0.001` as it becomes ridiculous
#' to report anything below that value.
#'
#' @param pvalues The p-values
#' @param lim.2dec The limit for showing two decimals. E.g.
#'  the p-value may be `0.056` and we may want to keep the two decimals in order
#'  to emphasize the proximity to the all-mighty `0.05` p-value and set this to
#'  \eqn{10^-2}. This allows that a value of `0.0056` is rounded to `0.006` and this
#'  makes intuitive sense as the `0.0056` level as this is well below
#'  the `0.05` value and thus not as interesting to know the exact proximity to
#'  `0.05`. *Disclaimer:* The `0.05`-limit is really silly and debated, unfortunately
#'  it remains a standard and this package tries to adapt to the current standards in order
#'  to limit publication associated issues.
#' @param lim.sig The significance limit for the less than sign, i.e. the '`<`'
#' @param html If the less than sign should be `<` or `&lt;` as needed for HTML output.
#' @param ... Currently only used for generating warnings of deprecated call parameters.
#' @return vector
#'
#' @examples
#' txtPval(c(0.10234,0.010234, 0.0010234, 0.000010234))
#' @family text formatters
#' @rdname txtPval
#' @export
txtPval <- function(pvalues,
                    lim.2dec = 10^-2,
                    lim.sig = 10^-4,
                    html=TRUE, ...){

  if (is.logical(html))
    html <- ifelse(html, "&lt; ", "< ")
  sapply(pvalues, function(x, lim.2dec, lim.sig, lt_sign) {
    if (is.na(as.numeric(x))) {
      warning("The value: '", x, "' is non-numeric and txtPval",
              " can't therefore handle it")
      return(x)
    }

    if (x < lim.sig)
      return(sprintf("%s%s", lt_sign, format(lim.sig, scientific = FALSE)))

    if (x > lim.2dec)
      return(format(x,
                    digits = 2,
                    nsmall = -floor(log10(x)) + 1))

    return(format(x, digits = 1, scientific = FALSE))
  }, lim.sig = lim.sig,
  lim.2dec = lim.2dec,
  lt_sign = html)
}
