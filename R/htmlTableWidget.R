#' htmlTable with pagination widget
#'
#' This widget renders a table with pagination into an htmlwidget
#'
#' @param x A data frame to be rendered
#' @param number_of_entries a numeric vector with the number of entries per page to show.
#'        If there is more than one number given, the user will be able to show the number
#'        of rows per page in the table.
#' @param ... Additional parameters passed to htmlTable
#' @inheritParams htmlwidgets::createWidget
#' @import htmlwidgets
#' @return an htmlwidget showing the paginated table
#' @export
htmlTableWidget <- function(x, number_of_entries = c(10, 25, 100),
                            width = NULL, height = NULL, elementId = NULL,
                            ...) {
  rendered_table <- htmlTable(x, ...)

  # forward options and variables using the input list:
  input <- list(
    thetable = rendered_table,
    options = list(number_of_entries = number_of_entries)
  )

  # create widget
  htmlwidgets::createWidget(
    name = "htmlTableWidget",
    x = input,
    width = width,
    height = height,
    package = "htmlTable",
    elementId = elementId
  )
}

#' Shiny bindings for htmlTableWidget
#'
#' Output and render functions for using htmlTableWidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a htmlTableWidget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name htmlTableWidget-shiny
#'
#' @examples
#' \dontrun{
#' # In the UI:
#' htmlTableWidgetOutput("mywidget")
#' # In the server:
#' renderHtmlTableWidget({
#'   htmlTableWidget(iris)
#' })
#' }
#' @export
htmlTableWidgetOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "htmlTableWidget", width, height, package = "htmlTable")
}

#' @rdname htmlTableWidget-shiny
#' @export
renderHtmlTableWidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, htmlTableWidgetOutput, env, quoted = TRUE)
}
