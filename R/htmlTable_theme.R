#' Set or update theme for \code{\link{htmlTable}}
#'
#' The theme guides many of the non-data objects visual appearance. The
#' theme can be over-ridden by settings for each table.
#'
#' @section Theme options
#'
#' The options available are:
#'
#' * `css.rgroup`: CSS style for the rgroup, if different styles are wanted for each of the
#'  rgroups you can just specify a vector with the number of elements.
#' * `css.rgroup.sep`: The line between different rgroups. The line is set to the TR element
#'  of the lower rgroup, i.e. you have to set the border-top/padding-top etc to a line with
#'  the expected function. This is only used for rgroups that are printed. You can specify
#'  different separators if you give a vector of rgroup - 1 length (this is since the first
#'  rgroup doesn't have a separator).
#' * `css.tspanner`: The CSS style for the table spanner.
#' * `css.tspanner.sep`: The line between different spanners.
#' * `css.total`: The css of the total row if such is activated.
#' * `css.cell`: The css.cell element allows you to add any possible CSS style to your
#'  table cells. See section below for details.
#' * `css.cgroup`: The same as \code{css.class} but for cgroup formatting.
#' * `css.class`: The html CSS class for the table. This allows directing html
#'  formatting through \href{http://www.w3schools.com/Css/}{CSS}
#'  directly at all instances of that class. \emph{Note:} unfortunately the
#'  CSS is frequently ignored by word processors. This option
#'  is mostly inteded for web-presentations.
#' * `css.table`: You can specify the the style of the table-element using this parameter
#' * `pos.rowlabel`: Where the rowlabel should be positioned. This value can be \code{"top"},
#'  \code{"bottom"}, \code{"header"}, or a integer between \code{1} and \code{nrow(cgroup) + 1}. The options
#'  \code{"bottom"} and \code{"header"} are the same, where the row label is presented at the same level as
#'  the header.
#' * `pos.caption`: Set to \code{"bottom"} to position a caption below the table
#'  instead of the default of \code{"top"}.
#'
#' @param theme A `list` containing all the valid or a `string`
#' @param ... You can specify options
#'
#' @return
#' @export
#' @md
#'
#' @examples
setHtmlTableTheme <- function(theme,
                              align,
                              align.header,
                              align.cgroup,

                              # CSS stuff
                              css.rgroup,
                              css.rgroup.sep,

                              css.tspanner,
                              css.tspanner.sep,

                              css.total,
                              css.cell,
                              css.cgroup,

                              css.class,
                              css.table,

                              # Positions
                              pos.rowlabel,
                              pos.caption,

                              # Colors
                              col.rgroup,
                              col.columns,

                              # More alternatives
                              padding.rgroup,
                              padding.tspanner) {
  if (!missing(theme)) {
    if (is.character(theme)) {
      newTheme <- prGetThemeListObject(theme_name = theme)
    } else if (is.list(theme)) {
      if (any(names(theme) == "")) {
        stop('Theme contains unnamed elements')
      }

      prAssertStyleNames(names(theme), 'You have invalid theme names.')

      newTheme <- theme
    } else {
      stop('The theme must either be a list or a valid predefined theme name')
    }
  } else {
    newTheme <- getHtmlTableTheme()
  }

  args <- as.list(match.call())
  args[[1]] <- NULL
  args$theme <- NULL
  if (length(args) > 0) {
    for (n in names(args)) {
      newTheme[[n]] <- args[n]
    }
  }

  options(htmlTable.theme = newTheme)
}

prAssertStyleNames <- function(x, message) {
  if (any(x == "")) {
    stop(message, " Empty names not allowed.")
  }

  invalid_names <- prGetInvalidStyleNames(x)
  if (length(invalid_names) > 0) {
    stop(message, ' See name(s): ', paste(invalid_names, collapse = ", "))
  }
}

prGetInvalidStyleNames <- function(x) {
  valid_names <- names(prGetThemeListObject(theme_name = "standard"))
  checked_names <- args %in% valid_names
  return(args[!checked_names])
}


#' Retrieve the \code{\link{htmlTable}} theme list
#'
#' A wrapper for a \code{\link[base]{getOption("htmlTable.theme")}} call that
#' returns the standard theme unless one is set.
#'
#' @return \code{list} with the styles to be applied to the table
#' @export
#'
#' @examples
#' getHtmlTableTheme()
getHtmlTableTheme <- function() {
  getOption("htmlTable.theme",
            default = prGetThemeListObject(theme_name = "standard"))
}

prGetThemeListObject <- function(theme_name = c("standard", "Google docs")) {
  theme_name <- match.arg(theme_name)
  if (theme_name == "standard") {
    # This list is the reference with all the available theme elements
    return(list(
      css.rgroup = getOption("htmlTable.css.rgroup", default="font-weight: 900;"),
      css.rgroup.sep = getOption("htmlTable.css.rgroup.sep", default =""),

      css.tspanner = getOption("htmlTable.css.tspanner",
                               default = "font-weight: 900; text-align: left;"),
      css.tspanner.sep = getOption("htmlTable.css.tspanner.sep",
                                   default = "border-top: 1px solid #BEBEBE;"),

      css.total = getOption("htmlTable.css.total",
                            default = "border-top: 1px solid #BEBEBE; font-weight: 900;"),

      css.cell = getOption("htmlTable.css.cell", default = ""),
      css.cgroup = getOption("htmlTable.css.cgroup", default = ""),

      css.class = getOption("htmlTable.css.class", default = "gmisc_table"),
      css.table = getOption("htmlTable.css.table", default = "margin-top: 1em; margin-bottom: 1em;"),

      # Positions
      pos.rowlabel = "bottom",
      pos.caption = 'top'
    ))
  }

  if (theme_name == "Google docs") {
    return(list(
      css.rgroup = getOption("htmlTable.css.rgroup", default="font-weight: 900;"),
      css.rgroup.sep = getOption("htmlTable.css.rgroup.sep", default =""),

      css.tspanner = getOption("htmlTable.css.tspanner",
                               default = "font-weight: 900; text-align: left;"),
      css.tspanner.sep = getOption("htmlTable.css.tspanner.sep",
                                   default = "border-top: 1px solid #BEBEBE;"),

      css.total = getOption("htmlTable.css.total",
                            default = "border-top: 1px solid #BEBEBE; font-weight: 900;"),

      css.cell = getOption("htmlTable.css.cell", default = "margin: 0; padding: 0;"),
      css.cgroup = getOption("htmlTable.css.cgroup", default = ""),

      css.class = getOption("htmlTable.css.class", default = "gmisc_table"),
      css.table = getOption("htmlTable.css.table", default = "margin-top: 1em; margin-bottom: 1em;"),

      # Positions
      pos.rowlabel = "bottom",
      pos.caption = 'top'
    ))
  }
}
