#' Add/set css and other style options
#'
#' This function is a preprocessing step before applying the \code{\link{htmlTable}} function.
#' You use this to style your tables with HTML cascading style sheet features.
#'
#' The function stores the current theme (see \code{\link{setHtmlTableTheme}}) + custom styles
#' to the provided object as an \code{\link[base]{attributes}}. It is stored under the element
#' \code{htmlTable.style} in the form of a list object.
#'
#' @section The \code{css.cell} argument:
#'
#' The \code{css.cell} parameter allows you to add any possible CSS style
#' to your table cells.  \code{css.cell} can be either a vector or a matrix.
#'
#' If  \code{css.cell} is a \emph{vector}, it's assumed that the styles should be repeated
#' throughout the rows (that is, each element in css.cell specifies the style
#' for a whole column of 'x').
#'
#' In the case of  \code{css.cell} being a \emph{matrix} of the same size of the \code{x} argument,
#' each element of \code{x} gets the style from the corresponding element in css.cell.  Additionally,
#' the number of rows of \code{css.cell} can be \code{nrow(x) + 1} so the first row of of \code{css.cell}
#' specifies the style for the header of \code{x}; also the number of columns of \code{css.cell}
#' can be \code{ncol(x) + 1} to include the specification of style for row names of \code{x}.
#'
#' Note that the \code{text-align} CSS field in the \code{css.cell} argument will be overriden
#' by the \code{align} argument.
#'
#' Excel has a specific css-style, \code{mso-number-format} that can be used for improving the
#' copy-paste functionality. E.g. the style could be written as: \code{css_matrix <-
#' matrix( data = "mso-number-format:\"\\@\"", nrow = nrow(df), ncol = ncol(df))}
#'
#' @param x The object that you later want to pass into \code{\link{htmlTable}}.
#' @param align A character strings specifying column alignments, defaulting to \code{'c'}
#'  to center. Valid chars for alignments are l = left, c = center and r = right. You can also specify
#'  \code{align='c|c'} and other LaTeX tabular formatting. If you want to set the alignment of the
#'  rownames this string needst to be \code{ncol(x) + 1}, otherwise it automatically
#'  pads the string with a left alignment for the rownames.
#' @param align.header A character strings specifying alignment for column header,
#'  defaulting to centered, i.e. \code{\link[base]{paste}(rep('c',ncol(x)),collapse='')}.
#' @param align.cgroup The justification of the \code{cgroups}
#' @param css.rgroup CSS style for the rgroup, if different styles are wanted for each of the
#'  rgroups you can just specify a vector with the number of elements.
#' @param css.rgroup.sep The line between different rgroups. The line is set to the TR element
#'  of the lower rgroup, i.e. you have to set the border-top/padding-top etc to a line with
#'  the expected function. This is only used for rgroups that are printed. You can specify
#'  different separators if you give a vector of rgroup - 1 length (this is since the first
#'  rgroup doesn't have a separator).
#' @param css.tspanner The CSS style for the table spanner.
#' @param css.tspanner.sep The line between different spanners.
#' @param css.total The css of the total row if such is activated.
#' @param css.cell The css.cell element allows you to add any possible CSS style to your
#'  table cells. See section below for details.
#' @param css.cgroup The same as \code{css.class} but for cgroup formatting.
#' @param css.class The html CSS class for the table. This allows directing html
#'  formatting through \href{http://www.w3schools.com/Css/}{CSS}
#'  directly at all instances of that class. \emph{Note:} unfortunately the
#'  CSS is frequently ignored by word processors. This option
#'  is mostly inteded for web-presentations.
#' @param css.table You can specify the the style of the table-element using this parameter
#' @param pos.rowlabel Where the rowlabel should be positioned. This value can be \code{"top"},
#'  \code{"bottom"}, \code{"header"}, or a integer between \code{1} and \code{nrow(cgroup) + 1}. The options
#'  \code{"bottom"} and \code{"header"} are the same, where the row label is presented at the same level as
#'  the header.
#' @param pos.caption Set to \code{"bottom"} to position a caption below the table
#'  instead of the default of \code{"top"}.
#' @param col.rgroup Alternating colors (zebra striping/banded rows) for each \code{rgroup}; one or two colors
#'  is recommended and will be recycled.
#' @param col.columns Alternating colors for each column.
#' @param padding.rgroup Generally two non-breakings spaces, i.e. \code{&nbsp;&nbsp;}, but some
#'  journals only have a bold face for the rgroup and leaves the subelements unindented.
#' @param padding.tspanner The table spanner is usually without padding but you may specify padding
#'  similar to \code{padding.rgroup} and it will be added to all elements, including the rgroup elements.
#'  This allows for a 3-level hierarchy if needed.
#'
#' @return \code{x} with the style added as an attribute that the htmlTable then can use for formatting.
#' @export
#'
#' @examples
#' library(magrittr)
#' matrix(1:4, ncol = 2) %>%
#'   addHtmlTableStyle(align = "c", css.cell = "background-color: orange;") %>%
#'   htmlTable(caption = "A simple style example")
#'
#' @rdname addStyles
#' @family htmlTableStyle
addHtmlTableStyle <- function(x,
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
  style_list <- prGetAttrWithDefault(x,
                                     which = style_attribute_name,
                                     default = getHtmlTableTheme())

  style_list <- prValidateAndMergeStyles(org_style_list = style_list,
                                         styles_from_arguments = prGetArgumentList(match.call(), c("x")),
                                         overwrite = TRUE)

  attr(x, style_attribute_name) <- style_list
  return(x)
}

#' @rdname addStyles
appendHtmlTableStyle <- function(x,
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
  style_list <- prGetAttrWithDefault(x,
                                     which = style_attribute_name,
                                     default = getHtmlTableTheme())

  style_list <- prValidateAndMergeStyles(org_style_list = style_list,
                                         styles_from_arguments = prGetArgumentList(match.call(), c("x")),
                                         overwrite = FALSE)

  attr(x, style_attribute_name) <- style_list
  return(x)
}

style_attribute_name <- "htmlTable.style"

prValidateAndMergeStyles <- function(org_style_list, styles_from_arguments, overwrite) {
  assert_list(org_style_list)
  assert_list(styles_from_arguments)

  if (length(styles_from_arguments) == 0) return(org_style_list)

  style_list <- org_style_list
  for (n in names(styles_from_arguments)) {
    # We only merge css components when we're not replacing everything
    if (startsWith(n, "css") && n != "css.class" && !overwrite) {
      # The second argument takes precedence as the final style when conflicts arise
      style_list[[n]] <- prGetStyle(style_list[[n]], styles_from_arguments[[n]])
    } else if (n == "pos.caption") {
      valid_caption_positions <- c("top", "bottom", "below")
      if (styles_from_arguments[[n]] %in% valid_caption_positions) {
        style_list[[n]] <- styles_from_arguments[[n]]
      } else {
        stop('The argument ', n, ' must be one of: ', paste(valid_caption_positions, collapse = ", "))
      }
    } else {
      style_list[[n]] <- styles_from_arguments[[n]]
    }
  }

  return(style_list)
}

prGetAttrWithDefault <- function(x, which, default = NA) {
  if (which %in% names(attributes(x))) {
    return(attr(x, which))
  }

  return(default)
}