#' Add/set css and other style options
#'
#' This function is a preprocessing step before applying the [htmlTable()] function.
#' You use this to style your tables with HTML cascading style sheet features.
#'
#' The function stores the current theme (see [setHtmlTableTheme()]) + custom styles
#' to the provided object as an [base::attributes()]. It is stored under the element
#' `htmlTable.style` in the form of a list object.
#'
#' @section The `css.cell` argument:
#'
#' The `css.cell` parameter allows you to add any possible CSS style
#' to your table cells.  `css.cell` can be either a vector or a matrix.
#'
#' If  `css.cell` is a *vector*, it's assumed that the styles should be repeated
#' throughout the rows (that is, each element in css.cell specifies the style
#' for a whole column of 'x').
#'
#' In the case of  `css.cell` being a *matrix* of the same size of the `x` argument,
#' each element of `x` gets the style from the corresponding element in css.cell.  Additionally,
#' the number of rows of `css.cell` can be `nrow(x) + 1` so the first row of of `css.cell`
#' specifies the style for the header of `x`; also the number of columns of `css.cell`
#' can be `ncol(x) + 1` to include the specification of style for row names of `x`.
#'
#' Note that the `text-align` CSS field in the `css.cell` argument will be overriden
#' by the `align` argument.
#'
#' Excel has a specific css-style, `mso-number-format` that can be used for improving the
#' copy-paste functionality. E.g. the style could be written as: `css_matrix <-
#' matrix( data = "mso-number-format:\"\\@\"", nrow = nrow(df), ncol = ncol(df))`
#'
#' @param x The object that you later want to pass into [htmlTable()].
#' @param align A character strings specifying column alignments, defaulting to `'c'`
#'  to center. Valid chars for alignments are l = left, c = center and r = right. You can also specify
#'  `align='c|c'` and other LaTeX tabular formatting. If you want to set the alignment of the
#'  rownames this string needst to be `ncol(x) + 1`, otherwise it automatically
#'  pads the string with a left alignment for the rownames.
#' @param align.header A character strings specifying alignment for column header,
#'  defaulting to centered, i.e. `[paste][base::paste](rep('c',ncol(x)),collapse='')`.
#' @param align.cgroup The justification of the `cgroups`
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
#' @param css.header The header style, not including the cgroup style
#' @param css.header.border_bottom The header bottom-border style, e.g. `border-bottom: 1px solid grey`
#' @param css.cgroup The same as `css.class` but for cgroup formatting.
#' @param css.class The html CSS class for the table. This allows directing html
#'  formatting through [CSS](https://www.w3schools.com/Css/)
#'  directly at all instances of that class. *Note:* unfortunately the
#'  CSS is frequently ignored by word processors. This option
#'  is mostly inteded for web-presentations.
#' @param css.table You can specify the the style of the table-element using this parameter
#' @param pos.rowlabel Where the rowlabel should be positioned. This value can be `"top"`,
#'  `"bottom"`, `"header"`, or a integer between `1` and `nrow(cgroup) + 1`. The options
#'  `"bottom"` and `"header"` are the same, where the row label is presented at the same level as
#'  the header.
#' @param pos.caption Set to `"bottom"` to position a caption below the table
#'  instead of the default of `"top"`.
#' @param col.rgroup Alternating colors (zebra striping/banded rows) for each `rgroup`; one or two colors
#'  is recommended and will be recycled.
#' @param col.columns Alternating colors for each column.
#' @param padding.rgroup Generally two non-breakings spaces, i.e. `&nbsp;&nbsp;`, but some
#'  journals only have a bold face for the rgroup and leaves the subelements unindented.
#' @param padding.tspanner The table spanner is usually without padding but you may specify padding
#'  similar to `padding.rgroup` and it will be added to all elements, including the rgroup elements.
#'  This allows for a 3-level hierarchy if needed.
#' @param spacer.celltype When using cgroup the table headers are separated through a empty
#'  HTML cell that is by default filled with `&nbsp;` (no-breaking-space) that prevents the cell
#'  from collapsing. The purpose of this is to prevent the headers underline to bleed into one
#'  as the underline is for the entire cell. You can alter this behavior by changing this option,
#'  valid options are `single_empty`, `skip`, `double_cell`. The `single_empty` is the default,
#'  the `skip` lets the header bleed into one and skips entirely, `double_cell` is for having
#'  two cells so that a vertical border ends up centered (specified using the `align` option).
#'  The arguments are matched internally using [base::match.arg] so you can specify only a part
#'  of the name, e.g. `"sk"` will match `"skip"`.
#' @param spacer.css.cgroup.bottom.border Defaults to `none` and used for separating cgroup headers.
#'  Due to a browser bug this is sometimes ignored and you may therefore need to set this
#'  to `1px solid white` to enforce a white border.
#' @param spacer.css If you want the spacer cells to share settings you can set it here
#' @param spacer.content Defaults to `&nbsp;` as this guarantees that the cell is not collapsed
#'  and is highly compatible when copy-pasting to word processors.
#'
#' @return `x` with the style added as an attribute that the htmlTable then can use for formatting.
#' @export
#'
#' @examples
#' library(magrittr)
#' matrix(1:4, ncol = 2) %>%
#'   addHtmlTableStyle(align = "c", css.cell = "background-color: orange;") %>%
#'   htmlTable(caption = "A simple style example")
#' @rdname addStyles
#' @family htmlTableStyle
addHtmlTableStyle <- function(x,
                              align = NULL,
                              align.header = NULL,
                              align.cgroup = NULL,

                              # CSS stuff
                              css.rgroup = NULL,
                              css.rgroup.sep = NULL,

                              css.tspanner = NULL,
                              css.tspanner.sep = NULL,

                              css.total = NULL,
                              css.cell = NULL,
                              css.cgroup = NULL,
                              css.header = NULL,
                              css.header.border_bottom = NULL,

                              css.class = NULL,
                              css.table = NULL,

                              # Positions
                              pos.rowlabel = NULL,
                              pos.caption = NULL,

                              # Colors
                              col.rgroup = NULL,
                              col.columns = NULL,

                              # More alternatives
                              padding.rgroup = NULL,
                              padding.tspanner = NULL,
                              spacer.celltype = NULL,
                              spacer.css.cgroup.bottom.border = NULL,
                              spacer.css = NULL,
                              spacer.content = NULL) {
  style_list <- prGetAttrWithDefault(x,
    which = style_attribute_name,
    default = getHtmlTableTheme()
  )

  style_list <- prValidateAndMergeStyles(
    org_style_list = style_list,
    styles_from_arguments = prGetArgumentList(match.call(), skip_elements = c("x")),
    overwrite = TRUE
  )

  attr(x, style_attribute_name) <- style_list
  return(x)
}

#' @rdname addStyles
appendHtmlTableStyle <- function(x,
                                 align = NULL,
                                 align.header = NULL,
                                 align.cgroup = NULL,

                                 # CSS stuff
                                 css.rgroup = NULL,
                                 css.rgroup.sep = NULL,

                                 css.tspanner = NULL,
                                 css.tspanner.sep = NULL,

                                 css.total = NULL,
                                 css.cell = NULL,
                                 css.cgroup = NULL,
                                 css.header = NULL,
                                 css.header.border_bottom = NULL,

                                 css.class = NULL,
                                 css.table = NULL,

                                 # Positions
                                 pos.rowlabel = NULL,
                                 pos.caption = NULL,

                                 # Colors
                                 col.rgroup = NULL,
                                 col.columns = NULL,

                                 # More alternatives
                                 padding.rgroup = NULL,
                                 padding.tspanner = NULL,
                                 spacer.celltype = NULL,
                                 spacer.css.cgroup.bottom.border = NULL,
                                 spacer.css = NULL,
                                 spacer.content = NULL) {
  style_list <- prGetAttrWithDefault(x,
    which = style_attribute_name,
    default = getHtmlTableTheme()
  )

  style_list <- prValidateAndMergeStyles(
    org_style_list = style_list,
    styles_from_arguments = prGetArgumentList(match.call(), skip_elements = c("x")),
    overwrite = FALSE
  )

  attr(x, style_attribute_name) <- style_list
  return(x)
}

#' Get style options for object
#'
#' A wrap around the [base::attr()] that retrieves the style
#' attribute used by [htmlTable()] (`htmlTable.style`).
#'
#' @param x The object intended for [htmlTable()].
#' @return A `list` if the attribute exists, otherwise `NULL`
#' @export
#' @examples
#' library(magrittr)
#'
#' mx <- matrix(1:4, ncol = 2)
#' colnames(mx) <- LETTERS[1:2]
#' mx %>%
#'   addHtmlTableStyle(align = "l|r") %>%
#'   getHtmlTableStyle()
getHtmlTableStyle <- function(x) {
  attr(x, style_attribute_name, exact = TRUE)
}

#' Check if object has a style set to it
#'
#' If the attribute `htmlTable.style` is set it will check if
#' the `style_name` exists and return a `logical`.
#'
#' @param x The object intended for [htmlTable()].
#' @param style_name A string that contains the style name.
#' @return `logical` `TRUE` if the attribute and style is not `NULL`
#' @export
#' @family htmlTableStyle
#' @examples
#' library(magrittr)
#'
#' mx <- matrix(1:4, ncol = 2)
#' colnames(mx) <- LETTERS[1:2]
#' mx %>%
#'   addHtmlTableStyle(align = "l|r") %>%
#'   hasHtmlTableStyle("align")
hasHtmlTableStyle <- function(x, style_name) {
  style <- getHtmlTableStyle(x)
  if (is.null(style)) {
    return(FALSE)
  }

  if (is.null(style[[style_name]])) {
    return(FALSE)
  }

  return(TRUE)
}

#' Highlight matching rows
#'
#' Adds a row highlight rule to an object that will later be rendered with
#' [htmlTable()]. The `condition` is evaluated against the table columns and
#' a `.rowname` variable containing the row names.
#'
#' The `style` argument accepts:
#' * a preset name such as `"warning"`, `"info"`, `"success"`, or `"muted"`
#' * a bare color value, which becomes `background-color: ...`
#' * raw CSS fragments, including named vectors such as `c(color = "white")`
#'
#' @param x A matrix or data.frame that will later be passed to [htmlTable()].
#' @param condition A logical expression or logical vector used to select rows.
#' @param style The CSS style to apply to matched rows.
#' @return `x` with a row highlight rule stored in its `htmlTable.style` attribute.
#' @export
#' @aliases highlight_row
#' @family htmlTableStyle
highlightRow <- function(x, condition, style = "warning") {
  style_list <- prGetAttrWithDefault(x,
    which = style_attribute_name,
    default = getHtmlTableTheme()
  )

  rule <- list(
    condition = substitute(condition),
    env = parent.frame(),
    style = prNormalizeHighlightStyle(style)
  )

  if (is.null(style_list$row.highlight)) {
    style_list$row.highlight <- list(rule)
  } else {
    style_list$row.highlight <- c(style_list$row.highlight, list(rule))
  }

  attr(x, style_attribute_name) <- style_list
  return(x)
}

style_attribute_name <- "htmlTable.style"

prNormalizeHighlightStyle <- function(style) {
  preset_styles <- list(
    warning = "background-color: #fff3cd; color: #856404;",
    info = "background-color: #d1ecf1; color: #0c5460;",
    success = "background-color: #d4edda; color: #155724;",
    muted = "background-color: #f8f9fa; color: #6c757d;"
  )

  if (is.null(style) || length(style) == 0) {
    return("")
  }

  fragments <- c()
  style_names <- names(style)
  if (is.null(style_names)) {
    style_names <- rep("", length(style))
  }

  for (i in seq_along(style)) {
    style_i <- style[[i]]
    style_name <- style_names[[i]]

    if (is.null(style_i) || length(style_i) == 0 || is.na(style_i) || style_i == "") {
      next
    }

    if (is.character(style_i) &&
      length(style_i) == 1 &&
      grepl(";", style_i, fixed = TRUE)) {
      fragments <- c(
        fragments,
        prGetStyle(unlist(strsplit(style_i, "\\b;(\\b|\\W+)", perl = TRUE)))
      )
      next
    }

    if (is.character(style_i) && length(style_i) == 1 && !grepl(":", style_i, fixed = TRUE)) {
      preset_key <- tolower(style_i)
      if (style_i == "none") {
        next
      }
      if (style_name != "") {
        fragments <- c(fragments, paste0(style_name, ": ", style_i))
      } else if (preset_key %in% names(preset_styles)) {
        fragments <- c(fragments, preset_styles[[preset_key]])
      } else {
        fragments <- c(fragments, paste0("background-color: ", style_i))
      }
    } else if (is.character(style_i) && length(style_i) == 1 && style_name != "") {
      fragments <- c(fragments, paste0(style_name, ": ", style_i))
    } else {
      fragments <- c(fragments, prGetStyle(style_i))
    }
  }

  prMergeHighlightCss(fragments)
}

prMergeHighlightCss <- function(...) {
  fragments <- unlist(list(...), use.names = FALSE)
  if (length(fragments) == 0) {
    return("")
  }

  fragments <- unlist(strsplit(fragments, "\\b;(\\b|\\W+)", perl = TRUE), use.names = FALSE)
  fragments <- trimws(fragments)
  fragments <- fragments[nzchar(fragments) & !is.na(fragments)]
  if (length(fragments) == 0) {
    return("")
  }

  style_names <- sub("^([^:]+).+", "\\1", fragments)
  fragments <- fragments[!duplicated(style_names, fromLast = TRUE)]
  fragments <- sapply(fragments, prAddSemicolon2StrEnd, USE.NAMES = FALSE)
  paste(fragments, collapse = " ")
}

prEvalRowHighlights <- function(x, rnames, row_highlight_rules) {
  if (is.null(row_highlight_rules) || length(row_highlight_rules) == 0) {
    return(rep("", times = nrow(x)))
  }

  row_df <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  row_df$.rowname <- if (!prSkipRownames(rnames) && length(rnames) == nrow(x)) {
    rnames
  } else if (!is.null(rownames(x))) {
    rownames(x)
  } else {
    as.character(seq_len(nrow(x)))
  }

  row_styles <- rep("", times = nrow(x))
  for (rule in row_highlight_rules) {
    rule_env <- list2env(row_df, parent = rule$env)
    condition <- eval(rule$condition, envir = rule_env)

    if (!is.logical(condition)) {
      stop("highlightRow condition must evaluate to a logical vector")
    }

    if (length(condition) == 1) {
      condition <- rep(condition, times = nrow(x))
    }

    if (length(condition) != nrow(x)) {
      stop(
        "highlightRow condition must evaluate to length 1 or nrow(x) (",
        nrow(x), "), not ", length(condition)
      )
    }

    if (anyNA(condition)) {
      stop("highlightRow condition must not contain NA values")
    }

    row_styles[condition] <- vapply(
      row_styles[condition],
      function(existing) {
        prMergeHighlightCss(existing, rule$style)
      },
      character(1)
    )
  }

  row_styles
}

#' @importFrom stringr str_replace
prValidateAndMergeStyles <- function(org_style_list, styles_from_arguments, overwrite) {
  assert_list(org_style_list)
  assert_list(styles_from_arguments)

  styles_from_arguments <- Filter(Negate(is.null), styles_from_arguments)

  if (length(styles_from_arguments) == 0) {
    return(org_style_list)
  }

  default_args <- list("pos.caption" = c("top", "bottom", "below"),
                       "spacer.celltype" =  c("single_empty", "skip", "double_cell"))

  style_list <- org_style_list
  for (n in names(styles_from_arguments)) {
    # We only merge css components when we're not replacing everything
    if (startsWith(n, "css") && n != "css.class" && !overwrite) {
      # The second argument takes precedence as the final style when conflicts arise
      style_list[[n]] <- prGetStyle(style_list[[n]], styles_from_arguments[[n]])
    } else if (n %in% names(default_args)) {
      tryCatch({
        style_list[[n]] <- match.arg(arg = styles_from_arguments[[n]], choices = default_args[[n]])
      }, error = function(x) {
        x %>%
          extract2("message") %>%
          str_replace("^'arg'", sprintf("'%s'", argument_name = n)) %>%
          stop()
      })
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
