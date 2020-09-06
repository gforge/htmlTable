#' Set or update theme for [htmlTable()]
#'
#' The theme guides many of the non-data objects visual appearance. The
#' theme can be over-ridden by settings for each table. Too get a more complete
#' understanding of the options, see [addHtmlTableStyle()].
#'
#' @section Theme options:
#'
#' The styles available are:
#'
#' * `standard`: The traditional standard style used in [htmlTable()] since the early days
#' * `Google docs`: A style that is optimized for copy-pasting into documents on Google drive. This
#'  is geared towards minimal padding and margins so that the table is as dense as possible.
#' * `blank`: Just as the name suggests the style is completly empty in terms of CSS. Positions
#'  for rowlabel and caption are set to `bottom` as these cannot be blank.
#'
#' You can also provide your own style. Each style should be a names vector, e.g. `c(width = "100px", color = "red")`
#' or just a real css string, `width: 100px; color: red;`.
#'
#' @param theme A `list` containing all the styles or a `string` that is matched to some of the preset style (See details
#'  below in the *Theme options* section). *Note*: the full name of the theme is not required as they are matched
#'  using [base::match.arg()].
#' @inheritParams addHtmlTableStyle
#'
#' @return An invisible `list` with the new theme
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' setHtmlTableTheme("Google", align = "r")
#' }
setHtmlTableTheme <- function(theme = NULL,
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
  if (!is.null(theme)) {
    if (is.character(theme)) {
      newTheme <- prGetThemeListObject(theme_name = theme)
    } else if (is.list(theme)) {
      if (any(names(theme) == "")) {
        stop("Theme contains unnamed elements")
      }

      prAssertStyleNames(names(theme), "You have invalid theme names.")

      newTheme <- theme
    } else {
      stop("The theme must either be a list or a valid predefined theme name")
    }
  } else {
    newTheme <- getHtmlTableTheme()
  }


  newTheme <- prValidateAndMergeStyles(
    org_style_list = newTheme,
    styles_from_arguments = prGetArgumentList(match.call(), c("", "theme")),
    overwrite = TRUE
  )

  prAssertStyles(newTheme)
  options(htmlTable.theme = newTheme)
  return(invisible(newTheme))
}

prGetArgumentList <- function(args, skip_elements) {
  if (!is.list(args)) {
    args <- as.list(args)
  }

  args <- args[Filter(function(x) !(x %in% skip_elements | x == ""), names(args))]
  Map(function(arg) {
    if (is.language(arg)) {
      return(eval(arg))
    }
    return(arg)
  }, args)
}

#' Retrieve the [htmlTable()] theme list
#'
#' A wrapper for a [`getOption("htmlTable.theme")()`][base::options] call that
#' returns the standard theme unless one is set.
#'
#' @return `list` with the styles to be applied to the table
#' @export
#'
#' @examples
#' getHtmlTableTheme()
getHtmlTableTheme <- function() {
  getOption("htmlTable.theme",
    default = prGetThemeListObject(theme_name = "standard")
  )
}

prGetThemeListObject <- function(theme_name = c("standard", "Google docs", "blank")) {
  theme_name <- match.arg(theme_name)

  common_non_css_elements <- list(
    align = "c",
    align.header = "c",

    # colors
    col.rgroup = "none",
    col.columns = "none",

    # More alternatives
    padding.rgroup = "&nbsp;&nbsp;",
    padding.tspanner = "",
    spacer.celltype = "single_empty",
    spacer.css.cgroup.bottom.border = "none",
    spacer.css = "",
    spacer.content = "&nbsp;"
  )

  if (theme_name == "standard") {
    # This list is the reference with all the available theme elements
    standard_theme <- list(
      css.rgroup = getOption("htmlTable.css.rgroup", default = "font-weight: 900;"),
      css.rgroup.sep = getOption("htmlTable.css.rgroup.sep", default = ""),

      css.tspanner = getOption("htmlTable.css.tspanner",
        default = "font-weight: 900; text-align: left;"
      ),
      css.tspanner.sep = getOption("htmlTable.css.tspanner.sep",
        default = "border-top: 1px solid #BEBEBE;"
      ),

      css.total = getOption("htmlTable.css.total",
        default = "border-top: 1px solid #BEBEBE; font-weight: 900;"
      ),

      css.cell = getOption("htmlTable.css.cell", default = ""),
      css.cgroup = getOption("htmlTable.css.cgroup", default = ""),
      css.header = getOption("htmlTable.css.header", default = "font-weight: 900"),
      css.header.border_bottom = getOption("htmlTable.css.header.border_bottom", default = "border-bottom: 1px solid grey"),

      css.class = getOption("htmlTable.css.class", default = "gmisc_table"),
      css.table = getOption("htmlTable.css.table", default = "margin-top: 1em; margin-bottom: 1em;"),
      # Positions
      pos.rowlabel = "bottom",
      pos.caption = "top"
    )

    return(prExtendlist(
      base = common_non_css_elements,
      extensions = standard_theme
    ))
  }

  if (theme_name == "Google docs") {
    doc_theme <- list(
      css.rgroup = getOption("htmlTable.css.rgroup", default = "font-weight: normal; margin: 0; padding: 0;"),
      css.rgroup.sep = getOption("htmlTable.css.rgroup.sep", default = ""),

      css.tspanner = getOption("htmlTable.css.tspanner",
        default = "font-weight: 900; text-align: left;"
      ),
      css.tspanner.sep = getOption("htmlTable.css.tspanner.sep",
        default = "border-top: 1px solid #BEBEBE;"
      ),

      css.total = getOption("htmlTable.css.total",
        default = "border-top: 1px solid #BEBEBE; font-weight: 900;"
      ),

      css.cell = getOption("htmlTable.css.cell", default = "margin: 0; padding: 0;"),
      css.cgroup = getOption("htmlTable.css.cgroup", default = "margin: 0; padding: 0; vertical-align: middle;"),
      css.header = getOption("htmlTable.css.header", default = "margin: 0; padding: 0; font-weight: 900; vertical-align: middle;"),
      css.header.border_bottom = getOption("htmlTable.css.header.border_bottom", default = "border-bottom: 1px solid grey"),

      css.class = getOption("htmlTable.css.class", default = "gmisc_table"),
      css.table = getOption("htmlTable.css.table", default = "margin-top: 1em; margin-bottom: 1em;"),

      spacer.celltype = "double_cell",
      spacer.css.cgroup.bottom.border = "1px solid white",
      spacer.content = "",
      spacer.css = "width: 2px;",

      # Positions
      pos.rowlabel = "bottom",
      pos.caption = "bottom"
    )

    return(prExtendlist(
      base = common_non_css_elements,
      extensions = doc_theme
    ))
  }

  if (theme_name == "blank") {
    blank_theme <- list(
      css.rgroup = "",
      css.rgroup.sep = "",

      css.tspanner = "",
      css.tspanner.sep = "",

      css.total = "",

      css.cell = "",
      css.cgroup = "",
      css.header = "",
      # Not blank as it is part of core table
      css.header.border_bottom = "border-bottom: 1px solid grey",

      css.class = "",
      css.table = "",

      # Positions
      pos.rowlabel = "bottom",
      pos.caption = "bottom"
    )
    return(prExtendlist(
      base = common_non_css_elements,
      extensions = blank_theme
    ))
  }
}

prExtendlist <- function(base, extensions) {
  for (n in names(extensions)) {
    base[[n]] <- extensions[[n]]
  }

  return(base)
}
