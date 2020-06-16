#' Output an HTML table
#'
#' This is a function for outputting a more advanced
#' tables using HTML. The core philosophy is to bring column and row groups
#' into the table and allow for a dense representation of
#' complex tables. The HTML-output is designed for
#' maximum compatibility with copy-paste functionality into
#' word-processors. For adding styles, see \code{\link{addHtmlTableStyle}}
#' and themes \code{\link{setHtmlTableTheme}}. \emph{Note:} If you are using
#' \pkg{tidyverse} and \pkg{dplyr} you may want to check out
#' \code{\link{tidyHtmlTable}} that automates many of the arguments
#' that \code{htmlTable} requires.
#'
#' @section Multiple rows of column spanners \code{cgroup}:
#'
#' If you want to have a column spanner in multiple levels you can
#' set the \code{cgroup} and \code{n.cgroup} arguments to a \code{matrix} or
#'  \code{list}.
#'
#' If the different levels have different number of elements and you have
#' provided a **matrix** you need to set the ones that lack elements to NA. For instance
#' \code{cgroup = rbind(c("first", "second", NA), c("a", "b", "c"))}.
#' And the corresponding \code{n.cgroup} would be \code{n.cgroup = rbind(c(1, 2, NA), c(2, 1, 2))}.
#' for a table consisting of 5 columns. The "first" spans the first two columns,
#' the "second" spans the last three columns, "a" spans the first two, "b"
#' the middle column, and "c" the last two columns.
#'
#' It is recommended to use `list` as you will not have to bother with the `NA`.
#'
#' If you want leave a \code{cgroup} empty then simply provide `""` as the \code{cgroup}.
#'
#' @section The \code{rgroup} argument:
#'
#'  The \code{rgroup} allows you to smoothly group rows. Each row within a group
#'  receives an indention of two blank spaces and are grouped with their
#'  corresponding \code{rgroup} element. The \code{sum(n.rgroup)} should always
#'  be equal or less than the matrix rows. If less then it will pad the
#'  remaining rows with either an empty \code{rgroup}, i.e. an "" or if the
#'  \code{rgroup} is one longer than the \code{n.rgroup} the last \code{n.rgroup} element will
#'  be calculated through \code{nrow(x) - sum(n.rgroup)} in order to make
#'  the table generating smoother.
#'
#' @section The add attribute to \code{rgroup}:
#'
#' You can now have an additional element at the \code{rgroup} level by specifying the
#' \code{attr(rgroup, 'add')}. The value can either be a \code{vector}, a \code{list},
#' or a \code{matrix}. See \code{vignette("general", package = "htmlTable")} for examples.
#' \itemize{
#'  \item{A \code{vector} of either equal number of \code{rgroup}s to the number
#'   of \code{rgroup}s that aren't empty, i.e. \code{rgroup[rgroup != ""]}. Or a named vector where
#'   the name must correspond to either an \code{rgroup} or to an \code{rgroup} number.}
#'  \item{A \code{list} that has exactly the same requirements as the vector.
#'   In addition to the previous we can also have a list with column numbers within
#'   as names within the list.}
#'  \item{A \code{matrix} with the dimension \code{nrow(x) x ncol(x)} or
#'   \code{nrow(x) x 1} where the latter is equivalent to a named vector.
#'   If you have \code{rownames} these will resolve similarly to the names to the
#'   \code{list}/\code{vector} arguments. The same thing applies to \code{colnames}.
#'  }
#' }
#'
#' @section Important \pkg{knitr}-note:
#'
#' This function will only work with \pkg{knitr} outputting \emph{HTML}, i.e.
#' markdown mode. As the function returns raw HTML-code
#' the compatibility with non-HTML formatting is limited,
#' even with \href{http://johnmacfarlane.net/pandoc/}{pandoc}.
#'
#' Thanks to the the \code{\link[knitr]{knit_print}} and the
#' \code{\link[knitr]{asis_output}}
#' the \code{results='asis'} is \emph{no longer needed} except within for-loops.
#' If you have a knitr-chunk with a for loop and use \code{print()} to produce
#' raw HTML you must set the chunk option \code{results='asis'}. \emph{Note}:
#' the print-function relies on the \code{\link[base]{interactive}()} function
#' for determining if the output should be sent to a browser or to the terminal.
#' In vignettes and other directly knitted documents you may need to either set
#' \code{useViewer = FALSE} alternatively set \code{options(htmlTable.cat = TRUE)}.
#'
#' @section RStudio's notebook:
#'
#' RStudio has an interactive notebook that allows output directly into the document.
#' In order for the output to be properly formatted it needs to have the \code{class}
#' of \code{html}. The \code{htmlTable} tries to identify if the environment is a
#' notebook document (uses the \pkg{rstudioapi} and identifies if its a file with and \code{Rmd}
#' file ending or if there is an element with \code{html_notebook}). If you don't want this
#' behavior you can remove it using the \code{options(htmlTable.skip_notebook = TRUE)}.
#'
#' @section Table counter:
#'
#' If you set the option table_counter you will get a Table 1,2,3
#' etc before each table, just set \code{options(table_counter=TRUE)}. If
#' you set it to a number then that number will correspond to the start of
#' the table_counter. The \code{table_counter} option will also contain the number
#' of the last table, this can be useful when referencing it in text. By
#' setting the option \code{options(table_counter_str = "<b>Table \%s:</b> ")}
#' you can manipulate the counter table text that is added prior to the
#' actual caption. Note, you should use the \code{\link{sprintf}} \code{\%s}
#' instead of \code{\%d} as the software converts all numbers to characters
#' for compatibility reasons. If you set \code{options(table_counter_roman = TRUE)}
#' then the table counter will use Roman numerals instead of Arabic.
#'
#'@section Empty data frames:
#' An empty data frame will result in a warning and output an empty table, provided that
#' \code{rgroup} and \code{n.rgroup} are not specified. All other row layout options will be ignored.
#'
#' @section Other:
#'
#' \emph{Copy-pasting:} As you copy-paste results into Word you need to keep
#' the original formatting. Either right click and choose that paste option or click
#' on the icon appearing after a paste. Currently the following compatibilities
#' have been tested with MS Word 2016:
#'
#' \itemize{
#'  \item{\bold{Internet Explorer} (v. 11.20.10586.0) Works perfectly when copy-pasting into Word}
#'  \item{\bold{RStudio} (v. 0.99.448) Works perfectly when copy-pasting into Word.
#'        \emph{Note:} can have issues with multi-line \code{cgroup}s -
#'        see \href{http://code.google.com/p/chromium/issues/detail?id=305130}{bug}}
#'  \item{\bold{Chrome} (v. 47.0.2526.106) Works perfectly when copy-pasting into Word.
#'        \emph{Note:} can have issues with multi-line \code{cgroup}s -
#'        see \href{http://code.google.com/p/chromium/issues/detail?id=305130}{bug}}
#'  \item{\bold{Firefox} (v. 43.0.3) Works poorly - looses font-styling, lines and general feel}
#'  \item{\bold{Edge} (v. 25.10586.0.0) Works poorly - looses lines and general feel}
#' }
#'
#' \emph{Direct word processor opening:} Opening directly in Libre Office or Word is no longer
#' recommended. You get much prettier results using the cut-and-paste option.
#'
#' Note that when using complex \code{cgroup} alignments with multiple levels
#' not every browser is able to handle this. For instance the RStudio
#' webkit browser seems to have issues with this and a
#' \href{http://code.google.com/p/chromium/issues/detail?id=305130}{bug has been filed}.
#'
#' As the table uses HTML for rendering you need to be aware of that headers,
#' row names, and cell values should try respect this for optimal display. Browsers
#' try to compensate and frequently the tables still turn out fine but it is
#' not advised. Most importantly you should try to use
#' \code{&lt;} instead of \code{<} and
#' \code{&gt;} instead of \code{>}. You can find a complete list
#' of HTML characters \href{http://ascii.cl/htmlcodes.htm}{here}.
#'
#' Lastly, I want to mention that function was inspired by the \pkg{Hmisc}
#' \code{\link[Hmisc]{latex}()} that can be an excellent alternative if you wish
#' to switch to PDF-output. For the sibling function \code{\link{tidyHtmlTable}}
#' you can directly switch between the functions.
#'
#' @param x The matrix/data.frame with the data. For the \code{print} and \code{knit_print}
#'  it takes a string of the class \code{htmlTable} as \code{x} argument.
#' @param header A vector of character strings specifying column
#'  header, defaulting to \code{\link[base]{colnames}(x)}
#' @param rnames Default row names are generated from \code{\link[base]{rownames}(x)}. If you
#'  provide \code{FALSE} then it will skip the row names. \emph{Note:} For \code{data.frames}
#'  if you do \code{\link[base]{rownames}(my_dataframe) <- NULL} it still has
#'  row names. Thus you need to use \code{FALSE} if you want to
#'  supress row names for \code{data.frames}.
#' @param rowlabel If the table has row names or \code{rnames},
#'  \code{rowlabel} is a character string containing the
#'  column heading for the \code{rnames}.
#' @param caption Adds a table caption.
#' @param tfoot Adds a table footer (uses the \code{<tfoot>} HTML element). The
#'  output is run through \code{\link{txtMergeLines}} simplifying the generation
#'  of multiple lines.
#' @param label A text string representing a symbolic label for the
#'  table for referencing as an anchor. All you need to do is to reference the
#'  table, for instance \code{<a href="#anchor_name">see table 2</a>}. This is
#'  known as the element's id attribute, i.e. table id, in HTML linguo, and should
#'  be unique id for an HTML element in contrast to the \code{css.class} element attribute.
#' @param rgroup A vector of character strings containing headings for row groups.
#'  \code{n.rgroup} must be present when \code{rgroup} is given. See
#'   detailed description in section below.
#' @param n.rgroup An integer vector giving the number of rows in each grouping. If \code{rgroup}
#'  is not specified, \code{n.rgroup} is just used to divide off blocks of rows by horizontal
#'  lines. If \code{rgroup} is given but \code{n.rgroup} is omitted, \code{n.rgroup} will
#'  default so that each row group contains the same number of rows. If you want additional
#'  rgroup column elements to the cells you can sett the "add" attribute to \code{rgroup} through
#'  \code{attr(rgroup, "add")}, see below explaining section.
#' @param cgroup A vector, matrix or list of character strings defining major column header. The default
#'  is to have none. These elements are also known as \emph{column spanners}. If you want a column \emph{not}
#'  to have a spanner then put that column as "". If you pass cgroup and \code{n.crgroup} as
#'  matrices you can have column spanners for several rows. See cgroup section below for details.
#' @param n.cgroup An integer vector, matrix or list containing the number of columns for which each element in
#'  cgroup is a heading. For example, specify \code{cgroup=c("Major_1","Major_2")},
#'  \code{n.cgroup=c(3,3)} if \code{"Major_1"} is to span columns 1-3 and
#'  \code{"Major_2"} is to span columns 4-6.
#'  \code{rowlabel} does not count in the column numbers. You can omit \code{n.cgroup}
#'  if all groups have the same number of columns. If the \code{n.cgroup} is one less than
#'  the number of columns in the matrix/data.frame then it automatically adds those.
#' @param tspanner The table spanner is somewhat of a table header that
#'  you can use when you want to join different tables with the same columns.
#' @param n.tspanner An integer vector with the number of rows or \code{rgroup}s in the original
#'  matrix that the table spanner should span. If you have provided one fewer n.tspanner elements
#'  the last will be imputed from the number of \code{rgroup}s (if you have provided \code{rgroup} and
#'  \code{sum(n.tspanner) < length(rgroup)}) or the number of rows in the table.
#' @param cspan.rgroup The number of columns that an \code{rgroup} should span. It spans
#'  by default all columns but you may want to limit this if you have column colors
#'  that you want to retain.
#' @param total The last row is sometimes a row total with a border on top and
#'  bold fonts. Set this to \code{TRUE} if you are interested in such a row. If you
#'  want a total row at the end of each table spanner you can set this to \code{"tspanner"}.
#' @param ... Passed on to \code{print.htmlTable} function and any argument except the
#'  \code{useViewer} will be passed on to the \code{\link[base]{cat}} functions arguments.
#'  \emph{Note:} as of version 2.0.0 styling options are still allowed but it is recommended
#'  to instead preprocess your object with \code{\link{addHtmlTableStyle}}.
#' @param ctable If the table should have a double top border or a single a' la LaTeX ctable style
#' @param compatibility Is default set to \code{LibreOffice} as some
#'  settings need to be in old HTML format as Libre Office can't
#'  handle some commands such as the css caption-alignment. Note: this
#'  option is not yet fully implemented for all details, in the future
#'  I aim to generate a HTML-correct table and one that is aimed
#'  at Libre Office compatibility. Word-compatibility is difficult as
#'  Word ignores most settings and destroys all layout attempts
#'  (at least that is how my 2010 version behaves). You can additinally use the
#'  \code{options(htmlTableCompat = "html")} if you want a change to apply
#'  to the entire document.
#'  MS Excel sometimes misinterprets certain cell data when opening HTML-tables (eg. 1/2 becomes 1. February).
#'  To avoid this please specify the correct Microsoft Office format for each cell in the table using the css.cell-argument.
#'  To make MS Excel interpret everything as text use "mso-number-format:\"\\@\"".
#' @param escape.html logical: should HTML characters be escaped? Defaults to FALSE.
#' @return \code{string} Returns a string of class \code{htmlTable}
#'
#' @example inst/examples/htmlTable_example.R
#'
#' @seealso \code{\link{txtMergeLines}},
#'          \code{\link{addHtmlTableStyle}},
#'          \code{\link{setHtmlTableTheme}},
#'          \code{\link[Hmisc]{latex}}
#'
#' @export
#' @rdname htmlTable
#' @family table functions
htmlTable <- function(x,
                      header = NULL,
                      rnames = NULL,
                      rowlabel = NULL,
                      caption = NULL,
                      tfoot = NULL,
                      label = NULL,

                      # Grouping
                      rgroup = NULL,
                      n.rgroup = NULL,

                      cgroup = NULL,
                      n.cgroup = NULL,

                      tspanner = NULL,
                      n.tspanner = NULL,

                      total = NULL,

                      ctable = TRUE,
                      compatibility = getOption("htmlTableCompat", "LibreOffice"),
                      cspan.rgroup = "all",
                      escape.html = FALSE,
                      ...){
  UseMethod("htmlTable")
}

#' @export
htmlTable.data.frame <- function(x, ...) {
  # deal gracefully with an empty data frame - issue a warning.
  if (nrow(x) == 0) {
    warning(paste(deparse(substitute(x)), "is an empty object"))
  }
  htmlTable.default(prConvertDfFactors(x), ...)
}

#' @export
htmlTable.matrix <- function(x, ...) {
  # deal gracefully with an empty matrix - issue a warning.
  if (nrow(x) == 0) {
    warning(paste(deparse(substitute(x)), "is an empty object"))
  }

  # Default to a sum-row when provided a table that
  dots <- list(...)
  if (all(class(x) %in% c("table", "matrix", "array")) &&
      !is.null(rownames(x)) &&
      grepl("^sum$", tail(rownames(x), 1), ignore.case = TRUE) &&
      is.null(dots$total)) {
    dots$total <- TRUE
  }
  dots$x <- x

  do.call(htmlTable.default, dots)
}

`.` <- "magrittr CMD check issue"

#' @importFrom stringr str_replace str_replace_all str_trim
#' @importFrom htmltools htmlEscape
#' @import checkmate
#' @import magrittr
#' @rdname htmlTable
#' @export
htmlTable.default <- function(x,
                              header = NULL,
                              rnames = NULL,
                              rowlabel = NULL,
                              caption = NULL,
                              tfoot = NULL,
                              label = NULL,

                              # Grouping
                              rgroup = NULL,
                              n.rgroup = NULL,

                              cgroup = NULL,
                              n.cgroup = NULL,

                              tspanner = NULL,
                              n.tspanner = NULL,

                              total = NULL,

                              ctable = TRUE,
                              compatibility = getOption("htmlTableCompat", "LibreOffice"),
                              cspan.rgroup = "all",
                              escape.html = FALSE,
                              ...)
{
  if (isTRUE(escape.html)) {
    x <- prEscapeHtml(x)
  }

  x <- prPrepInputMatrixDimensions(x, header = header)
  dots <- list(...)
  style_dots <- names(dots) %in% Filter(function(x) !(x %in% c("", "x")),
                                        formals(addHtmlTableStyle) %>% names)
  if (sum(style_dots) > 0) {
    style_dots_list <- dots[style_dots]
    dots <- dots[!style_dots]
    style_dots_list$x <- x
    x <- do.call(addHtmlTableStyle, style_dots_list)
  }

  style_list <- prGetAttrWithDefault(x,
                                     which = style_attribute_name,
                                     default = getHtmlTableTheme())

  if (is.null(rgroup) && !is.null(n.rgroup)) {
    # Add "" rgroups corresponding to the n.rgroups
    rgroup = rep("", length.out = length(n.rgroup))
  }

  # Unfortunately in knitr there seems to be some issue when the
  # rnames is specified immediately as: rnames=rownames(x)
  if (is.null(rnames)) {
    if (any(is.null(rownames(x)) == FALSE))
      rnames <- rownames(x)

    if (any(is.null(rownames(x))) &&
        !is.null(rgroup)) {
      warning("You have not specified rnames but you seem to have rgroups.",
              " If you have the first column as rowname but you want the rgroups",
              " to result in subhedings with indentation below then, ",
              " you should change the rnames to the first column and then",
              " remove it from the table matrix (the x argument object).")
    }
  }

  if (!is.null(rowlabel) &&
      prSkipRownames(rnames))
    stop("You can't have a row label and no rownames.",
         " Either remove the rowlabel argument",
         ", set the rnames argument",
         ", or set the rownames of the x argument.")

  if (is.null(header) && !is.null(colnames(x))) {
    header <- colnames(x)
  } else if (!is.null(header)) {
    if (length(header) != ncol(x))
      stop("You have a header with ", length(header), " cells",
           " while your output matrix has only ", ncol(x), " columns")
  }

  # Fix alignment to match with the matrix
  style_list$align <- prPrepareAlign(style_list$align, x, rnames)
  style_list$align.header <- prPrepareAlign(style_list$align.header, x, rnames, default_rn = "c")

  if (tolower(compatibility) %in% c("libreoffice", "libre office",
                                    "open office", "openoffice",
                                    "word", "ms word", "msword")) {
    compatibility <- "LibreOffice"
  }

  if (!is.null(rgroup)) {
    if (is.null(n.rgroup))
      stop("You need to specify the argument n.rgroup if you want to use rgroups")

    if (any(n.rgroup < 1)) {
      warning("You have provided rgroups with less than 1 elements,",
              " these will therefore be removed: ",
              paste(sprintf("'%s' = %d", rgroup, n.rgroup)[n.rgroup < 1],
                    collapse = ", "))
      rgroup <- rgroup[n.rgroup >= 1]
      n.rgroup <- n.rgroup[n.rgroup >= 1]
    }

    # Sanity check for rgroup
    if (sum(n.rgroup) >  nrow(x)) {
      stop("Your rows are fewer than suggested by the n.rgroup,",
           " i.e. ", sum(n.rgroup) , "(n.rgroup) > ", nrow(x), "(rows in x)")
    }

    if (sum(n.rgroup) < nrow(x) &&
        (length(n.rgroup) == length(rgroup) - 1 ||
         length(n.rgroup) == length(rgroup))) {
      # Add an empty rgroup if missing
      if (length(n.rgroup) == length(rgroup))
        rgroup <- c(rgroup, "")
      # Calculate the remaining rows and add those
      n.rgroup <- c(n.rgroup, nrow(x) - sum(n.rgroup))
    }else if (sum(n.rgroup) != nrow(x)) {
      stop("Your n.rgroup doesn't add up")
    }


    # Sanity checks style_list$css.rgroup and prepares the style
    if (length(style_list$css.rgroup) > 1 &&
        length(style_list$css.rgroup) != length(rgroup))
      stop(sprintf("You must provide the same number of styles as the rgroups, %d != %d",
                   length(style_list$css.rgroup), length(rgroup)))
    else if (length(style_list$css.rgroup) == 1) {
      style_list$css.rgroup <- prGetStyle(style_list$css.rgroup)

      if (length(rgroup) > 0)
        style_list$css.rgroup <- rep(style_list$css.rgroup, length.out = length(rgroup))
    } else {
      for (i in 1:length(style_list$css.rgroup))
        style_list$css.rgroup[i] <- prGetStyle(style_list$css.rgroup[i])
    }

    # Sanity checks style_list$css.rgroup.sep and prepares the style
    if (length(style_list$css.rgroup.sep) > 1 &&
        length(style_list$css.rgroup.sep) != length(rgroup) - 1)
      stop(sprintf("You must provide the same number of separators as the rgroups - 1, %d != %d",
                   length(style_list$css.rgroup.sep), length(rgroup) - 1))
    else if (length(style_list$css.rgroup.sep) == 1) {
      style_list$css.rgroup.sep <- prAddSemicolon2StrEnd(style_list$css.rgroup.sep)

      if (length(rgroup) > 0)
        style_list$css.rgroup.sep <- rep(style_list$css.rgroup.sep, length.out = length(rgroup))
    } else {
      for (i in 1:length(style_list$css.rgroup.sep))
        style_list$css.rgroup.sep[i] <- prAddSemicolon2StrEnd(style_list$css.rgroup.sep[i])
    }

    cspan.rgroup <- rep(cspan.rgroup, length.out = length(rgroup))
  }

  ## this will convert color names to hexadecimal (easier for user)
  ## but also leaves hex format unchanged
  style_list$col.rgroup <- prPrepareColors(style_list$col.rgroup, n = nrow(x), ng = n.rgroup, gtxt = rgroup)
  style_list$col.columns <- prPrepareColors(style_list$col.columns, ncol(x))

  if (!is.null(tspanner)) {

    # Sanity checks style_list$css.tspanner and prepares the style
    if (length(style_list$css.tspanner) > 1 &&
        length(style_list$css.tspanner) != length(tspanner))
      stop(sprintf("You must provide the same number of styles as the tspanners, %d != %d",
                   length(style_list$css.tspanner), length(tspanner)))
    else if (length(style_list$css.tspanner) == 1) {
      style_list$css.tspanner <- prAddSemicolon2StrEnd(style_list$css.tspanner)

      if (length(tspanner) > 0)
        style_list$css.tspanner <- rep(style_list$css.tspanner, length.out = length(tspanner))
    } else {
      for (i in 1:length(style_list$css.tspanner))
        style_list$css.tspanner[i] <- prAddSemicolon2StrEnd(style_list$css.tspanner[i])
    }


    # Sanity checks style_list$css.tspanner.sep and prepares the style
    if (length(style_list$css.tspanner.sep) > 1 &&
        length(style_list$css.tspanner.sep) != length(tspanner) - 1)
      stop(sprintf("You must provide the same number of separators as the tspanners - 1, %d != %d",
                   length(style_list$css.tspanner.sep), length(tspanner) - 1))
    else if (length(style_list$css.tspanner.sep) == 1) {
      style_list$css.tspanner.sep <- prGetStyle(style_list$css.tspanner.sep)

      if (length(tspanner) > 0)
        style_list$css.tspanner.sep <- rep(style_list$css.tspanner.sep, length.out = length(tspanner) - 1)
    } else {
      for (i in 1:length(style_list$css.tspanner.sep))
        style_list$css.tspanner.sep[i] <- prGetStyle(style_list$css.tspanner.sep[i])
    }
  }

  # Convert dimnames to something useful
  if (!is.null(names(dimnames(x)))) {
    # First dimname is always the variable name for the row
    dimname4row <- names(dimnames(x))[1]
    if (!is.null(dimname4row) && dimname4row != "") {
      # Use rgroup or tspanner as this is visually more separated than rowlabel
      # if these are available
      if (is.null(rgroup)) {
        rgroup <- dimname4row
        n.rgroup <- nrow(x)
      } else if (is.null(tspanner)) {
        tspanner <- dimname4row
        n.tspanner <- nrow(x)
      } else if (is.null(rowlabel)) {
        rowlabel <- dimname4row
      }
    }

    # Second dimname is always the variable name for the columns
    dimname4col <- names(dimnames(x))[2]
    if (!is.null(dimname4col) && dimname4col != "") {
      # Use rgroup or tspanner as this is visually more separated than rowlabel
      # if these are available
      if (is.null(cgroup)) {
        cgroup <- dimname4col
        n.cgroup <- ncol(x)

        # If this is a addmargins object we shouldn't have the cspanner including the
        # sum marker
        if (!is.null(total) && total &&
            grepl("^sum$", tail(colnames(x), 1), ignore.case = TRUE)) {
          cgroup %<>% c("")
          n.cgroup <- c(n.cgroup[1] - 1, 1)
        }
      }
    }
  }

  # Sanity check for tspanner
  if (!is.null(tspanner)) {
    if (is.null(n.tspanner))
      stop("You need to specify the argument n.tspanner if you want to use table spanners")

    if (any(n.tspanner < 1)) {
      stop("You have  provided invalid number of rows in the n.tspanner argument - minimum is 1, you have: ",
           vector2string(n.tspanner),
           " where no. ", vector2string(which(n.tspanner)),
           " was less than 1")
    }
    if (length(n.tspanner) == length(tspanner) - 1) {
      if (is.null(rgroup) || sum(n.tspanner) > length(rgroup)) {
        n.tspanner = append(n.tspanner, nrow(x) - sum(n.tspanner))
      } else {
        n.tspanner = append(n.tspanner, length(rgroup) - sum(n.tspanner))
      }
    }
    if (any(n.tspanner < 1)) {
      stop("You have more tspannners than n.tspanner while the number of rows doesn't leave room for more tspanners")
    }

    if (sum(n.tspanner) !=  nrow(x)) {
      if (is.null(rgroup))
        stop(sprintf("Your rows don't match in the n.tspanner, i.e. %d != %d",
                     sum(n.tspanner), nrow(x)))

      if (sum(n.tspanner) != length(rgroup))
        stop(sprintf("Your rows don't match either the total number of rows '%d'
                     or the number of rgroups '%d' the sum of n.tspanner %d",
                     nrow(x),
                     length(rgroup),
                     sum(n.tspanner)))

      org_nt <- n.tspanner
      for (i in 1:length(n.tspanner)) {
        offset <- sum(org_nt[0:(i - 1)]) + 1
        n.tspanner[i] = sum(n.rgroup[offset:(offset + org_nt[i] - 1)])
      }
    }

    # Make sure there are no collisions with rgrou
    if (!is.null(n.rgroup)) {
      for (i in 1:length(n.tspanner)) {
        rows <- sum(n.tspanner[1:i])
        if (!rows %in% cumsum(n.rgroup))
          stop("There is no splitter that matches the table spanner ",
               tspanner[i],
               " (no. ", i, ") with rgroup splits.",
               " The missing row splitter should be on row number ", rows,
               " and is not in the n.rgroup list: ", vector2string(n.rgroup),
               " note, it should match the cumulative sum n.rgroup", vector2string(cumsum(n.rgroup)))
      }
    }
  }

  # With multiple rows in cgroup we need to keep track of
  # how many spacer cells occur between the groups
  cgroup_spacer_cells <- rep(0, times = (ncol(x) - 1))

  # Sanity check for cgroup
  if (!is.null(cgroup)) {
    ret <- prPrepareCgroup(x = x,
                           cgroup = cgroup,
                           n.cgroup = n.cgroup,
                           style_list = style_list)

    cgroup <- ret$cgroup
    n.cgroup <- ret$n.cgroup
    cgroup_spacer_cells <- ret$cgroup_spacer_cells
    style_list$align.cgroup <- ret$align.cgroup
    style_list$css.cgroup <- ret$css.cgroup
  }

  style_list$pos.rowlabel <- prGetRowlabelPos(cgroup, style_list$pos.rowlabel, header)

  tc <- getOption("table_counter", FALSE)
  if (tc) {
    # Count which table it currently is
    if (is.numeric(tc))
      tc <- tc + 1
    else
      tc <- 1
    options(table_counter = tc)
  }

  # The id works just as well as any anchor
  table_id <- getOption("table_counter", "")
  if (!is.null(label)) {
    table_id <- sprintf(" id='%s'", label)
  }else if (is.numeric(table_id)) {
    table_id <- paste0(" id='table_", table_id, "'")
  }else if (table_id == FALSE) {
    table_id <- ""
  }

  # A column counter that is used for <td colspan="">
  total_columns <- ncol(x) + !prSkipRownames(rnames)
  if (!is.null(cgroup)) {
    if (!is.matrix(cgroup)) {
      total_columns <- total_columns + length(cgroup) - 1
    }else{
      total_columns <- total_columns + sum(cgroup_spacer_cells)
    }
  }

  if (is.null(total) ||
      (is.logical(total) &&
       all(total == FALSE))) {
    total = c()
  }else if (is.logical(total)) {
    if (length(total) == 1) {
      total <- nrow(x)
    }else if (length(total) == nrow(x)) {
      total <- which(total)
    }else if (!is.null(n.tspanner) &&
              length(total) == length(n.tspanner)) {
      total <- cumsum(n.tspanner)[total]
    }else{
      stop("You have provided an invalid 'total' argument:",
           " '", paste(total, collapse = "', '"), "'.",
           " Logical values accepted are either single TRUE elements",
           ", of the same length as the output matrix (", nrow(x), ")",
           ", or of the same length as the tspanner (",
           ifelse(is.null(n.tspanner), "not provided", length(n.tspanner)), ").")
    }
  }else if (is.numeric(total)) {
    if (any(!total %in% 1:nrow(x)))
      stop("You have indicated an invalid row as the total row.",
           " Valid rows are only 1 to ", nrow(x),
           " and you have provided invalid row(s): ",
           "'", paste(total[!total %in% 1:nrow(x)], collapse = "', '"), "'")
  }else if (all(total == "tspanner")) {
    total <- cumsum(n.tspanner)
  }else{
    stop("You have provided an invalid 'total' argument:",
         " '", paste(total, collapse = "', '"), "' ",
         " of the class ", paste(class(total), collapse = " & "), ".",
         " The function currently only accepts logical or numerical",
         " values.")
  }

  style_list$css.total <- rep(style_list$css.total, length.out = length(total))

  assert(
    check_matrix(style_list$css.cell),
    check_character(style_list$css.cell)
  )
  prepped_cell_css <- prPrepareCss(x,
                                   css = style_list$css.cell,
                                   rnames = rnames, header = header,
                                   style_list = style_list)

  ###############################
  # Start building table string #
  ###############################
  table_str <- sprintf("<table class='%s' style='border-collapse: collapse; %s' %s>",
                       paste(style_list$css.class, collapse = ", "),
                       paste(style_list$css.table, collapse = "; "),
                       table_id)

  # Theoretically this should be added to the table but the
  # import to word processors works then less well and therefore I've
  # constructed this work-around with borders for the top and bottom cells
  first_row <- TRUE;
  if (isTRUE(ctable)) {
    top_row_style = "border-top: 2px solid grey;"
    bottom_row_style = "border-bottom: 2px solid grey;"
  } else if (any(ctable %in% c('single', 'double'))) {
    ctable <- rep_len(ctable, 2L)
    ctable[ctable %in% 'single'] <- 'solid'
    top_row_style = ifelse(ctable[1] ==  'solid', "border-top: 2px solid grey;", "border-top: 4px double grey;")
    bottom_row_style = ifelse(ctable[2] ==  'solid',
                              "border-bottom: 2px solid grey;",
                              "border-bottom: 4px double grey;")
  } else {
    top_row_style = "border-top: 4px double grey;"
    bottom_row_style = "border-bottom: 1px solid grey;"
  }


  # Add caption according to standard HTML
  if (!is.null(caption)) {
    # Combine a table counter if provided
    caption <- paste0("\n\t", prTblNo(caption))

    if (compatibility != "LibreOffice") {
      if (style_list$pos.caption %in% c("bottom", "below")) {
        table_str %<>%
          paste0("\n\t<caption style='caption-side: bottom'>")
      }else{
        table_str %<>%
          paste0("\n\t<caption style='caption-side: top'>")
      }

      table_str %<>%
        paste0(caption, "</caption>")
    }
  }

  if (!is.null(header) ||
      !is.null(cgroup) ||
      !is.null(caption)) {
    thead <- prGetThead(x = x,
                        header = header,
                        cgroup = cgroup,
                        n.cgroup = n.cgroup,
                        caption = caption,
                        compatibility = compatibility,
                        total_columns = total_columns,
                        style_list = style_list,
                        top_row_style = top_row_style,
                        rnames = rnames,
                        rowlabel = rowlabel,
                        cgroup_spacer_cells = cgroup_spacer_cells,
                        prepped_cell_css = prepped_cell_css,
                        cell_style = cell_style)
    first_row <- FALSE
    table_str %<>%
      paste0(thead)

  }

  table_str %<>%
    paste0("\n\t<tbody>")

  if (is.null(rgroup))
    row_clrs <- style_list$col.rgroup
  else
    row_clrs <- unlist(attr(style_list$col.rgroup, "group"))

  rgroup_iterator <- 0
  tspanner_iterator <- 0
  if (nrow(x) > 0) {
    for (row_nr in 1:nrow(x)) {
      rname_style = attr(prepped_cell_css, "rnames")[row_nr]

      # First check if there is a table spanner that should be applied
      if (!is.null(tspanner) &&
          (row_nr == 1 ||
           row_nr > sum(n.tspanner[1:tspanner_iterator]))) {
        tspanner_iterator = tspanner_iterator + 1

        rs <- c(rname_style,
                style_list$css.tspanner[tspanner_iterator])

        # Use a separator from the one above if this
        # at least the second spanner. Graphically this
        # appears as if underneath the group while it's
        # actually above but this merges into one line
        if (tspanner_iterator > 1) {
          rs %<>%
            c(style_list$css.tspanner.sep[tspanner_iterator - 1])
        }


        if (first_row) {
          rs %<>%
            c(top_row_style)
        }

        table_str %<>%
          sprintf("%s\n\t<tr><td colspan='%d' style='%s'>%s</td></tr>",
                  .,
                  total_columns,
                  prGetStyle(rs),
                  tspanner[tspanner_iterator])
        first_row <- FALSE
      }


      # Add the row group if any
      # and it's:
      # - first row
      # - the row belongs to the next row group
      rgroup_sep_style <- FALSE
      if (!is.null(rgroup) &&
          (row_nr == 1 ||
           row_nr > sum(n.rgroup[1:rgroup_iterator]))) {
        rgroup_iterator = rgroup_iterator + 1

        rs <- c(rname_style,
                style_list$css.rgroup[rgroup_iterator],
                `background-color` = style_list$col.rgroup[rgroup_iterator])

        # Use a separator from the one above if this
        # at least the second group. Graphically this
        # appears as if underneath the group while it's
        # actually above but this merges into one line
        if (rgroup_iterator > 1) {
          rs <- c(rs,
                  style_list$css.rgroup.sep[rgroup_iterator - 1])
        }

        # Only add if there is anything in the group
        if (is.na(rgroup[rgroup_iterator]) == FALSE &&
            rgroup[rgroup_iterator] != "") {

          if (first_row) {
            rs <- c(rs,
                    top_row_style)
          }

          rgroup_str <- prGetRgroupLine(x = x,
                                        total_columns = total_columns,
                                        rgroup = rgroup,
                                        rgroup_iterator = rgroup_iterator,
                                        cspan = cspan.rgroup[rgroup_iterator],
                                        rnames = rnames,
                                        style = rs,
                                        cgroup_spacer_cells = cgroup_spacer_cells,
                                        style_list = style_list,
                                        prepped_row_css = prepped_cell_css[row_nr,])

          table_str %<>%
            paste(rgroup_str)

          first_row <- FALSE
        }else if (rgroup_iterator > 1 && style_list$css.rgroup.sep[rgroup_iterator - 1] != "") {
          # Add the separator if the rgroup wasn't added so that it's included in the regular cells
          rgroup_sep_style = style_list$css.rgroup.sep[rgroup_iterator - 1]
        }
      }


      cell_style <- rs <- paste("background-color:", row_clrs[row_nr])
      if (first_row) {
        rs %<>%
          c(top_row_style)
        cell_style %<>%
          c(top_row_style)
      }else if (rgroup_sep_style != FALSE) {
        rs %<>% c(rgroup_sep_style)
      }
      first_row <- FALSE

      if (row_nr == nrow(x)) {
        cell_style %<>%
          c(bottom_row_style)
      }

      if (row_nr %in% total) {
        cell_style %<>%
          c(style_list$css.total[which(row_nr == total)])
      }

      if (prGetStyle(rs) == "") {
        table_str %<>%
          paste0("\n\t<tr>")
      }else{
        table_str %<>%
          sprintf("%s\n\t<tr style='%s'>",
                  .,
                  prGetStyle(rs))
      }

      if (!prSkipRownames(rnames)) {
        pdng <- style_list$padding.tspanner
        # Minor change from original function. If the group doesn't have
        # a group name then there shouldn't be any indentation
        if (!is.null(rgroup) &&
            rgroup_iterator > 0 &&
            is.na(rgroup[rgroup_iterator]) == FALSE &&
            rgroup[rgroup_iterator] != "") {
          pdng %<>%
            paste0(style_list$padding.rgroup)
        }

        # The padding doesn't work well with the Word import - well nothing really works well with word...
        # table_str <- sprintf("%s\n\t\t<td style='padding-left: .5em;'>%s</td>", table_str, rnames[row_nr])
        table_str %<>%
          sprintf("%s\n\t\t<td style='%s'>%s%s</td>",
                  .,
                  prGetStyle(c(rname_style, cell_style),
                             align = prGetAlign(style_list$align, 1)),
                  pdng,
                  rnames[row_nr])
      }

      cell_str <- prAddCells(rowcells = x[row_nr,],
                             cellcode = "td",
                             style_list = style_list,
                             style = cell_style,
                             cgroup_spacer_cells = cgroup_spacer_cells,
                             has_rn_col = !prSkipRownames(rnames)*1,
                             prepped_cell_css = prepped_cell_css[row_nr, ])
      table_str %<>%
        paste0(cell_str, "\n\t</tr>")
    }
  }
  # Close body
  table_str %<>%
    paste0("\n\t</tbody>")

  if (!is.null(caption) &
      compatibility == "LibreOffice" &
      style_list$pos.caption %in% c("bottom", "below")) {

    table_str %<>%
      sprintf("%s\n\t<tr><td colspan='%d' style='text-align: left;'>%s</td></tr>",
              .,
              total_columns,
              caption)
  }

  # Add footer
  if (!is.null(tfoot)) {
    table_str %<>%
      sprintf("%s\n\t<tfoot><tr><td colspan='%d'>",
              .,
              total_columns)

    # Add the body
    table_str %<>%
      paste0("\n\t", txtMergeLines(tfoot))

    table_str %<>% paste0("</td></tr></tfoot>")
  }

  # Close table
  table_str %<>%
    paste0("\n</table>")

  # Fix indentation issue with pandoc v1.13 - can be overridden if you want to look at a pretty `cat()`
  if (!getOption("htmlTable.pretty_indentation", default = FALSE)) {
    table_str %<>% gsub("\t", "", .)
  }

  # HTML favors UTF-8 and thus the string should be encoded as utf8
  table_str <- enc2utf8(table_str)
  class(table_str) <- c("htmlTable", class(table_str))
  attr(table_str, "...") <- dots
  attr(table_str, "html") <- TRUE

  # Add html class if this is a table inside a notebook for inline output
  if (!getOption('htmlTable.skip_notebook', FALSE) && prIsNotebook()) {
    class(table_str) <- c("html", class(table_str))
  }

  return(table_str)
}

#' @importFrom methods setClass
setClass("htmlTable", contains = "character")
