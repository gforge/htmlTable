#' Outputting HTML tables
#'
#' This is a function for outputting a more advanced
#' table than what \pkg{xtable}, \pkg{ztable}, or \pkg{knitr}'s
#' \code{\link[knitr]{kable}()} allows.
#' It's aim is to provide the \pkg{Hmisc} \code{\link[Hmisc]{latex}()}
#' colgroup and rowgroup functions in HTML. The html-output is designed for
#' maximum compatibility with LibreOffice/OpenOffice.
#'
#' @section Multiple rows of column spanners \code{cgroup}:
#'
#' If you want to have a column spanner in multiple levels you can
#' set the \code{cgroup} and \code{n.cgroup} arguments to matrices.
#' If the different levels have different number of elements you
#' need to set the ones that lack elements to NA. For instance
#' \code{cgroup = rbind(c("first", "second", NA), c("a", "b", "c"))}.
#' And the corresponding n,cgroup would be \code{n.cgroup = rbind(c(1, 2, NA), c(2, 1, 2))}.
#' for a table consisting of 5 columns. The "first" spans the first two columns,
#' the "second" spans the last three columns, "a" spans the first two, "b"
#' the middle column, and "c" the last two columns.
#'
#' @section The add attribute to \code{rgroup}:
#'
#' You can now have an additional element at the rgroup level by specifying the
#' \code{att(rgroup, 'add')}. The value can either be a vector or a list of the
#' same length as the rgroup or a list/vector with names corresponding to integers
#' within the rgroup span.
#'
#' @section Important \pkg{knitr}-note:
#'
#' This funciton will only work with \pkg{knitr} outputting \emph{html}, i.e.
#' markdown mode. As the function returns raw html-code
#' the compatibility with non-html formatting is limited,
#' even with \href{http://johnmacfarlane.net/pandoc/}{pandoc}.
#'
#' Thanks to the the \code{\link[knitr]{knit_print}} and the
#' \code{\link[knitr]{asis_output}}
#' the \code{results='asis'} is \emph{no longer needed} except within for-loops.
#' If you have a knitr-chunk with a for loop and use \code{print()} to produce
#' raw html you must set the chunk option \code{results='asis'}. \code{Note}:
#' the print-function relies on the \code{\link[base]{interactive}()} function
#' for determining if the output should be sent to a browser or to the terminal.
#' In vignettes and other directly knitted documents you may need to either set
#' \code{useViewer = FALSE} alternatively set \code{options(htmlTable.cat = TRUE)}.
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
#' then the table counter will use Roman numumerals instead of Arabic.
#'
#' @section Possible issues:
#'
#' Note that when using complex cgroup alignments with multiple levels
#' not every browser is able to handle this. For instance the RStudio
#' webkit browser seems to have issues with this and a
#' \href{http://code.google.com/p/chromium/issues/detail?id=305130}{bug has been filed}.
#'
#' As the table uses html for rendering you need to be aware of that headers,
#' rownames, and cell values should try respect this for optimal display. Browsers
#' try to compensate and frequently the tables still turn out fine but it is
#' not advized. Most importantly you should try to use
#' \code{&lt;} instead of \code{<} and
#' \code{&gt;} instead of \code{>}. You can find a complete list
#' of html characters \href{http://ascii.cl/htmlcodes.htm}{here}.
#'
#' @param x The matrix/data.frame with the data. For the \code{print} and \code{knit_print}
#'  it takes a string of the class \code{htmlTable} as \code{x} argument.
#' @param header A vector of character strings specifying column
#'  header, defaulting to \code{\link[base]{colnames}(x)}
#' @param rnames Default rownames are generated from \code{\link[base]{rownames}(x)}. If you
#'  provide \code{FALSE} then it will skip the rownames. \emph{Note:} For \code{data.frames}
#'  if you do \code{\link[base]{rownames}(my_dataframe) <- NULL} it still has
#'  rownames. Thus you need to use \code{FALSE} if you want to
#'  surpress rownames for \code{data.frames}.
#' @param rowlabel If the table has rownames or \code{rnames},
#'  rowlabel is a character string containing the
#'  column heading for the \code{rnames}.
#' @param caption Adds a table caption.
#' @param tfoot Adds a table footer (uses the \code{<tfoot>} html element). The
#'  output is run through \code{\link{txtMergeLines}} simplifying the generation
#'  of multiple lines.
#' @param label A text string representing a symbolic label for the
#'  table for referencing as an anchor. All you need to do is to reference the
#'  table, for instance \code{<a href="#anchor_name">see table 2</a>}. This is
#'  known as the element's id attribute, i.e. table id, in HTML linguo, and should
#'  be unique id for an HTML element in contrast to the \code{css.class} element attribute.
#'
#' @param align A character strings specifying column alignments, defaulting to
#'  \code{\link[base]{paste}(rep('c',ncol(x)),collapse='')} to center. Valid alignments are
#'  l = left, c = center and r = right. You can also specify \code{align='c|c'} and
#'  other LaTeX tabular formatting. If you want to set the alignment of the
#'  rownames this string needst to be \code{ncol(x) + 1}, otherwise it automatically
#'  pads the string with a left alignment for the rownames.
#' @param align.header A character strings specifying alignment for column header,
#'  defaulting to centered, i.e. \code{\link[base]{paste}(rep('c',ncol(x)),collapse='')}.
#' @param align.cgroup The justification of the \code{cgroups}
#'
#' @param rgroup A vector of character strings containing headings for row groups.
#'  \code{n.rgroup} must be present when \code{rgroup} is given. The first
#'  \code{n.rgroup[1]}rows are sectioned off and \code{rgroup[1]} is used as a bold
#'  heading for them. The usual row dimnames (which must be present if \code{rgroup} is)
#'  are indented. The next \code{n.rgroup[2]} rows are treated likewise, etc. If you don't
#'  want a row to be part of a row group then you just put "" for that row, remember to add
#'  the corresponding number of rows in n.rgroup.
#' @param n.rgroup An integer vector giving the number of rows in each grouping. If \code{rgroup}
#'  is not specified, \code{n.rgroup} is just used to divide off blocks of rows by horizontal
#'  lines. If \code{rgroup} is given but \code{n.rgroup} is omitted, \code{n.rgroup} will
#'  default so that each row group contains the same number of rows.
#' @param cgroup A vector or a matrix of character strings defining major column header. The default
#'  is to have none. These elements are also known as \emph{column spanners}. If you want a column \emph{not}
#'  to have a spanner then put that column as "". If you pass cgroup and \code{n.crgroup} as
#'  matrices you can have column spanners for several rows. See cgroup section below for details.
#' @param n.cgroup An integer vector or matrix containing the number of columns for which each element in
#'  cgroup is a heading. For example, specify \code{cgroup=c("Major_1","Major_2")},
#'  \code{n.cgroup=c(3,3)} if \code{"Major_1"} is to span columns 1-3 and
#'  \code{"Major_2"} is to span columns 4-6.
#'  \code{rowlabel} does not count in the column numbers. You can omit \code{n.cgroup}
#'  if all groups have the same number of columns.
#' @param tspanner The table spanner is somewhat of a table header that
#'  you can use when you want to join different tables with the same columns.
#' @param n.tspanner An integer vector with the number of rows in the original matrix that
#'  the table spanner should span.
#' @param total The last row is sometimes a row total with a border on top and
#'  bold fonts. Set this to \code{TRUE} if you are interested in such a row. If you
#'  want a total row at the end of each table spanner you can set this to \code{"tspanner"}.
#'
#' @param css.rgroup CSS style for the rgorup, if different styles are wanted for each of the
#'  rgroups you can just specify a vector with the number of elements
#' @param css.rgroup.sep The line between different rgroups. The line is set to the TR element
#'  of the lower rgroup, i.e. you have to set the border-top/padding-top etc to a line with
#'  the expected function. This is only used for rgroups that are printed. You can specify
#'  different separators if you give a vector of rgroup - 1 length (this is since the first
#'  rgroup doesn't have a separator).
#' @param css.tspanner The CSS style for the table spanner
#' @param css.tspanner.sep The line between different spanners
#' @param css.total The css of the total row
#' @param css.cell The css.cell element allows you to add any possible CSS style to your
#'  table cells. If you provide a vector the vector it is assummed that the styles should
#'  be repeated throughout the columns. If you provide a matrix of the same size as your
#'  \code{x} argument. If have \code{ncol(x) + 1} the first row will correspond to the
#'  rowname style. Correspondingly if the size is \code{nrow(x) + 1} it is assummed that the
#'  first row is the header row.
#' @param css.class The html CSS class for the table. This allows directing html
#'  formatting through \href{http://www.w3schools.com/css/css_selectors.asp}{CSS}
#'  directly at all instances of that class. \emph{Note:} unfortunately the
#'  CSS is frequently ignored by word processors. This option
#'  is mostly inteded for web-presentations.
#' @param css.cgroup The same as \code{css.class} but for cgroup formatting.
#'
#' @param pos.rowlabel Where the rowlabel should be positioned. This value can be \code{"top"},
#'  \code{"bottom"}, \code{"header"}, or a integer between \code{1} and \code{nrow(cgroup) + 1}. The options
#'  \code{"bottom"} and \code{"header"} are the same, where the row label is presented at the same level as
#'  the header.
#' @param pos.caption Set to \code{"bottom"} to position a caption below the table
#'  instead of the default of \code{"top"}.
#' @param cspan.rgroup The number of columns that an \code{rgroup} should span. It spans
#'  by default all columns but you may want to limit this if you have column colors
#'  that you want to retain.
#'
#' @param ... Passed on to \code{print.htmlTable} function and any argument except the
#'  \code{useViewer} will be passed on to the \code{\link[base]{cat}} functions arguments.
#'
#' @param col.rgroup Alternating colors (zebra striping/banded rows) for each \code{rgroup}; one or two colors
#'  is recommended and will be recycled.
#' @param col.columns Alternating colors for each column.
#'
#' @param padding.rgroup Generally two non-breakings spaces, i.e. \code{&nbsp;&nbsp;}, but some
#'  journals only have a bold face for the rgroup and leaves the subelements unindented.
#' @param padding.tspanner The table spanner is usually without padding but you may specify padding
#'  similar to \code{padding.rgroup} and it will be added to all elements, including the rgroup elements.
#'  This allows for a 3-level hierarchy if needed.
#' @param ctable If the table should have a double top border or a single a' la LaTeX ctable style
#' @param compatibility Is default set to \code{LibreOffice} as some
#'  settings need to be in old html format as Libre Office can't
#'  handle some commands such as the css caption-alignment. Note: this
#'  option is not yet fully implemented for all details, in the future
#'  I aim to generate a html-correct table and one that is aimed
#'  at Libre Office compatibility. Word-compatibility is difficult as
#'  Word ignores most settings and destroys all layout attempts
#'  (at least that is how my 2010 version behaves).
#' @return \code{string} Returns a string of class htmlTable
#'
#' @example inst/examples/htmlTable_example.R
#'
#' @seealso \code{\link{txtMergeLines}},
#'          \code{\link[Hmisc]{latex}}
#'
#' @export
#' @rdname htmlTable
#' @family table functions
htmlTable <- function(x, ...){
  UseMethod("htmlTable")
}

`.` <- "magrittr RCM check issue"

#' @importFrom stringr str_trim
#' @importFrom stringr str_replace
#' @import magrittr
#' @rdname htmlTable
#' @export
htmlTable.default <- function(x,
                              header,
                              rnames,
                              rowlabel,
                              caption,
                              tfoot,
                              label,

                              # Grouping
                              rgroup,
                              n.rgroup,

                              cgroup,
                              n.cgroup,

                              tspanner,
                              n.tspanner,

                              total,

                              # Alignment
                              align = paste(rep('c',ncol(x)),collapse=''),
                              align.header= paste(rep('c',ncol(x)),collapse=''),
                              align.cgroup,

                              # CSS stuff
                              css.rgroup = "font-weight: 900;",
                              css.rgroup.sep = "",

                              css.tspanner = "font-weight: 900; text-align: left;",
                              css.tspanner.sep = "border-top: 1px solid #BEBEBE;",

                              css.total = "border-top: 1px solid #BEBEBE; font-weight: 900;",

                              css.cell = "",
                              css.cgroup = "",

                              css.class = "gmisc_table",

                              # Positions
                              pos.rowlabel = "bottom",
                              pos.caption='top',

                              # Colors
                              col.rgroup = 'none',
                              col.columns =  'none',

                              # More alternatives
                              padding.rgroup = "&nbsp;&nbsp;",
                              padding.tspanner = "",
                              ctable = TRUE,
                              compatibility = "LibreOffice",
                              cspan.rgroup = "all",
                              ...)
{
  # Warnings due to interface changes in 1.0
  API_changes <-
    c(rowname = "rnames",
      headings = "header",
      halign = "align.header",
      cgroup.just = "align.cgroup",
      rgroupCSSstyle = "css.rgroup",
      rgroupCSSseparator = "css.rgroup.sep",
      tspannerCSSstyle = "css.tspanner",
      tspannerCSSseparator = "css.tspanner.sep",
      rgroup.padding = "padding.rgroup",
      rowlabel.pos =  "pos.rowlabel",
      caption.loc  = "pos.caption",
      altcol = "col.rgroup",
      tableCSSclass = "css.class")
  dots <- list(...)
  fenv <- environment()
  for (i in 1:length(API_changes)){
    old_name <- names(API_changes)[i]
    new_name <- API_changes[i]
    if (old_name %in% names(dots)){
      if (class(fenv[[new_name]]) == "name"){
        fenv[[new_name]] <- dots[[old_name]]
        dots[[old_name]] <- NULL
        warning("Deprecated: '", old_name, "'",
                " argument is now '", new_name ,"'",
                " as of ver. 1.0")
      }else{
        stop("You have set both the old parameter name: '", old_name, "'",
             " and the new parameter name: '", new_name, "'.",
             " Note that parameters may have a default value and you may have only",
             " set the old paramter while the function automatically attaches a value to the new parameter")
      }
    }
  }

  if (is.null(dim(x))){
    x <- matrix(x, ncol = ifelse(missing(header),
                                 length(x),
                                 length(header)))
  }else if (length(dim(x)) != 2)
    stop("Your table variable seems to have the wrong dimension,",
         " length(dim(x)) = ", length(dim(x)) , " != 2")


  ## this will convert color names to hexadecimal (easier for user)
  ## but also leaves hex format unchanged
  col.rgroup <- prPrepareColors(col.rgroup, n = nrow(x), ng = n.rgroup, gtxt = rgroup)
  col.columns <- prPrepareColors(col.columns, ncol(x))

  # Unfortunately in knitr there seems to be some issue when the
  # rnames is specified immediately as: rnames=rownames(x)
  if (missing(rnames)){
    if (any(is.null(rownames(x)) == FALSE))
      rnames <- rownames(x)
  }

  if (missing(rnames) &&
        any(is.null(rownames(x))) &&
        !missing(rgroup)){
    warning("You have not specified rnames but you seem to have rgroups.",
            " If you have the first column as rowname but you want the rgroups",
            " to result in subhedings with indentation below then, ",
            " you should change the rnames to the first column and then",
            " remove it from the table matrix (the x argument object).")
  }

  if (!missing(rowlabel) &&
        prSkipRownames(rnames))
    stop("You can't have a row label and no rownames.",
         " Either remove the rowlabel argument",
         ", set the rnames argument",
         ", or set the rownames of the x argument.")

  if (missing(header) &&
        !is.null(colnames(x))){
    header<-colnames(x)
  }else if(!missing(header)){
    if (length(header) != ncol(x))
      stop("You have a header with ", length(header), " cells",
           " while your output matrix has only ", ncol(x), " columns")
  }

  # Fix alignment to match with the matrix
  align <- prPrepareAlign(align, x, rnames)
  align.header <- prPrepareAlign(align.header, x, rnames, default_rn = "c")

  if (tolower(compatibility) %in% c("libreoffice", "libre office",
                                    "open office", "openoffice",
                                    "word", "ms word", "msword")){
    compatibility <- "LibreOffice"
  }

  if (!missing(rgroup)){
    if (missing(n.rgroup))
      stop("You need to specify the argument n.rgroup if you want to use rgroups")

    if (any(n.rgroup < 1)){
      warning("You have provided rgroups with less than 1 elements,",
              " these will therefore be removed: ",
              paste(sprintf("'%s' = %d", rgroup, n.rgroup)[n.rgroup < 1],
                    collapse=", "))
      rgroup <- rgroup[n.rgroup >= 1]
      n.rgroup <- n.rgroup[n.rgroup >= 1]
    }
    # Sanity check for rgroup
    if (sum(n.rgroup) !=  nrow(x))
      stop("Your rows don't match in the n.rgroup,",
           " i.e. ", sum(n.rgroup) , "(n.rgroup) != ", nrow(x), "(rows in x)")

    # Sanity checks css.rgroup and prepares the style
    if (length(css.rgroup) > 1 &&
          length(css.rgroup) != length(rgroup))
      stop(sprintf("You must provide the same number of styles as the rgroups, %d != %d",
                   length(css.rgroup), length(rgroup)))
    else if(length(css.rgroup) == 1){
      css.rgroup <- prGetStyle(css.rgroup)

      if (length(rgroup) > 0)
        css.rgroup <- rep(css.rgroup, length.out=length(rgroup))
    } else {
      for (i in 1:length(css.rgroup))
        css.rgroup[i] <- prGetStyle(css.rgroup[i])
    }

    # Sanity checks css.rgroup.sep and prepares the style
    if (length(css.rgroup.sep) > 1 &&
          length(css.rgroup.sep) != length(rgroup)-1)
      stop(sprintf("You must provide the same number of separators as the rgroups - 1, %d != %d",
                   length(css.rgroup.sep), length(rgroup)-1))
    else if(length(css.rgroup.sep) == 1){
      css.rgroup.sep <- prAddSemicolon2StrEnd(css.rgroup.sep)

      if (length(rgroup) > 0)
        css.rgroup.sep <- rep(css.rgroup.sep, length.out=length(rgroup))
    } else {
      for (i in 1:length(css.rgroup.sep))
        css.rgroup.sep[i] <- prAddSemicolon2StrEnd(css.rgroup.sep[i])
    }

    cspan.rgroup <- rep(cspan.rgroup, length.out = length(rgroup))
  }

  if (!missing(tspanner)){

    # Sanity checks css.tspanner and prepares the style
    if (length(css.tspanner) > 1 &&
          length(css.tspanner) != length(tspanner))
      stop(sprintf("You must provide the same number of styles as the tspanners, %d != %d",
                   length(css.tspanner), length(tspanner)))
    else if(length(css.tspanner) == 1){
      css.tspanner <- prAddSemicolon2StrEnd(css.tspanner)

      if (length(tspanner) > 0)
        css.tspanner <- rep(css.tspanner, length.out=length(tspanner))
    } else {
      for (i in 1:length(css.tspanner))
        css.tspanner[i] <- prAddSemicolon2StrEnd(css.tspanner[i])
    }


    # Sanity checks css.tspanner.sep and prepares the style
    if (length(css.tspanner.sep) > 1 &&
          length(css.tspanner.sep) != length(tspanner)-1)
      stop(sprintf("You must provide the same number of separators as the tspanners - 1, %d != %d",
                   length(css.tspanner.sep), length(tspanner)-1))
    else if(length(css.tspanner.sep) == 1){
      css.tspanner.sep <- prGetStyle(css.tspanner.sep)

      if (length(tspanner) > 0)
        css.tspanner.sep <- rep(css.tspanner.sep, length.out=length(tspanner)-1)
    } else {
      for (i in 1:length(css.tspanner.sep))
        css.tspanner.sep[i] <- prGetStyle(css.tspanner.sep[i])
    }
  }


  # Sanity check for tspanner
  if (!missing(tspanner)){
    if (missing(n.tspanner))
      stop("You need to specify the argument n.tspanner if you want to use table spanners")

    if(sum(n.tspanner) !=  nrow(x))
      stop(sprintf("Your rows don't match in the n.tspanner, i.e. %d != %d",
                   sum(n.tspanner), nrow(x)))

    # Make sure there are no collisions with rgrou
    if (!missing(n.rgroup)){
      for (i in 1:length(n.tspanner)){
        rows <- sum(n.tspanner[1:i])
        if (!rows %in% cumsum(n.rgroup))
          stop("There is no splitter that matches the table spanner ",
               tspanner[i],
               " (no. ", i, ") with rgroup splits.",
               " The missing row splitter should be on row number ", rows,
               " and is not in the n.rgroup list: ", paste(n.rgroup, collapse=", "),
               " note, it should match the cumulative sum n.rgroup", paste(cumsum(n.rgroup), collapse=", "))
      }
    }
  }

  # With multiple rows in cgroup we need to keep track of
  # how many spacer cells occur between the groups
  cgroup_spacer_cells <- rep(0, times=(ncol(x)-1))

  # Sanity check for cgroup
  if (!missing(cgroup)){
    ret <- prPrepareCgroup(x = x,
                           cgroup = cgroup,
                           n.cgroup = n.cgroup,
                           align.cgroup = align.cgroup,
                           css.cgroup = css.cgroup)

    # TODO: use attach/environment recoding
    cgroup <- ret$cgroup
    n.cgroup <- ret$n.cgroup
    align.cgroup <- ret$align.cgroup
    cgroup_spacer_cells <- ret$cgroup_spacer_cells
    css.cgroup <- ret$css.cgroup
  }

  pos.rowlabel <- prGetRowlabelPos(cgroup, pos.rowlabel, header)

  tc <- getOption("table_counter", FALSE)
  if (tc){
    # Count which table it currently is
    if (is.numeric(tc))
      tc <- tc + 1
    else
      tc <- 1
    options(table_counter = tc)
  }

  # The id works just as well as any anchor
  table_id <- getOption("table_counter", "")
  if (!missing(label)){
    table_id <- sprintf(" id='%s'", label)
  }else if(is.numeric(table_id)){
    table_id <- paste0(" id='table_", table_id, "'")
  }

  # A column counter that is used for <td colspan="">
  total_columns <- ncol(x)+!prSkipRownames(rnames)
  if(!missing(cgroup)){
    if (!is.matrix(cgroup)){
      total_columns <- total_columns + length(cgroup) - 1
    }else{
      total_columns <- total_columns + sum(cgroup_spacer_cells)
    }
  }

  if (missing(total) ||
        (is.logical(total) &&
           all(total == FALSE))){
    total = c()
  }else if (is.logical(total)){
    if (length(total) == 1){
      total <- nrow(x)
    }else if(length(total) == nrow(x)){
      total <- which(total)
    }else if(!missing(n.tspanner) &&
               length(total) == length(n.tspanner)){
      total <- cumsum(n.tspanner)[total]
    }else{
      stop("You have provided an invalid 'total' argument:",
           " '", paste(total, collapse="', '"), "'.",
           " Logical values accepted are either single TRUE elements",
           ", of the same length as the output matrix (", nrow(x), ")",
           ", or of the same length as the tspanner (",
           ifelse(missing(n.tspanner), "not provided", length(n.tspanner)), ").")
    }
  }else if (is.numeric(total)){
    if (any(!total %in% 1:nrow(x)))
      stop("You have indicated an invalid row as the total row.",
           " Valid rows are only 1 to ", nrow(x),
           " and you have provided invalid row(s): ",
           "'", paste(total[!total %in% 1:nrow(x)], collapse="', '"), "'")
  }else if (all(total == "tspanner")){
    total <- cumsum(n.tspanner)
  }else{
    stop("You have provided an invalid 'total' argument:",
         " '", paste(total, collapse="', '"), "' ",
         " of the class ", class(total), ".",
         " The function currently only accepts logical or numerical",
         " values.")
  }

  css.total <- rep(css.total, length.out = length(total))

  css.cell <- prPrepareCss(x, css = css.cell,
                           rnames = rnames, header = header)

  ###############################
  # Start building table string #
  ###############################
  table_str <- sprintf("<table class='%s' style='border-collapse: collapse;' %s>",
                       paste(css.class, collapse=", "),
                       table_id)

  # Theoretically this should be added to the table but the
  # import to word processors works then less well and therefore I've
  # constructed this work-around with borders for the top and bottom cells
  first_row <- TRUE;
  if (ctable){
    top_row_style = "border-top: 2px solid grey;"
    bottom_row_style = "border-bottom: 2px solid grey;"
  } else {
    top_row_style = "border-top: 4px double grey;"
    bottom_row_style = "border-bottom: 1px solid grey;"
  }


  # Add caption according to standard HTML
  if (!missing(caption)){
    # Combine a table counter if provided
    caption <- paste0("\n\t", prTblNo(caption))

    if(compatibility != "LibreOffice"){
      if (pos.caption %in% c("bottom", "below")){
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

  if (!missing(header) ||
        !missing(cgroup)){
    thead <- prGetThead(x = x,
                        header = header,
                        cgroup = cgroup,
                        n.cgroup = n.cgroup,
                        caption = caption,
                        pos.caption = pos.caption,
                        compatibility = compatibility,
                        total_columns = total_columns,
                        align.cgroup = align.cgroup,
                        css.cgroup = css.cgroup,
                        top_row_style = top_row_style,
                        rnames = rnames,
                        rowlabel = rowlabel,
                        pos.rowlabel = pos.rowlabel,
                        cgroup_spacer_cells = cgroup_spacer_cells,
                        css.cell = css.cell,
                        align.header = align.header,
                        cell_style = cell_style)
    first_row <- FALSE
    table_str %<>%
      paste0(thead)

  }

  table_str %<>%
    paste0("\n\t<tbody>")

  if (missing(rgroup))
    row_clrs <- col.rgroup
  else
    row_clrs <- unlist(attr(col.rgroup, "group"))

  rgroup_iterator <- 0
  tspanner_iterator <- 0
  for (row_nr in 1:nrow(x)){
    rname_style = attr(css.cell, "rnames")[row_nr + !prSkipRownames(rnames)]

    # First check if there is a table spanner that should be applied
    if (!missing(tspanner) &&
          (row_nr == 1 ||
             row_nr > sum(n.tspanner[1:tspanner_iterator]))){
      tspanner_iterator = tspanner_iterator + 1

      rs <- c(rname_style,
              css.tspanner[tspanner_iterator])

      # Use a separator from the one above if this
      # at least the second spanner. Graphically this
      # appears as if underneath the group while it's
      # actually above but this merges into one line
      if (tspanner_iterator > 1){
        rs %<>%
          c(css.tspanner.sep[tspanner_iterator-1])
      }


      if (first_row){
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
    if (!missing(rgroup) &&
      (row_nr == 1 ||
        row_nr > sum(n.rgroup[1:rgroup_iterator]))){
      rgroup_iterator = rgroup_iterator + 1

      rs <- c(rname_style,
              css.rgroup[rgroup_iterator],
              `background-color` = col.rgroup[rgroup_iterator])

      # Use a separator from the one above if this
      # at least the second group. Graphically this
      # appears as if underneath the group while it's
      # actually above but this merges into one line
      if (rgroup_iterator > 1){
        rs <- c(rs,
                css.rgroup.sep[rgroup_iterator-1])
      }

      # Only add if there is anything in the group
      if (is.na(rgroup[rgroup_iterator]) == FALSE &&
            rgroup[rgroup_iterator] != ""){

        if (first_row){
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
                                      align = align,
                                      cgroup_spacer_cells = cgroup_spacer_cells,
                                      col.columns = col.columns,
                                      css.row = css.cell[row_nr,],
                                      padding.tspanner = padding.tspanner)

        table_str %<>%
          paste(rgroup_str)

        first_row <- FALSE
      }
    }


    cell_style <- rs <- paste("background-color:", row_clrs[row_nr])
    if (first_row){
      rs %<>%
        c(top_row_style)
      cell_style %<>%
        c(top_row_style)
    }
    first_row <- FALSE

    if (row_nr == nrow(x)){
      cell_style %<>%
        c(bottom_row_style)
    }

    if (row_nr %in% total){
      cell_style %<>%
        c(css.total[which(row_nr == total)])
    }

    if (prGetStyle(rs) == ""){
      table_str %<>%
        paste0("\n\t<tr>")
    }else{
      table_str %<>%
        sprintf("%s\n\t<tr style='%s'>",
                .,
                prGetStyle(rs))
    }

    if (!prSkipRownames(rnames)){
      pdng <- padding.tspanner
      # Minor change from original function. If the group doesn't have
      # a group name then there shouldn't be any indentation
      if (!missing(rgroup) &&
            rgroup_iterator > 0 &&
            is.na(rgroup[rgroup_iterator]) == FALSE &&
            rgroup[rgroup_iterator] != ""){
        pdng %<>%
          paste0(padding.rgroup)
      }

      # The padding doesn't work well with the Word import - well nothing really works well with word...
      # table_str <- sprintf("%s\n\t\t<td style='padding-left: .5em;'>%s</td>", table_str, rnames[row_nr])
      table_str %<>%
        sprintf("%s\n\t\t<td style='%s'>%s%s</td>",
                .,
                prGetStyle(cell_style,
                             align=prGetAlign(align, 1)),
                pdng,
                rnames[row_nr])
    }

    cell_str <- prAddCells(rowcells = x[row_nr,],
                             cellcode = "td",
                             align = align,
                             style = cell_style,
                             cgroup_spacer_cells = cgroup_spacer_cells,
                             has_rn_col = !prSkipRownames(rnames)*1,
                             col.columns = col.columns,
                             css.cell = css.cell[row_nr, ])
    table_str %<>%
      paste0(cell_str, "\n\t</tr>")
  }

  # Close body
  table_str %<>%
    paste0("\n\t</tbody>")

  if (!missing(caption) &
        compatibility == "LibreOffice" &
        pos.caption %in% c("bottom", "below")){

    table_str %<>%
      sprintf("%s\n\t<tr><td colspan='%d' style='text-align: left;'>%s</td></tr>",
              .,
              total_columns,
              caption)
  }

  # Add footer
  if (!missing(tfoot)){
    # Initiate the tfoot
    table_str %<>%
      sprintf("%s\n\t<tfoot><tr><td colspan='%d'>",
              .,
              total_columns)

    # Add the actual tfoot to a new row
    table_str %<>%
      paste0("\n\t", txtMergeLines(tfoot))

    # Close the tfoot
    table_str %<>%
      paste0("</td></tr></tfoot>")
  }

  # Close table
  table_str %<>%
    paste0("\n</table>")

  # Fix indentation issue with pandoc v1.13
  table_str %<>% gsub("\t", "", .)

  class(table_str) <- c("htmlTable", class(table_str))
  attr(table_str, "...") <- list(...)

  return(table_str)
}

#' @export
htmlTable.data.frame <- function(x, ...) {
  # Convert all factors to characters to print them as they expected
  i <- sapply(x, is.factor)
  if(any(i)){
    x[i] <- lapply(x[i], as.character)
  }

  htmlTable.default(x,...)
}

#' @importFrom methods setClass
setClass("htmlTable", contains = "character")


#' @rdname htmlTable
#' @importFrom knitr knit_print
#' @importFrom knitr asis_output
#' @export
knit_print.htmlTable<- function(x, ...){
  asis_output(x)
}

#' @rdname htmlTable
#' @param useViewer If you are using RStudio there is a viewer thar can render
#'  the table within that is envoced if in \code{\link[base]{interactive}} mode.
#'  Set this to \code{FALSE} if you want to remove that  functionality. You can
#'  also force the function to call a specific viewer by setting this to a
#'  viewer function, e.g. \code{useViewer = utils::browseUrl} if you want to
#'  override the default RStudio viewer. Another option that does the same is to
#'  set the \code{options(viewer=utils::browseUrl)} and it will default to that
#'  particular viewer (this is how RStudio decides on a viewer).
#'  \emph{Note:} If you want to force all output to go through the
#'  \code{\link[base]{cat}()} the set \code{\link[base]{options}(htmlTable.cat = TRUE)}.
#' @export
#' @importFrom utils browseURL
print.htmlTable<- function(x, useViewer, ...){
  args <- attr(x, "...")
  # Use the latest ... from the print call
  # and override the original htmlTable call ...
  # if there is a conflict
  print_args <- list(...)
  for (n in names(print_args)){
    args[[n]] <- print_args[[n]]
  }

  # Since the print may be called from another print function
  # it may be handy to allow functions to use attributes for the
  # useViewer parameter
  if (missing(useViewer)){
    if ("useViewer" %in% names(args) &&
      (is.logical(args$useViewer) ||
         is.function(args$useViewer))){
        useViewer <- args$useViewer
        args$useViewer <- NULL
    }else{
      useViewer <- TRUE
    }
  }

  if (interactive() &&
        !getOption("htmlTable.cat", FALSE) &&
        (is.function(useViewer) ||
        useViewer != FALSE))
  {
    if (is.null(args$file)){
      args$file <- tempfile(fileext=".html")
    }

    htmlPage <- paste("<html>",
                      "<head>",
                      "<meta http-equiv=\"Content-type\" content=\"text/html; charset=UTF-8\">",
                      "</head>",
                      "<body>",
                      "<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
                      x,
                      "</div>",
                      "</body>",
                      "</html>", sep="\n")
    # We only want to use those arguments that are actually in cat
    # anything else that may have inadvertadly slipped in should
    # be ignored or it will be added to the output
    cat_args <- args
    cat_args <- cat_args[names(cat_args) %in% names(formals(cat))[-1]]
    do.call(cat, c(htmlPage, cat_args))

    if (is.function(useViewer)){
      useViewer(args$file)
    }else{
      viewer <- getOption("viewer")
      if (!is.null(viewer) &&
            is.function(viewer)){
        # (code to write some content to the file)
        viewer(args$file)
      }else{
        utils::browseURL(args$file)
      }
    }
  }else{
    cat_args <- args
    cat_args <- cat_args[names(cat_args) %in% names(formals(cat))[-1]]
    do.call(cat, c(x, cat_args))
  }

  invisible(x)
}

#' Gets the last table number
#'
#' The function relies on \code{options("table_counter")}
#' in order to keep track of the last number.
#'
#' @param roman Whether or not to use roman numbers instead
#'  of arabic. Can also be set through \code{options(table_caption_no_roman = TRUE)}
#'
#' @export
#' @examples
#' org_opts <- options(table_counter=1)
#' tblNoLast()
#' options(org_opts)
#' @family table functions
tblNoLast <- function(roman = getOption("table_counter_roman",
                                        FALSE)){
  last_no <- getOption("table_counter")
  if (is.logical(last_no) ||
        is.null(last_no)){
    stop("You cannot call the get last figure number",
         " when there has been no prior figure registerd.",
         " In other words, you need to call the fiCapNo()",
         " on a figure before you call this function.",
         " If you want the next number then call figCapNoNext()",
         " instead of this function.")
  }

  if (roman)
    last_no <- as.character(as.roman(last_no))

  return(last_no)
}

#' Gets the next table number
#'
#' The function relies on \code{options("table_counter")}
#' in order to keep track of the last number.
#'
#' @inheritParams tblNoLast
#' @export
#' @examples
#' org_opts <- options(table_counter=1)
#' tblNoNext()
#' options(org_opts)
#' @family table functions
tblNoNext <- function(roman = getOption("table_counter_roman",
                                        FALSE)){
  last_no <- getOption("table_counter")
  if (is.logical(last_no)){
    if (last_no == FALSE)
      stop("You cannot call the get last figure number",
           " when you have explicitly set the fig_cap_no",
           " option to false.")
    last_no <- 0

  }else if (is.null(last_no)){
    last_no <- 0
  }

  next_no <- last_no + 1

  if (roman)
    next_no <- as.character(as.roman(next_no))

  return(next_no)
}
