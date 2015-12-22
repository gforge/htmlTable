#' Gets the table counter string
#'
#' Returns the string used for htmlTable to number the different tables.
#' Uses options \code{table_counter}, \code{table_counter_str},
#' and \code{table_counter_roman} to produce the final string. You
#' can set each option by simply calling \code{options()}.
#'
#' @param The caption
#' @return \code{string} Returns a string formatted according to
#'  the table_counter_str and table_counter_roman. The number is
#'  decided by the table_counter variable
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prTblNo <- function (caption) {
  tc <- getOption("table_counter", FALSE)
  if (tc == FALSE){
    if (missing(caption))
      return("")
    else
      return(caption)
  }

  table_template <- getOption("table_counter_str", "Table %s: ")
  out <- sprintf(table_template,
                 ifelse(getOption("table_counter_roman", FALSE),
                        as.character(as.roman(tc)),
                        as.character(tc)))
  if (!missing(caption))
    out <- paste(out, caption)

  return(out)
}

#' Gets the CSS style element
#'
#' A funciton for checking, merging, and more
#' with a variety of different style formats.
#'
#' @param styles The styles can be provided as \code{vector},
#'  \code{named vector}, or \code{string}.
#' @param ... All styles here are merged with the first parameter.
#'  If you provide a name, e.g. \code{styles="background: blue", align="center"}
#'  the function will convert the \code{align} into proper \code{align: center}.
#' @return \code{string} Returns the codes merged into one string with
#'  correct CSS ; and : structure.
#' @keywords internal
#' @import magrittr
#' @family hidden helper functions for \code{\link{htmlTable}}
prGetStyle <- function(...){
  mergeNames <- function(sv){
    if (!is.null(names(sv))){
      sv <-
        mapply(function(n, v){
          if (n == "")
            return(v)
          paste0(n, ": ", v)
        }, n=names(sv), v=sv, USE.NAMES=FALSE)
    }
    return(sv)
  }
  spltNames <- function(sv){
    ret_sv <- c()
    for (i in 1:length(sv))
      ret_sv <- c(ret_sv,
                  # Split on the ; in case it is not at the end/start
                  unlist(strsplit(sv[i], "\\b;(\\b|\\W+)", perl=TRUE)))
    return(ret_sv)
  }

  styles <- c()
  dots <- list(...)
  if (length(dots) == 0)
    return("")

  for (i in 1:length(dots)){
    element <- dots[[i]]
    if (length(element) == 1){
      if (element == "")
        next

      if (!grepl("\\b[:](\\b|\\W+)", element, perl=TRUE)){
        if(!is.null(names(element))){
          element <-
            paste0(names(element), ": ", element)
        }else if(!is.null(names(dots)) &&
                   names(dots)[i] != ""){
          element <-
            paste0(names(dots)[i], ": ", element)
        }else if(element != "none") {
          stop("The style should be formatted according to 'style_name: value'",
               " you have provided style '", element,"'")
        }
      }
      styles %<>%
        c(element)
    }else{
      if (!is.null(names(element))){
        element <- mergeNames(element)
      }

      styles <- c(styles,
                  spltNames(element))
    }
  }

  if (!all(grepl("^[^:]+:.+", styles)))
    stop("Invalid styles detected, one or more styles lack the needed style 'name: value': ",
         paste(paste0("'", styles[!grepl("^[^:]+:.+", styles)], "'"), collapse=", "))

  # Remove empty background colors
  if (any(grepl("^background-color: none", styles))){
    styles <- styles[-grep("^background-color: none", styles)]
  }

  # Merge background colors
  if (sum(grepl("^background-color:", styles)) > 1){
    clrs <- styles[grep("^background-color:", styles)]
    clrs <- gsub("^background-color:[ ]*([^;]+);*", "\\1", clrs)
    clr <- prMergeClr(clrs)
    # Pick a color merge
    styles <- styles[-grep("^background-color:", styles)]
    styles <-
      c(styles,
        paste0("background-color: ", clr))
  }

  style_names <- gsub("^([^:]+).+", "\\1", styles)
  if (!any(duplicated(style_names))){
    unique_styles <- styles
  }else{
    # Only select the last style if two of the same type
    # exist. This in order to avoid any conflicts.
    unique_styles <- c()
    for(n in unique(style_names)){
      unique_styles <-
        c(unique_styles,
          styles[max(which(n == style_names))])
    }
  }

  unique_styles <- sapply(unique_styles, prAddSemicolon2StrEnd, USE.NAMES = FALSE)
  paste(unique_styles, collapse=" ")
}

#' Add a ; at the end
#'
#' The CSS expects a semicolon at the end of each argument
#' this function just adds a semicolong if none is given
#' and remove multiple semicolon if such exist
#'
#' @param my_str The string that is to be processed
#' @return \code{string}
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prAddSemicolon2StrEnd <- function(my_str){
  if (!is.null(names(my_str))){
    tmp <- str_trim(my_str)
    names(tmp) <- names(my_str)
    my_str <- tmp
  }else{
    my_str <- str_trim(my_str)
  }
  my_str_n <- sapply(my_str, nchar, USE.NAMES = FALSE)
  if (any(my_str_n == 0))
    my_str <- my_str[my_str_n > 0]

  if(length(my_str) == 0)
    return("")

  if (tail(strsplit(my_str, "")[[1]], 1) != ";"){
    n <- names(my_str)
    my_str <- sprintf("%s;", my_str)
    if (!is.null(n))
      names(my_str) <- n
  }

  # Remove duplicated ;
  my_str <- gsub(";;+", ";", my_str)
  empty_str <- sapply(my_str, function(x) x == ";", USE.NAMES = FALSE)
  if (any(empty_str))
    my_str <- my_str[!empty_str]

  if(length(my_str) == 0)
    return("")

  return (my_str)
}

#' Retrieve a header row
#'
#' This function retrieves a header row, i.e. a row
#' within the <th> elements on top of the table. Used by
#' \code{\link{htmlTable}}.
#'
#' @param cgroup_vec The cgroup may be a matrix, this is
#'  just one row of that matrix
#' @param n.cgroup_vec The same as above but for the counter
#' @param cgroup_vec.just The same as above bot for the justificaiton
#' @param row_no The row number within the header group. Useful for multirow
#'  headers when we need to output the rowlabel at the \code{pos.rowlabel}
#'  level.
#' @param css.cgroup_vec The CSS row corresponding to the current row
#' @param top_row_style The top row has a special style depending on
#'  the \code{ctable} option in the \code{htmlTable} call.
#' @param cgroup_spacer_cells The spacer cells due to the multiple cgroup levels.
#'  With multiple rows in cgroup we need to keep track of how many spacer cells
#'  occur between the columns. This variable contains is of the size \code{ncol(x)-1}
#'  and 0 if there is no cgroup element between.
#' @return \code{string}
#' @keywords internal
#' @inheritParams htmlTable
#' @family hidden helper functions for \code{\link{htmlTable}}
prGetCgroupHeader <- function(x,
                              cgroup_vec,
                              n.cgroup_vec,
                              cgroup_vec.just,
                              css.cgroup_vec,
                              row_no, top_row_style,
                              rnames,
                              rowlabel, pos.rowlabel,
                              cgroup_spacer_cells,
                              css.cell){

  header_str <- "\n\t<tr>"
  if (row_no == 1)
    ts <- top_row_style
  else
    ts <- ""

  if (!missing(rowlabel)){
    if (row_no == pos.rowlabel)
      header_str %<>% sprintf("%s\n\t\t<th style='%s'>%s</th>",
                              .,
                              prGetStyle(c(`font-weight`=900),
                                           ts,
                                           attr(css.cell, "rnames")[1]),
                              rowlabel)
    else
      header_str %<>%
      sprintf("%s\n\t\t<th style='%s'></th>",
              .,
              prGetStyle(ts))
  }else if (!prSkipRownames(rnames)){
    header_str %<>% sprintf("%s\n\t\t<th style='%s'></th>",
                            .,
                            prGetStyle(ts))
  }

  for (i in 1:length(cgroup_vec)){
    if (!is.na(n.cgroup_vec[i])){
      start_column <- ifelse(i == 1,
                             1,
                             sum(n.cgroup_vec[1:(i-1)], na.rm=TRUE) + 1)

      # 10 3-1
      # 0 0 1
      colspan <- n.cgroup_vec[i] +
        ifelse(start_column > length(cgroup_spacer_cells) ||
                 n.cgroup_vec[i] == 1,
               0,
               ifelse(start_column == 1,
                      sum(cgroup_spacer_cells[1:(n.cgroup_vec[i]-1)]),
                      ifelse(sum(n.cgroup_vec[1:i], na.rm=TRUE) == ncol(x),
                             sum(cgroup_spacer_cells[start_column:length(cgroup_spacer_cells)]),
                             sum(cgroup_spacer_cells[start_column:((start_column-1) + (n.cgroup_vec[i]-1))]))))

      if (nchar(cgroup_vec[i]) == 0)# Removed as this may now be on purpose || is.na(cgroup_vec[i]))
        header_str %<>% sprintf("%s\n\t\t<th colspan='%d' style='%s'></th>",
                                .,
                                colspan,
                                prGetStyle(c(`font-weight`=900),
                                           ts,
                                           align=prGetAlign(cgroup_vec.just, i),
                                           css.cgroup_vec[i]))
      else
        header_str %<>% sprintf("%s\n\t\t<th colspan='%d' style='%s'>%s</th>",
                                .,
                                colspan,
                                prGetStyle(c(`font-weight`=900,
                                             `border-bottom`="1px solid grey"),
                                           ts,
                                           align=prGetAlign(cgroup_vec.just, i),
                                           css.cgroup_vec[i]),
                                cgroup_vec[i])

      # If not last then add a filler cell between the row categories
      # this is also the reason that we need the cgroup_spacer_cells
      if (i != sum(!is.na(cgroup_vec)))
        header_str %<>% sprintf("%s<th style='%s; border-bottom: hidden;'>&nbsp;</th>",
                                ., ts)
    }
  }
  header_str %<>%
    paste0("\n\t</tr>")

  return(header_str)
}

#' Prepares the cgroup argument
#'
#' Due to the complicated structure of multilevel cgroups there
#' some preparation for the cgroup options is required.
#'
#' @inheritParams htmlTable
#' @return \code{list(cgroup, n.cgroup, align.cgroup, cgroup_spacer_cells)}
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prPrepareCgroup <- function(x, cgroup, n.cgroup, align.cgroup, css.cgroup){
  cgroup_spacer_cells <- rep(0, times=(ncol(x)-1))

  # The cgroup is by for compatibility reasons handled as a matrix
  if (!is.matrix(cgroup)){

    cgroup <- matrix(cgroup, nrow=1)
    if (missing(n.cgroup))
      n.cgroup <- matrix(NA, nrow=1)
    else{
      if (any(n.cgroup < 1)){
        warning("You have provided cgroups with less than 1 element,",
                " these will therefore be removed: ",
                paste(sprintf("'%s' = %d", cgroup, n.cgroup)[n.cgroup < 1],
                      collapse=", "))
        cgroup <- cgroup[,n.cgroup >= 1, drop=FALSE]
        n.cgroup <- n.cgroup[n.cgroup >= 1]
      }

      if (ncol(cgroup) != length(n.cgroup)){
        n.cgroup <- n.cgroup[n.cgroup > 0]
        if (ncol(cgroup) != length(n.cgroup))
          stop("You have provided invalid n.cgroup,",
               " it should have the same length as the cgroup (", ncol(cgroup), ")",
               " but it has the length of ", length(n.cgroup))
      }
      n.cgroup <- matrix(n.cgroup, nrow=1)
    }
  }else if(missing(n.cgroup)){
    stop("If you specify the cgroup argument as a matrix you have to",
         " at the same time specify the n.cgroup argument.")
  }

  if (missing(align.cgroup)){
    align.cgroup <- apply(n.cgroup, 1,
                         function(nc) paste(rep("c", times=sum(!is.na(nc))), collapse=""))
    align.cgroup <- matrix(align.cgroup,
                          ncol = 1)
  }else{
    if (NROW(align.cgroup) != nrow(n.cgroup))
      stop("You have different dimensions for your align.cgroup and your cgroups, ",
           NROW(align.cgroup), " (just) !=", nrow(n.cgroup), " (n.cgroup)")

    # An old leftover behaviour from the latex() function
    if (NCOL(align.cgroup) > 1)
      align.cgroup <- apply(align.cgroup, 1, function(x) paste(ifelse(is.na(x), "", x), collapse=""))

    align.cgroup <- mapply(prPrepareAlign,
                          align = align.cgroup,
                          x = apply(n.cgroup, 1, function(nc) sum(!is.na(nc))),
                          rnames=FALSE)

    align.cgroup <- matrix(align.cgroup, ncol=1)
  }

  # Go bottom up as the n.cgroup can be based on the previous
  # n.cgroup row.
  for (i in nrow(cgroup):1){
    # The row is empty and filled with NA's then we check
    # that it is possible to evenly split the cgroups among
    # the columns of the table
    if (all(is.na(n.cgroup[i,])) &&
          ncol(x) %% length(cgroup[i,]) == 0){
      # This generates the n.cgroup if this is missing
      n.cgroup[i,] <- rep(ncol(x)/length(cgroup[i,]), times=length(cgroup[i,]))
    }else if(any(n.cgroup[i,!is.na(n.cgroup[i,])] < 1)){
      stop("You have in n.cgroup row no ", i, " cell(s) with < 1")

    }else if(sum(n.cgroup[i,], na.rm=TRUE) != ncol(x)){
      ncgroupFixFromBelowGroup <- function(nc, i){
        if (i+1 > nrow(nc))
          stop("You have provided an invalid nc",
               " where it has fewer rows than the one of interest")

        # Select those below that are not missing
        row_below <- nc[i + 1, !is.na(nc[i + 1, ])]
        # The first position to start
        start_pos <- 1
        # This is a slightly complicated run that took a while to figure out
        # and I'm still afraid of ever having to debug this section.
        for (ii in 1:ncol(nc)){
          if (!is.na(nc[i, ii])){
            # Need to find where to begin tha addition
            pos <- ifelse(any(start_pos > cumsum(row_below)),
                          tail(which(start_pos > cumsum(row_below)), 1) + 1,
                          1)
            # Change the value to the rows below values that add up to this row
            # if the nc value is 1 and start position is 1 -> 1:(1+1-1) -> 1:1 -> 1
            # if the nc value is 2 and start position is 2 -> 2:(2+2-1) -> 2:3
            # if the nc value is 2 and start position is 1 -> 1:(1+2-1) -> 1:2
            nc[i, ii] <- sum(row_below[pos:(pos + nc[i, ii] - 1)])
            # Update the new start position:
            # if first run and nc is 2 then 1 + 2 -> 3 i.e.
            # next run the start_pos is 3 and lets say that nc is 3 then 3 + 3 -> 6
            start_pos <- start_pos + nc[i, ii]
          }
        }

        # Return the full object
        return(nc)
      }

      # This grouping can be based upon the next row
      if (i < nrow(cgroup) &&
            sum(n.cgroup[i, ], na.rm = TRUE) == sum(!is.na(n.cgroup[i + 1, ])))
      {
        n.cgroup <- ncgroupFixFromBelowGroup(n.cgroup, i)
      }else{
        stop(sprintf("Your columns don't match in the n.cgroup for the %d header row, i.e. %d != %d",
                     i,
                     sum(n.cgroup[i,], na.rm=TRUE),
                     ncol(x)))
      }
    }

    if (!all(is.na(n.cgroup[i, ]) == is.na(cgroup[i, ]))){
      stop("On header row (the cgroup argument) no ", i,
           " you fail to get the NA's matching.",
           "\n  The n.cgroup has elements no:",
           sprintf(" '%s'", paste(which(is.na(n.cgroup[i, ])), collapse=", ")),
           " missing while cgroup has elements no:",
           sprintf(" '%s'", paste(which(is.na(cgroup[i, ])), collapse=", ")),
           " missing.",
           "\n If the NA's don't occur at the same point",
           " the software can't decide what belongs where.",
           "\n The full cgroup row: ", paste(cgroup[i, ], collapse=", "),
           "\n The full n.cgroup row: ", paste(n.cgroup[i, ], collapse=", "),
           "\n Example: for a two row cgroup it would be:",
           " n.cgroup = rbind(c(1, NA), c(2, 1)) and",
           " cgroup = rbind(c('a', NA), c('b', 'c'))")
    }

    # Add a spacer cell for each cgroup. If two cgroups
    # on different rows have the same separation then it
    # is enough to have one spacer.
    for (ii in 1:(length(n.cgroup[i, ])-1)){
      if (!is.na(n.cgroup[i, ii]) && sum(n.cgroup[i, 1:ii], na.rm=TRUE) <= length(cgroup_spacer_cells))
        cgroup_spacer_cells[sum(n.cgroup[i, 1:ii], na.rm=TRUE)] <- 1
    }
  }

  css.cgroup <- prPrepareCss(x = cgroup, css = css.cgroup)
  return(list(cgroup = cgroup,
              n.cgroup = n.cgroup,
              align.cgroup = align.cgroup,
              cgroup_spacer_cells = cgroup_spacer_cells,
              css.cgroup = css.cgroup))
}

#' Gets the rowlabel position
#'
#' @inheritParams htmlTable
#' @return \code{integer} Returns the position within the header rows
#'  to print the \code{rowlabel} argument
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prGetRowlabelPos <- function (cgroup, pos.rowlabel, header) {
  no_cgroup_rows <-
    ifelse(!missing(cgroup),
           nrow(cgroup),
           0)
  no_header_rows <-
    no_cgroup_rows +
    (!missing(header))*1
  if (is.numeric(pos.rowlabel)){
    if(pos.rowlabel < 1)
      stop("You have specified a pos.rowlabel that is less than 1: ", pos.rowlabel)
    else if (pos.rowlabel > no_header_rows)
      stop("You have specified a pos.rowlabel that more than the max limit, ",
           no_header_rows,
           ", you have provided: ", pos.rowlabel)
  }else{
    pos.rowlabel <- tolower(pos.rowlabel)
    if (pos.rowlabel %in% c("top"))
      pos.rowlabel <- 1
    else if (pos.rowlabel %in% c("bottom", "header"))
      pos.rowlabel <- no_header_rows
    else
      stop("You have provided an invalid pos.rowlabel text,",
           " only 'top', 'bottom' or 'header' are allowed,",
           " can't interpret '", pos.rowlabel, "'")
  }

  return(pos.rowlabel)
}

#' Add a cell
#'
#' Adds a row of cells <td>val</td><td>...</td> to a table string for
#' \code{\link{htmlTable}}
#'
#' @inheritParams htmlTable
#' @param rowcells The cells with the values that are to be added
#' @param cellcode Type of cell, can either be \code{th} or \code{td}
#' @param style The cell style
#' @param cgroup_spacer_cells The number of cells that occur between
#'  columns due to the cgroup arguments.
#' @param has_rn_col Due to the alignment issue we need to keep track
#'  of if there has already been printed a rowname column or not and therefore
#'  we have this has_rn_col that is either 0 or 1.
#' @param offset For rgroup rows there may be an offset != 1
#' @param css.cell The css.cell but only for this row compared to the htmlTable matrix
#' @return \code{string} Returns the string with the new cell elements
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prAddCells <- function(rowcells, cellcode, align, style, cgroup_spacer_cells, has_rn_col, col.columns,
                         offset = 1, css.cell){
  cell_str <- ""
  style = prAddSemicolon2StrEnd(style)

  for (nr in offset:length(rowcells)){
    cell_value <- rowcells[nr]
    # We don't want missing to be NA in a table, it should be empty
    if (is.na(cell_value))
      cell_value <- ""

    cell_style <- c(css.cell[nr],
                    style,
                    prGetAlign(align, nr + has_rn_col))
    if (!missing(col.columns)){
      cell_style %<>%
        c(`background-color` = col.columns[nr])
    }

    cell_str %<>%
      sprintf("%s\n\t\t<%s style='%s'>%s</%s>",
              .,
              cellcode,
              prGetStyle(cell_style),
              cell_value,
              cellcode)

    # Add empty cell if not last column
    if (nr != length(rowcells) &&
          nr <= length(cgroup_spacer_cells) &&
          cgroup_spacer_cells[nr] > 0){
      spanner_style <- style
      if (!missing(col.columns)){
        if (col.columns[nr] == col.columns[nr + 1]){
          spanner_style %<>%
            c(`background-color` = col.columns[nr])
        }
      }
      cell_str %<>%
        sprintf("%s\n\t\t<%s style='%s' colspan='%d'>&nbsp;</%s>",
                .,
                cellcode,
                prGetStyle(spanner_style),
                cgroup_spacer_cells[nr],
                cellcode)
    }
  }
  return (cell_str)
}

#' Gets alignment
#'
#' @param index The index of the align parameter of interest
#' @family hidden helper functions for
#' @keywords internal
#' @inheritParams htmlTable
prGetAlign <- function(align, index) {
  segm_rgx <- "[^lrc]*[rlc][^lrc]*"

  res_align <- align
  align <- ""
  # Loop to remove every element prior to the one of interest
  for (i in 1:index){
    if (nchar(res_align) == 0)
      stop("Requested column outside of span, ", index, " > ", i)

    rmatch <- regexpr(segm_rgx, res_align)
    lrc_data <- substr(res_align, 1, rmatch + attr(rmatch, "match.length") - 1)
    res_align <- substring(res_align, rmatch + attr(rmatch, "match.length"))
  }
  styles <- c()
  if (grepl("^[|]", lrc_data))
    styles["border-left"] = "1px solid black"
  if (grepl("[|]$", lrc_data))
    styles["border-right"] = "1px solid black"

  if (grepl("l", lrc_data))
    styles["text-align"] = "left"
  if (grepl("c", lrc_data))
    styles["text-align"] = "center"
  if (grepl("r", lrc_data))
    styles["text-align"] = "right"

  return(styles)
}

#' Prepares the align to match the columns
#'
#' The alignment may be tricky and this function therefore simplifies
#' this process by extending/shortening the alignment to match the
#' correct number of columns.
#'
#' @param default_rn The default rowname alignment. This is an option
#'  as the header uses the same function and there may be differences in
#'  how the alignments should be implemented.
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
#' @inheritParams htmlTable
prPrepareAlign <- function (align, x, rnames, default_rn = "l") {
  if (length(align) > 1)
    align <- paste(align, collapse="")

  segm_rgx <- "[^lrc]*[rlc][^lrc]*"
  no_elements <- length(strsplit(align, split = segm_rgx)[[1]])
  no_cols <- ifelse(is.null(dim(x)), x, ncol(x))
  if (!prSkipRownames(rnames)){
    no_cols <- no_cols + 1
    if (no_elements < no_cols){
      align <- paste0(default_rn, align)
    }
  }

  res_align <- align
  align <- ""
  for (i in 1:no_cols){
    rmatch <- regexpr(segm_rgx, res_align)
    tmp_lrc <- substr(res_align, 1, rmatch + attr(rmatch, "match.length") - 1)
    res_align <- substring(res_align, rmatch + attr(rmatch, "match.length"))
    align <- paste0(align,
                    tmp_lrc)
    if (nchar(res_align) < 1 &&
          i != no_cols){
      align <- paste0(align,
                      paste(rep(tmp_lrc, times=no_cols - i), collapse=""))
      break;
    }
  }

  structure(align,
            n = no_cols,
            class = class(align))
}

#' Returns if rownames should be printed for the htmlTable
#'
#' @inheritParams htmlTable
#' @keywords internal
prSkipRownames <- function(rnames){
  if(missing(rnames))
    return(TRUE)

  if (length(rnames) == 1 &&
        rnames == FALSE)
    return(TRUE)

  return(FALSE)
}

#' Prepares the alternating colors
#'
#' @param clr The colors
#' @param n The number of rows/columns applicable to the color
#' @param ng The n.rgroup/n.cgroup argument if applicable
#' @param gtxt The rgroup/cgroup texts
#' @return \code{character} A vector containing hexadecimal colors
#' @import magrittr
#' @keywords internal
prPrepareColors <- function(clr, n, ng, gtxt){
  clr <- sapply(clr, function(a_clr){
    if(a_clr == "none")
      return(a_clr)
    if (grepl("^#[0-9ABCDEFabcdef]{3,3}$", a_clr)){
      a_clr %<>%
        substring(first = 2) %>%
        strsplit(split = "") %>%
        unlist %>%
        sapply(FUN = rep, times=2) %>%
        paste(collapse="") %>%
        tolower %>%
        paste0("#", .)
    }else{
      a_clr %<>%
        col2rgb %>%
        as.hexmode %>%
        as.character %>%
        paste(collapse="") %>%
        paste0("#", .)
    }
  }, USE.NAMES=FALSE)

  if(!missing(ng)){
    # Split groups into separate if the gtxt is ""
    if (any(gtxt == "")){
      tmp <- c()
      for (i in 1:length(ng)){
        if (gtxt[i] != "" &&
              !is.na(gtxt[i])){
          tmp <- c(tmp,
                   ng[i])
        }else{
          tmp <- c(tmp,
                   rep(1, ng[i]))
        }
      }
      ng <- tmp
    }

    clr <- rep(clr, length.out = length(ng))
    attr(clr, "groups") <-
      Map(rep, clr, length.out = ng)
  }else if(!missing(n)){
    clr <- rep(clr, length.out = n)
  }

  return(clr)
}


#' Merges multiple colors
#'
#' Uses the \code{\link[grDevices]{colorRampPalette}} for merging colors.
#' \emph{Note:} When merging more than 2 colors the order in the color
#' presentation matters. Each color is merged with its neigbors before
#' merging with next. If there is an uneven number of colors the middle
#' color is mixed with both left and right side.
#'
#' @param clrs The colors
#' @return \code{character} A hexadecimal color
#' @import magrittr
#' @keywords internal
prMergeClr<- function(clrs){
  if (length(clrs) == 1)
    return(clrs)
  if (length(clrs) == 2)
    return(colorRampPalette(clrs)(3)[2])

  split_lngth <- floor(length(clrs)/2)
  left <- head(clrs, split_lngth)
  right <- tail(clrs, split_lngth)
  if (length(clrs) %% 2 == 1){
    left %<>%
      c(clrs[split_lngth + 1])
    right %<>%
      c(clrs[split_lngth + 1], .)
  }

  left <- prMergeClr(left)
  right <- prMergeClr(right)
  return(prMergeClr(c(left,
                        right)))
}

#' Prepares the cell style
#'
#' @param css The CSS styles that are to be converted into
#'  a matrix.
#' @param name The name of the CSS style that is prepared
#' @inheritParams htmlTable
#' @return \code{matrix}
#' @keywords internal
prPrepareCss <- function(x, css, rnames, header, name = deparse(substitute(css))){
  css.header <- rep("", times = ncol(x))
  css.rnames <- rep("", times = nrow(x) + !missing(header))
  if (is.data.frame(css))
    css <- as.matrix(css)

  if (is.matrix(css)){
    if (any(grepl("^[^:]*[a-zA-Z]+[:]*:", css))){
      rownames(css) <- NULL
      colnames(css) <- NULL
    }
    if (ncol(css) == ncol(x) + 1 &&
          !prSkipRownames(rnames)){
      if (!missing(header)){
        if (nrow(css) == nrow(x) + 1){
          css.rnames <- css[,1]
        }else if(nrow(css) == nrow(x)){
          css.rnames[2:length(css.rnames)] <- css[,1]
        }else{
          stop("There is an invalid number of rows for the ", name ," matrix.",
               " Your x argument has '", nrow(x), "' rows",
               " while your ", name ," has '", nrow(css), "' rows",
               " and there is a header")
        }
      }else if(nrow(x) == nrow(css)){
        css.rnames <- css[,1]
      }else{
        stop("There is an invalid number of rows for the ", name ," matrix.",
             " Your x argument has '", nrow(x), "' rows",
             " while your ", name ," has '", nrow(css), "' rows",
             " (there is no header)")
      }

      css <-
        css[,-1]
    }else if (ncol(css) != ncol(x)){
      stop("There is an invalid number of columns for the ", name ," matrix.",
           " Your x argument has '", ncol(x), "' columns",
           " while your ", name ," has '", ncol(css), "' columns",
           " and there are ", ifelse(prSkipRownames(rnames),
                                     "no", ""),
           " rownames.")
    }

    if (nrow(css) == nrow(x) + 1 &&
          !missing(header)){
      css.header <- css[1,]
      css <- css[-1,]
    }else if(nrow(css) != nrow(x)){
      stop("There is an invalid number of rows for the ", name ," matrix.",
           " Your x argument has '", nrow(x), "' rows",
           " while your ", name ," has '", nrow(css), "' rows",
           " and there is ", ifelse(missing(header),
                                    "no", "a"),
           " header")
    }
  }else if(is.vector(css)){
    if (length(css) == ncol(x) + 1){
      css.rnames = rep(css[1], nrow(x) + prSkipRownames(rnames))
      css <-
        css[-1]
    }else if(length(css) != ncol(x) &&
               length(css) != 1){
      stop("The length of your ", name ," vector '", length(css) ,"'",
           " does not correspond to the column length '", ncol(x) ,"'",
           " (there are ", ifelse(prSkipRownames(rnames),
                                    "no", ""),
           " rownames)")
    }

    css <- matrix(css,
                  nrow=nrow(x),
                  ncol=ncol(x),
                  byrow = TRUE)
  }

  return(structure(css,
                   rnames = css.rnames,
                   header = css.header,
                   class=class(css)))
}


#' Get the add attribute element
#'
#' Gets the add element attribute if it exists. If non-existant it will
#' return NULL.
#'
#' @param rgroup_iterator The rgroup number of interest
#' @param no_cols The \code{ncol(x)} of the core htmlTable x argument
#' @inheritParams htmlTable
#' @keywords internal
prAttr4RgroupAdd <- function (rgroup, rgroup_iterator, no_cols) {
  if (is.null(attr(rgroup, "add")))
    return(NULL)

  add_elmnt <- attr(rgroup, "add")
  if (is.null(names(add_elmnt))){
    if (is.null(dim(add_elmnt)) &&
        length(add_elmnt) == sum(rgroup !=  "")){
      if (!is.list(add_elmnt))
        add_elmnt <- as.list(add_elmnt)
      names(add_elmnt) <- (1:length(rgroup))[rgroup !=  ""]
    }else if(!is.null(dim(add_elmnt)) &&
             ncol(add_elmnt) %in% c(1, no_cols)){

      # Convert matrix to stricter format
      tmp <- list()
      for (i in 1:nrow(add_elmnt)){
        if (ncol(add_elmnt) == 1){
          tmp[[i]] <- add_elmnt[i,]
        }else{
          tmp2 <- as.list(add_elmnt[i,])
          names(tmp2) <- 1:no_cols
          tmp[[i]] <- tmp2
        }
      }
      if (nrow(add_elmnt) == sum(rgroup !=  "")){
        names(tmp) <- (1:length(rgroup))[rgroup != ""]
      } else if (!is.null(rownames(add_elmnt))){
        names(tmp) <- rownames(add_elmnt)
      } else {
        stop("You have provided a matrix as the
             add attribute to rgroups without rows that either
             match the number of rgroups available '", length(rgroup[rgroup != ""]), "'",
             " (you provided '", nrow(add_elmnt), "' rows).",
             " And you also failed to have rownames.")
      }
      add_elmnt <- tmp

    }else{
      stop("The length of the rgroup 'add' attribute must either match",
           " (1) the length of the rgroup",
           " (2) or have names corresponding to the mapping integers")
    }

  }

  if (!is.list(add_elmnt) &&
        !is.vector(add_elmnt))
    stop("The rgroup mus either be a list or a vector")

  add_pos <- ifelse(grepl("^[123456789][0-9]*$",
                          names(add_elmnt)),
                    as.integer(names(add_elmnt)),
                    NA)
  if (any(is.na(add_pos))){
    # Look for rgroup names that match to those not
    # found through the integer match
    # If found the number is assigned to the add_pos
    available_rgroups <- rgroup[rgroup != ""]
    if (!all(is.na(add_pos)))
      available_rgroups <- available_rgroups[-na.omit(add_pos)]
    matches <-
      sapply(available_rgroups,
             function(row_label){
               match <- which(row_label == names(add_elmnt)[is.na(add_pos)])
               if (length(match) > 1)
                 stop("Invalid add argument name '", row_label, "'",
                      " it matches several rgroups.",
                      " Functionality with same rgroup names has not yet been added.",
                      " Try to use integers instead that match the row number.")
               if (length(match) == 1)
                 return(match)
               return(0)
             })
    if (is.vector(matches) &&
        sum(matches != 0) == sum(is.na(add_pos))){
      for (i in 1:sum(is.na(add_pos))){
        row_label <- names(add_elmnt)[i]
        add_pos <- matches[row_label]
      }
    }else{
      stop("The rgroup 'add' element contains invalid names: ",
           "'", paste(names(add_elmnt)[is.na(add_pos)], collapse="', '"), "'",
           " that were neither valid integers or occurred among the rgroup names: ",
           "'", paste(rgroup[rgroup != ""], collapse="', '"), "'")
    }
    names(add_elmnt) <- add_pos
  }

  if (!is.list(add_elmnt))
    add_elmnt <- as.list(add_elmnt)

  if (any(add_pos < 1))
    stop("The rgroup 'add' attribute cannot have integer names below 1")

  if (any(!add_pos < length(rgroup)) ||
      any(rgroup[add_pos] == ""))
    stop("The rgroup 'add' attribute cannot have integer names indicating",
         " positions larger than the length of the rgroup",
         " ('", length(rgroup), "' or matches one of the empty groups).",
         " The problematic position(s):",
         " '", paste(add_pos[add_pos > length(rgroup)], collapse="', '") ,"'")

  # Return the matching iterator
  if (rgroup_iterator %in% names(add_elmnt)){
    return(add_elmnt[[as.character(rgroup_iterator)]])
  }

  return(NULL)
}
