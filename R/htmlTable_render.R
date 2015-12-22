#' Renders the table head (thead)
#'
#' @inheritParams htmlTable
#' @inheritParams prGetCgroupHeader
#' @param total_columns The total number of columns including the rowlabel and the
#'  specer cells
#' @return \code{string} Returns the html string for the \code{<thead>...</thead>} element
#' @keywords internal
prGetThead <- function (x,
                        header,
                        cgroup, n.cgroup,
                        caption,
                        pos.caption,
                        compatibility,
                        total_columns,
                        align.cgroup,
                        css.cgroup,
                        top_row_style,
                        rnames,
                        rowlabel,
                        pos.rowlabel,
                        cgroup_spacer_cells,
                        css.cell,
                        align.header,
                        cell_style) {
  first_row <- TRUE
  # Start the head
  head_str <- "\n\t<thead>"

  if (!missing(caption) &
      compatibility == "LibreOffice" &
      !pos.caption %in% c("bottom", "below")){

    head_str %<>%
      sprintf("%s\n\t<tr><td colspan='%d' style='text-align: left;'>%s</td></tr>",
              .,
              total_columns,
              caption)
  }

  # Add the cgroup table header
  if (!missing(cgroup)){

    for (i in 1:nrow(cgroup)){
      cgrp_str <- prGetCgroupHeader(x = x,
                                    cgroup_vec = cgroup[i,],
                                    n.cgroup_vec = n.cgroup[i,],
                                    cgroup_vec.just = align.cgroup[i, ],
                                    css.cgroup_vec = css.cgroup[i,],
                                    row_no = i,
                                    top_row_style = top_row_style,
                                    rnames = rnames,
                                    rowlabel = rowlabel,
                                    pos.rowlabel = pos.rowlabel,
                                    cgroup_spacer_cells = cgroup_spacer_cells,
                                    css.cell = css.cell)
      head_str %<>%
        paste0(cgrp_str)
    }
    first_row <- FALSE
  }


  # Add the header
  if (!missing(header)){
    # The bottom border was ment to be here but it doesn't
    # work that well in the export
    head_str %<>%
      paste0("\n\t<tr>")

    no_cgroup_rows <-
      ifelse(!missing(cgroup),
             nrow(cgroup),
             0)
    ts <- ifelse(no_cgroup_rows > 0, "", top_row_style)
    if (!missing(rowlabel) && pos.rowlabel == no_cgroup_rows + 1){
      head_str %<>% sprintf("%s\n\t\t<th style='%s'>%s</th>",
                            .,
                            prGetStyle(c(`font-weight` = 900,
                                         `border-bottom` = "1px solid grey"),
                                       ts,
                                       attr(css.cell, "rnames")[1],
                                       align=prGetAlign(align.header, 1)),
                            rowlabel)
    }else if(!prSkipRownames(rnames)){
      head_str %<>% sprintf("%s\n\t\t<th style='%s'> </th>",
                            .,
                            prGetStyle(c(`border-bottom`="1px solid grey"),
                                       ts))
    }

    cell_style <- "border-bottom: 1px solid grey;"
    if (first_row){
      cell_style %<>%
        c(top_row_style)
    }

    cell_str <- prAddCells(rowcells = header,
                           cellcode = "th",
                           align = align.header,
                           style=cell_style,
                           cgroup_spacer_cells = cgroup_spacer_cells,
                           has_rn_col = !prSkipRownames(rnames)*1,
                           css.cell = attr(css.cell, "header"))
    head_str %<>%
      paste0(cell_str, "\n\t</tr>")
    first_row <- FALSE
  }

  #################################
  # Close head and start the body #
  #################################
  head_str %<>%
    paste0("\n\t</thead>")
  return(head_str)
}

#' Gets the number of rgroup htmlLine
#'
#' @param total_columns The total number of columns including the rowlabel and the
#'  spacer cells
#' @param cspan The column span of the current rgroup
#' @param style The css style corresponding to the rgroup css style that includes
#'  the color specific for the rgroup, i.e. \code{col.rgroup}.
#' @param cgroup_spacer_cells The vector indicating the position of the cgroup
#'  spacer cells
#' @param css.row The css.cell information for this particular row.
#' @param padding.tspanner The tspanner padding
#' @param rgroup_iterator An integer indicating the rgroup
#' @inheritParams htmlTable
#' @keywords internal
prGetRgroupLine <- function(x,
                            total_columns,
                            rgroup,
                            rgroup_iterator,
                            cspan,
                            rnames,
                            align,
                            style,
                            cgroup_spacer_cells,
                            col.columns,
                            css.row,
                            padding.tspanner){
  ret_str <- ""
  rgroup_elmnt <- rgroup[rgroup_iterator]
  add_elmnt <- prAttr4RgroupAdd(rgroup = rgroup,
                                rgroup_iterator = rgroup_iterator,
                                no_cols = ncol(x))

  ## this will allow either css.rgroup or col.rgroup to
  ## color the rgroup label rows
  if (is.numeric(cspan) &&
      cspan < ncol(x) ||
      !is.null(add_elmnt)){

    filler_cells <- rep("", ncol(x))

    if (!is.null(add_elmnt)){
      if (!is.numeric(cspan))
        cspan <- ncol(x) + 1*!prSkipRownames(rnames)

      if (length(add_elmnt) > 1){
        if (is.null(names(add_elmnt)))
          stop("The rgroup 'add' attribute element no '", rgroup_iterator, "'",
               " either be a single element or a named list/vector")

        add_pos <- as.integer(names(add_elmnt))
        if (any(is.na(add_pos)) ||
            any(add_pos < 1) ||
            any(add_pos > ncol(x)))
          stop("You have provided invalid element position for rgroup = '", rgroup_elmnt, "'",
               " the attribute seeems to be a list but the names are invalid",
               " '", paste(names(add_elmnt), collapse="', '"), "'",
               " they should be integers between 1 and ", ncol(x))

        first_pos <- min(add_pos) - 1 + 1*!prSkipRownames(rnames)
        if (missing(cspan)){
          cspan <- first_pos
        }else{
          cspan <- min(cspan,
                       first_pos)
        }

        for (ii in 1:length(add_pos)){
          filler_cells[add_pos[ii]] <- add_elmnt[[ii]]
        }
      }else if(length(add_elmnt) == 1){
        if (is.null(names(add_elmnt)) ||
            names(add_elmnt) == "last"){
          add_pos <- ncol(x)
        }else{
          add_pos <- as.integer(names(add_elmnt))
          if (is.na(add_pos) ||
              add_pos < 1 ||
              add_pos > ncol(x))
            stop("You have provided invalid element position for rgroup = '", rgroup_elmnt, "'",
                 " the attribute seeems to be a list but the name is invalid",
                 " '", names(add_elmnt), "'",
                 " it should be an integer between 1 and ", ncol(x))
        }

        first_pos <- add_pos - 1 + 1*!prSkipRownames(rnames)
        if (missing(cspan)){
          cspan <- first_pos
        }else{
          cspan <- min(cspan,
                       first_pos)
        }

        filler_cells[add_pos] <- add_elmnt
      }else{
        stop("The attribute to the rgroup '", rgroup_elmnt, "'",
             " does not have a length!")
      }
    }

    true_span <- cspan +
      sum(cgroup_spacer_cells[0:(cspan-
                                   1*!prSkipRownames(rnames))])
    ret_str %<>%
      sprintf("%s\n\t<tr><td colspan='%d' style='%s'>%s</td>",
              .,
              true_span,
              prGetStyle(style),
              paste0(padding.tspanner,
                     rgroup_elmnt))


    cols_left <- ncol(x) - (cspan - 1*!prSkipRownames(rnames))
    cell_str <- prAddCells(rowcells = filler_cells,
                           cellcode = "td",
                           align = align,
                           style = style,
                           cgroup_spacer_cells = cgroup_spacer_cells,
                           has_rn_col = !prSkipRownames(rnames)*1,
                           col.columns = col.columns,
                           offset = ncol(x) - cols_left + 1,
                           css.cell = css.row)
    ret_str %<>%
      paste0(cell_str)


    ret_str %<>% paste0("</tr>")

  }else{
    ret_str %<>%
      sprintf("%s\n\t<tr><td colspan='%d' style='%s'>%s</td></tr>",
              .,
              total_columns,
              prGetStyle(style),
              paste0(padding.tspanner,
                     rgroup_elmnt))
  }

  return(ret_str)
}
