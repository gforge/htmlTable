#' Renders the table head (thead)
#'
#' @inheritParams htmlTable
#' @inheritParams prGetCgroupHeader
#' @param total_columns The total number of columns
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
