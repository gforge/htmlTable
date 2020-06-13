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
                        compatibility,
                        total_columns,
                        css.cgroup,
                        top_row_style,
                        rnames,
                        rowlabel,
                        cgroup_spacer_cells,
                        prepped_cell_css,
                        style_list,
                        cell_style) {
  first_row <- TRUE
  # Start the head
  head_str <- "\n\t<thead>"

  if (!missing(caption) &
      compatibility == "LibreOffice" &
      !style_list$pos.caption %in% c("bottom", "below")){

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
                                    cgroup_vec.just = style_list$align.cgroup[i, ],
                                    css_4_cgroup_vec = style_list$css.cgroup[i,],
                                    row_no = i,
                                    top_row_style = top_row_style,
                                    rnames = rnames,
                                    rowlabel = rowlabel,
                                    style_list = style_list,
                                    cgroup_spacer_cells = cgroup_spacer_cells,
                                    prepped_cell_css = prepped_cell_css)
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
    if (!missing(rowlabel) && style_list$pos.rowlabel == no_cgroup_rows + 1){
      head_str %<>% sprintf("%s\n\t\t<th style='%s'>%s</th>",
                            .,
                            prGetStyle(c(`font-weight` = 900,
                                         `border-bottom` = "1px solid grey"),
                                       ts,
                                       attr(prepped_cell_css, "rnames")[1],
                                       align=prGetAlign(style_list$align.header, 1)),
                            rowlabel)
    }else if(!prSkipRownames(rnames)){
      head_str %<>% sprintf("%s\n\t\t<th style='%s'> </th>",
                            .,
                            prGetStyle(c(`border-bottom` = "1px solid grey"),
                                       ts))
    }

    cell_style <- "border-bottom: 1px solid grey;"
    if (first_row){
      cell_style %<>% c(top_row_style)
    }

    cell_str <- prAddCells(rowcells = header,
                           cellcode = "th",
                           style_list = style_list,
                           style=cell_style,
                           cgroup_spacer_cells = cgroup_spacer_cells,
                           has_rn_col = !prSkipRownames(rnames)*1,
                           prepped_cell_css = attr(prepped_cell_css, "header"))
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
