#' Renders the table head (thead)
#'
#' @inheritParams htmlTable
#' @inheritParams prGetCgroupHeader
#' @param total_columns The total number of columns including the rowlabel and the
#'  specer cells
#' @return `string` Returns the html string for the `<thead>...</thead>` element
#' @keywords internal
#' @importFrom stringr str_interp
prGetThead <- function(x,
                       header = NULL,
                       cgroup = NULL, n.cgroup = NULL,
                       caption = NULL,
                       compatibility,
                       total_columns,
                       css.cgroup,
                       top_row_style,
                       rnames,
                       rowlabel = NULL,
                       cgroup_spacer_cells,
                       prepped_cell_css,
                       style_list,
                       cell_style) {
  first_row <- TRUE
  # Start the head
  head_str <- "\n\t<thead>"

  if (!is.null(caption) &
    compatibility == "LibreOffice" &
    !style_list$pos.caption %in% c("bottom", "below")) {
    head_str %<>% paste(str_interp("<tr><td colspan='${COLSPAN}' style='text-align: left;'>${CONTENT}</td></tr>",
                                   list(COLSPAN = total_columns,
                                        CONTENT = caption)),
                        sep = "\n\t")
  }

  # Add the cgroup table header
  if (!is.null(cgroup)) {
    for (i in 1:nrow(cgroup)) {
      cgrp_str <- prGetCgroupHeader(
        x = x,
        cgroup_vec = cgroup[i, ],
        n.cgroup_vec = n.cgroup[i, ],
        cgroup_vec.just = style_list$align.cgroup[i, ],
        css_4_cgroup_vec = style_list$css.cgroup[i, ],
        row_no = i,
        top_row_style = top_row_style,
        rnames = rnames,
        rowlabel = rowlabel,
        style_list = style_list,
        cgroup_spacer_cells = cgroup_spacer_cells,
        prepped_cell_css = prepped_cell_css
      )
      head_str %<>%
        paste0(cgrp_str)
    }
    first_row <- FALSE
  }


  # Add the header
  if (!is.null(header)) {
    header_rowlabel_str <- NA
    no_cgroup_rows <- ifelse(!is.null(cgroup), nrow(cgroup), 0)
    ts <- ifelse(no_cgroup_rows > 0, "", top_row_style)

    header_list <- NULL
    if (!is.null(rowlabel) && style_list$pos.rowlabel == no_cgroup_rows + 1) {
      header_list <- list(STYLE = prGetStyle(style_list$css.header.border_bottom,
                                             style_list$css.header[1],
                                             ts,
                                             attr(prepped_cell_css, "rnames")[1],
                                             align = prGetAlign(style_list$align.header, 1, style_list = style_list)),
                          CONTENT = rowlabel)
    } else if (!prSkipRownames(rnames)) {
      header_list <- list(STYLE = prGetStyle(style_list$css.header.border_bottom,
                                             ts),
                          CONTENT = "")
    }

    if (!is.null(header_list)) {
      header_rowlabel_str <- paste(str_interp("<th style='${STYLE}'>${CONTENT}</th>", header_list),
                            sep = "\n\t\t")
    }


    cell_style <- c(style_list$css.header.border_bottom)
    if (first_row) {
      cell_style %<>% c(top_row_style)
    }

    cell_str <- prAddCells(
      rowcells = header,
      cellcode = "th",
      style_list = style_list,
      style = cell_style,
      cgroup_spacer_cells = cgroup_spacer_cells,
      has_rn_col = !prSkipRownames(rnames) * 1,
      prepped_cell_css = attr(prepped_cell_css, "header"),
      style_list_align_key = "align.header"
    )

    # The bottom border was ment to be here but it doesn't
    # work that well in the export
    if (is.na(header_rowlabel_str)) {
      head_str %<>% paste(paste0("<tr>", cell_str),
                          "</tr>",
                          sep = "\n\t")

    } else {
      head_str %<>% paste(paste0("<tr>", header_rowlabel_str, cell_str),
                          "</tr>",
                          sep = "\n\t")
    }

    first_row <- FALSE
  }

  #################################
  # Close head and start the body #
  #################################
  head_str %<>%
    paste0("\n\t</thead>")
  return(head_str)
}
