#' Retrieve a header row
#'
#' This function retrieves a header row, i.e. a row
#' within the `<th>` elements on top of the table. Used by
#' [htmlTable()].
#'
#' @param cgroup_vec The `cgroup` may be a `matrix`, this is
#'  just one row of that `matrix`
#' @param n.cgroup_vec The same as above but for the counter
#' @param cgroup_vec.just The same as above bot for the justification
#' @param row_no The row number within the header group. Useful for multi-row
#'  headers when we need to output the `rowlabel` at the `pos.rowlabel`
#'  level.
#' @param style_list The list with all the styles
#' @param top_row_style The top row has a special style depending on
#'  the `ctable` option in the `htmlTable` call.
#' @param cgroup_spacer_cells The spacer cells due to the multiple cgroup levels.
#'  With multiple rows in cgroup we need to keep track of how many spacer cells
#'  occur between the columns. This variable contains is of the size `ncol(x)-1`
#'  and 0 if there is no cgroup element between.
#' @return `string`
#' @keywords internal
#' @inheritParams htmlTable
#' @family hidden helper functions for htmlTable
#' @importFrom stringr str_interp
prGetCgroupHeader <- function(x,
                              cgroup_vec,
                              n.cgroup_vec,
                              cgroup_vec.just,
                              row_no, top_row_style,
                              rnames,
                              rowlabel = NULL,
                              cgroup_spacer_cells,
                              style_list,
                              prepped_cell_css,
                              css_4_cgroup_vec) {
  header_str <- "\n\t<tr>"
  if (row_no == 1) {
    ts <- top_row_style
  } else {
    ts <- ""
  }

  if (!is.null(rowlabel)) {
    if (row_no == style_list$pos.rowlabel) {
      header_str %<>% sprintf(
        "%s\n\t\t<th style='%s'>%s</th>",
        .,
        prGetStyle(
          c(`font-weight` = 900),
          ts,
          attr(prepped_cell_css, "rnames")[1]
        ),
        rowlabel
      )
    } else {
      header_str %<>%
        sprintf(
          "%s\n\t\t<th style='%s'></th>",
          .,
          prGetStyle(ts)
        )
    }
  } else if (!prSkipRownames(rnames)) {
    header_str %<>% sprintf(
      "%s\n\t\t<th style='%s'></th>",
      .,
      prGetStyle(ts)
    )
  }

  for (i in 1:length(cgroup_vec)) {
    if (!is.na(n.cgroup_vec[i])) {
      start_column <- ifelse(i == 1,
        1,
        sum(n.cgroup_vec[1:(i - 1)], na.rm = TRUE) + 1
      )

      # 10 3-1
      # 0 0 1
      colspan <- n.cgroup_vec[i] +
        ifelse(start_column > length(cgroup_spacer_cells) || n.cgroup_vec[i] == 1,
        0,
        ifelse(start_column == 1,
          sum(cgroup_spacer_cells[1:(n.cgroup_vec[i] - 1)]),
          ifelse(sum(n.cgroup_vec[1:i], na.rm = TRUE) == ncol(x),
            sum(cgroup_spacer_cells[start_column:length(cgroup_spacer_cells)]),
            sum(cgroup_spacer_cells[start_column:((start_column - 1) + (n.cgroup_vec[i] - 1))])
          )
        ) * prGetEmptySpacerCellSize(style_list = style_list)
        )


      header_align <- prGetAlign(cgroup_vec.just,
                                 index = i,
                                 style_list = style_list)
      if (nchar(cgroup_vec[i]) == 0) { # Removed as this may now be on purpose || is.na(cgroup_vec[i]))
        header_values <- list(COLSPAN = colspan,
                              STYLE = prGetStyle(c(`font-weight` = 900),
                                                 ts,
                                                 header_align,
                                                 css_4_cgroup_vec[i]),
                              CONTENT = "")
      } else {
        header_values <- list(COLSPAN = colspan,
                              STYLE = prGetStyle(c(`font-weight` = 900,
                                                   `border-bottom` = "1px solid grey"),
                                                 ts,
                                                 header_align,
                                                 css_4_cgroup_vec[i]),
                              CONTENT = cgroup_vec[i])
      }

      header_str %<>% paste(str_interp("<th colspan='${COLSPAN}' style='${STYLE}'>${CONTENT}</th>",
                                       header_values),
                            sep = "\n\t\t")

      # If not last then add a filler cell between the row categories
      # this is also the reason that we need the cgroup_spacer_cells
      if (i != sum(!is.na(cgroup_vec))) {
        bottom_border_style = str_interp("border-bottom: ${STYLE};",
                                         list(STYLE = style_list$spacer.css.cgroup.bottom.border))
        header_str %<>% prAddEmptySpacerCell(style_list = style_list,
                                             cell_style = prGetStyle(bottom_border_style,
                                                                     ts),
                                             align_style = header_align,
                                             cell_tag = "th")
      }
    }
  }
  header_str %<>%
    paste0("\n\t</tr>")

  return(header_str)
}
