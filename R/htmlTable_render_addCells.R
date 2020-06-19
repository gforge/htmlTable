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
#' @param style_list The style_list
#' @return \code{string} Returns the string with the new cell elements
#' @keywords internal
#' @family hidden helper functions for htmlTable
prAddCells <- function(rowcells, cellcode, style_list, style, prepped_cell_css, cgroup_spacer_cells, has_rn_col, offset = 1) {
  cell_str <- ""
  style <- prAddSemicolon2StrEnd(style)

  for (nr in offset:length(rowcells)) {
    cell_value <- rowcells[nr]
    # We don't want missing to be NA in a table, it should be empty
    if (is.na(cell_value)) {
      cell_value <- ""
    }

    cell_style <- c(
      prepped_cell_css[nr],
      style,
      prGetAlign(style_list$align, index = nr + has_rn_col)
    )
    if (!is.null(style_list$col.columns)) {
      cell_style %<>%
        c(`background-color` = style_list$col.columns[nr])
    }

    cell_str %<>%
      sprintf(
        "%s\n\t\t<%s style='%s'>%s</%s>",
        .,
        cellcode,
        prGetStyle(cell_style),
        cell_value,
        cellcode
      )

    # Add empty cell if not last column
    if (nr != length(rowcells) &&
      nr <= length(cgroup_spacer_cells) &&
      cgroup_spacer_cells[nr] > 0) {
      # The same style as previous but without align borders
      cell_style <- c(
        prepped_cell_css[nr],
        style,
        prGetAlign(style_list$align, index = nr + has_rn_col, emptyCell = TRUE)
      )
      spanner_style <- style

      if (!is.null(style_list$col.columns)) {
        if (style_list$col.columns[nr] == style_list$col.columns[nr + 1]) {
          spanner_style %<>% c(`background-color` = style_list$col.columns[nr])
        }
      }

      cell_str %<>%
        sprintf(
          "%s\n\t\t<%s style='%s' colspan='%d'>&nbsp;</%s>",
          .,
          cellcode,
          prGetStyle(cell_style, spanner_style),
          cgroup_spacer_cells[nr],
          cellcode
        )
    }
  }
  return(cell_str)
}
