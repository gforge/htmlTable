#' Add an empty cell
#'
#'
#' Depending on the `spacer.celltype` set in [addHtmlTableStyle()] we
#' will use different spacer cells.
#'
#' @param cell_style The style of the current cell that should be applied to all cells
#' @param align_style The style from [prGetAlign()]
#' @param cell_tag What HTML tag to use
#' @param colspan The number of rows each tag should span
#'
#' @return `string`
#' @keywords internal
#' @inheritParams htmlTable
#' @family hidden helper functions for htmlTable
#' @importFrom stringr str_interp str_replace
prAddEmptySpacerCell <- function(x,
                                 style_list,
                                 cell_style,
                                 align_style,
                                 cell_tag = c("td", "th"),
                                 colspan = 1) {
  str_to_append <- switch(style_list$spacer.celltype,
                          single_empty = "<${TAG} style='${CELL_STYLE}' colspan=${COLSPAN}>${CONTENT}</${TAG}>",
                          skip = "",
                          double_cell = paste("<${TAG} style='${CELL_STYLE}${CELL_STYLE_BORDER}' colspan=${COLSPAN}>${CONTENT}</${TAG}>",
                                              "<${TAG} style='${CELL_STYLE}' colspan=${COLSPAN}>${CONTENT}</${TAG}>"))

  if (is.null(str_to_append)) {
    stop("The cell style has not been implemented")
  }

  border_style = ""
  if (attr(align_style, "has_border")) {
    border_style = paste("border-right:", attr(align_style, "border_style")$default)
  }

  variables <- list(TAG = match.arg(cell_tag),
                    CELL_STYLE = prGetStyle(style_list$spacer.css,
                                            cell_style),
                    COLSPAN = colspan,
                    CONTENT = style_list$spacer.content,
                    CELL_STYLE_BORDER = border_style)
  str_to_append %<>% str_interp(variables)

  paste0(x, str_to_append)
}


prGetEmptySpacerCellSize <- function(style_list) {
  no <- switch(style_list$spacer.celltype,
               single_empty = 1,
               skip = 0,
               double_cell = 2)

  if (is.null(no)) {
    stop("The cell style has not been implemented")
  }

  return(no)
}