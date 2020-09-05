#' Gets alignment
#'
#' @param index The index of the align parameter of interest
#' @family hidden helper functions for
#' @keywords internal
#' @inheritParams addHtmlTableStyle
prGetAlign <- function(align,
                       index,
                       style_list = NULL,
                       spacerCell = FALSE,
                       followed_by_spacer_cell = FALSE,
                       previous_was_spacer_cell = FALSE) {
  segm_rgx <- "[^lrc]*[rlc][^lrc]*"

  res_align <- align
  align <- ""
  # Loop to remove every element prior to the one of interest
  for (i in 1:index) {
    if (nchar(res_align) == 0) {
      stop("Requested column outside of span, ", index, " > ", i)
    }

    rmatch <- regexpr(segm_rgx, res_align)
    lrc_data <- substr(res_align, 1, rmatch + attr(rmatch, "match.length") - 1)
    res_align <- substring(res_align, rmatch + attr(rmatch, "match.length"))
  }

  styles <- c()
  border_in_spacer_cell <- FALSE
  if (!is.null(style_list) && style_list$spacer.celltype == "double_cell") {
    border_in_spacer_cell = TRUE
  }

  border_position <- NULL
  if (grepl("^\\|", lrc_data)) {
    border_position <- "left"
  }

  if (grepl("\\|$", lrc_data)) {
    border_position <- c(border_position, "right")
  }

  border_style <- list(default = getOption("htmlTable.css.border", default = "1px solid black"))

  if (!is.null(border_position)) {
    for (pos in border_position) {
      border_name <- paste0("border-", pos)
      border_style[[pos]] <- getOption(paste0("htmlTable.css.", border_name),
                                       default = border_style$default)

      if (!spacerCell &&
          (!border_in_spacer_cell ||
           (!followed_by_spacer_cell && pos == "right") ||
           (!previous_was_spacer_cell && pos == "left"))) {
        styles[border_name] <- border_style[[pos]]
      }
    }
  }

  if (grepl("l", lrc_data)) {
    styles["text-align"] <- "left"
  }
  if (grepl("c", lrc_data)) {
    styles["text-align"] <- "center"
  }
  if (grepl("r", lrc_data)) {
    styles["text-align"] <- "right"
  }

  return(structure(styles,
                   has_border = !is.null(border_position),
                   border_position = border_position,
                   border_style = border_style))
}
