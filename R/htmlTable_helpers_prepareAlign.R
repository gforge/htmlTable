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
#' @family hidden helper functions for htmlTable
#' @inheritParams htmlTable
prPrepareAlign <- function(align, x, rnames, default_rn = "l") {
  assert_character(align)

  if (length(align) > 1) {
    align <- paste(align, collapse = "")
  }

  segm_rgx <- "[^lrc]*[rlc][^lrc]*"
  no_elements <- length(strsplit(align, split = segm_rgx)[[1]])
  no_cols <- ifelse(is.null(dim(x)), x, ncol(x))
  if (!prSkipRownames(rnames)) {
    no_cols <- no_cols + 1
    if (no_elements < no_cols) {
      align <- paste0(default_rn, align)
    }
  }

  res_align <- align
  align <- ""
  for (i in 1:no_cols) {
    rmatch <- regexpr(segm_rgx, res_align)
    tmp_lrc <- substr(res_align, 1, rmatch + attr(rmatch, "match.length") - 1)
    res_align <- substring(res_align, rmatch + attr(rmatch, "match.length"))
    align <- paste0(
      align,
      tmp_lrc
    )
    if (nchar(res_align) < 1 &&
      i != no_cols) {
      align <- paste0(
        align,
        paste(rep(tmp_lrc, times = no_cols - i), collapse = "")
      )
      break
    }
  }

  structure(align,
    n = no_cols,
    class = class(align)
  )
}
