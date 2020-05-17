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