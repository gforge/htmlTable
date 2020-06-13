#' Merges multiple colors
#'
#' Uses the \code{\link[grDevices]{colorRampPalette}} for merging colors.
#' \emph{Note:} When merging more than 2 colors the order in the color
#' presentation matters. Each color is merged with its neigbors before
#' merging with next. If there is an uneven number of colors the middle
#' color is mixed with both left and right side.
#'
#' @param clrs The colors
#' @return \code{character} A hexadecimal color
#' @import magrittr
#' @keywords internal
#' @importFrom grDevices colorRampPalette
#' @importFrom utils head
prMergeClr<- function(clrs){
  if (length(clrs) == 1)
    return(clrs)
  if (length(clrs) == 2)
    return(colorRampPalette(clrs)(3)[2])

  split_lngth <- floor(length(clrs)/2)
  left <- head(clrs, split_lngth)
  right <- tail(clrs, split_lngth)
  if (length(clrs) %% 2 == 1){
    left %<>%
      c(clrs[split_lngth + 1])
    right %<>%
      c(clrs[split_lngth + 1], .)
  }

  left <- prMergeClr(left)
  right <- prMergeClr(right)
  return(prMergeClr(c(left,
                        right)))
}