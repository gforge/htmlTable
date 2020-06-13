#' Retrieves counts for rgroup, cgroup, & tspanner arguments
#'
#' This function is a wrapper to \code{\link[base]{rle}} that
#' does exactly this but is a little too picky about input values.
#'
#' @param x The vector to process
#' @return \code{list(n = rle$lengths, names = rle$values)}
#' @export
#' @examples
#' prepGroupCounts(c(1:3, 3:1))
prepGroupCounts <- function(x) {
  # Drop all classes but the base class as rle
  counts <- rle(as.vector(x))
  ret <- list(
    n = counts$lengths,
    idx = cumsum(counts$lengths),
    names = counts$values
  )
  structure(ret,
    class = c("htmlTable_group_count", class(ret))
  )
}
