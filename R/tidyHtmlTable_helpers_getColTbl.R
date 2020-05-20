getColTbl <- function(x) {
  out <- prExtractElementsAndConvertToTbl(x, elements = c("cgroup", "header"))

  out$c_idx <- 1:nrow(out)
  return(out)
}
