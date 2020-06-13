# This checks to make sure that the mapping columns of the tidy dataset
# uniquely specify a given value
checkUniqueness <- function(tidyTableDataList) {
  tidyTableData <- do.call(cbind, tidyTableDataList)
  dupes <- tidyTableData %>% duplicated()
  if (sum(dupes) != 0) {
    stop(paste0(
      "The input parameters ",
      paste(paste0("\"", names(tidyTableData), "\""), collapse = ", "),
      " do not specify unique rows. The following rows ",
      "are duplicated: ",
      paste(which(dupes), collapse = ", ")
    ))
  }
}
