# This checks to make sure that the mapping columns of the tidy dataset
# uniquely specify a given value
checkUniqueness <- function(tidyTableDataList) {
  tidyTableData <- do.call(cbind, tidyTableDataList)
  dupes <- tidyTableData %>% duplicated()
  if (sum(dupes) != 0) {
    core_msg <- paste0("The input parameters ",
                       paste(paste0("\"", names(tidyTableData), "\""), collapse = ", "),
                       " do not specify unique rows, have you forgotten one?.")
    duplicated_rows <- paste0("The following rows are duplicated: ", paste(which(dupes), collapse = ", "))
    if (is.null(tidyTableDataList$rnames_unique)) {
      core_msg <- paste(core_msg,
                        "Check if you intended to provide the rnames_unique (see the help page).")
    }
    stop(core_msg, "\n", duplicated_rows)
  }
}
