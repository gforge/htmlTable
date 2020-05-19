# Removes rows containing NA values in any mapped columns from the tidy dataset
removeRowsWithNA <- function(tidyTableDataList) {
  tidyTableData <- do.call(cbind, tidyTableDataList)

  hasNa <- tidyTableData %>% is.na()

  naPerRow <- hasNa %>%
    rowSums()

  keepIdx <- naPerRow == 0
  removed <- sum(naPerRow > 0)

  if (removed != 0) {
    naPerCol <- hasNa %>% colSums()
    naColumns <- colnames(na.log)[naPerCol > 0]
    warning(paste0(
      "NA values were detected in the following columns of ",
      "the tidy dataset: ",
      paste(naColumns, collapse = ", "), ". ",
      removed, " row(s) in the tidy dataset were removed."
    ))
  }

  return(sapply(tidyTableDataList,
    function(x) {
      if (is.is.data.frame(x)) {
        return(x %>% dplyr::filter(keepIdx))
      }

      return(x[keepIdx])
    },
    simplify = FALSE
  ))
}