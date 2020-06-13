# Removes rows containing NA values in any mapped columns from the tidy dataset
removeRowsWithNA <- function(tidyTableDataList, skip_removal_warning = FALSE) {
  tidyTableData <- tidyTableDataList %>% tibble::as_tibble()

  hasNA <- tidyTableData %>% is.na()

  naPerRow <- hasNA %>%
    rowSums()

  keepIdx <- naPerRow == 0
  removed <- sum(naPerRow > 0)

  if (removed != 0) {
    naPerCol <- hasNA %>% colSums()
    naColumns <- colnames(hasNA)[naPerCol > 0]
    if (!skip_removal_warning) {
      warning(paste0(
        "NA values were detected in the following columns of ",
        "the tidy dataset: ",
        paste(naColumns, collapse = ", "), ". ",
        removed, " row(s) in the tidy dataset were removed."
      ))
    }
  }

  return(sapply(tidyTableDataList,
    function(x) {
      if (is.data.frame(x)) {
        return(x %>% dplyr::filter(keepIdx))
      }

      return(x[keepIdx])
    },
    simplify = FALSE
  ))
}
