getRowTbl <- function(x) {
  out <- prExtractElementsAndConvertToTbl(x,
    elements = c("tspanner", "rgroup", "rnames")
  ) %>%
    dplyr::arrange() %>%
    # This is necessary in order to not generate NA values when setting
    # hidden elements to "" and this can't be in prExtractElementsAndConvertToTbl
    # as we need to be able to sort according to the column in getColTbl
    dplyr::mutate_if(is.factor, as.character)

  out$r_idx <- 1:nrow(out)
  return(out)
}
