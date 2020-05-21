getRowTbl <- function(x) {
  out <- prExtractElementsAndConvertToTbl(x,
                                          elements = c("tspanner", "rgroup", "rnames"))

  out$r_idx <- 1:nrow(out)
  return(out)
}

prExtractElementsAndConvertToTbl <- function(x, elements) {
  x[elements] %>%
    purrr::keep(~!is.null(.)) %>%
    tibble::as_tibble() %>%
    dplyr::distinct() %>%
    dplyr::arrange_at(dplyr::vars(tidyselect::any_of(elements))) %>%
    # This is necessary in order to not generate NA values when setting
    # hidden elements to ""
    dplyr::mutate_if(is.factor, as.character)
}