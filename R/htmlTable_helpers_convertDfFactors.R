#' Convert all factors to characters to print them as they expected
#'
#' @inheritParams htmlTable
#' @return The data frame with factors as characters
prConvertDfFactors <- function(x){
  if (!"data.frame" %in% class(x))
    return(x)

  i <- sapply(x, function(col)
    (
      (
        !is.numeric(col) &&
          !is.character(col)
      ) ||
        (
          inherits(col, "times") # For handlin Chron input
        )
    )
  )

  if(any(i)){
    x[i] <- lapply(x[i], as.character)
  }

  return (x)
}
