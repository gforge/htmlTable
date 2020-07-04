#' Prepares the alternating colors
#'
#' @param clr The colors
#' @param n The number of rows/columns applicable to the color
#' @param ng The n.rgroup/n.cgroup argument if applicable
#' @param gtxt The rgroup/cgroup texts
#' @return `character` A vector containing hexadecimal colors
#' @import magrittr
#' @keywords internal
#' @importFrom grDevices col2rgb
prPrepareColors <- function(clr, n = NULL, ng = NULL, gtxt) {
  clr <- sapply(clr, function(a_clr) {
    if (a_clr == "none") {
      return(a_clr)
    }
    if (grepl("^#[0-9ABCDEFabcdef]{3,3}$", a_clr)) {
      a_clr %<>%
        substring(first = 2) %>%
        strsplit(split = "") %>%
        unlist() %>%
        sapply(FUN = rep, times = 2) %>%
        paste(collapse = "") %>%
        tolower() %>%
        paste0("#", .)
    } else {
      a_clr %<>%
        col2rgb %>%
        as.hexmode() %>%
        as.character() %>%
        paste(collapse = "") %>%
        paste0("#", .)
    }
  }, USE.NAMES = FALSE)

  if (!is.null(ng)) {
    # Split groups into separate if the gtxt is ""
    if (any(gtxt == "")) {
      tmp <- c()
      for (i in 1:length(ng)) {
        if (gtxt[i] != "" &&
          !is.na(gtxt[i])) {
          tmp <- c(
            tmp,
            ng[i]
          )
        } else {
          tmp <- c(
            tmp,
            rep(1, ng[i])
          )
        }
      }
      ng <- tmp
    }

    clr <- rep(clr, length.out = length(ng))
    attr(clr, "groups") <-
      Map(rep, clr, length.out = ng)
  } else if (!is.null(n)) {
    clr <- rep(clr, length.out = n)
  }

  return(clr)
}
