#' Generate an htmlTable using a ggplot2-like interface
#'
#' Builds an \code{htmlTable} by mapping columns from the input data, \code{x},
#' to elements of an output \code{htmlTable} (e.g. rnames, header, etc.)
#'
#' @section Column-mapping parameters:
#'
#'   The \code{tidyHtmlTable} function is designed to work like ggplot2 in that
#'   columns from \code{x} are mapped to specific parameters from the
#'   \code{htmlTable} function. At minimum, \code{x} must contain the names
#'   of columns mapping to \code{rnames}, \code{header}, and \code{rnames}.
#'   \code{header} and \code{rnames} retain the same meaning as in the
#'   htmlTable function. \code{value} contains the individual values that will
#'   be used to fill each cell within the output \code{htmlTable}.
#'
#'   A full list of parameters from \code{htmlTable} which may be mapped to
#'   columns within \code{x} include:
#'
#'   \itemize{
#'     \item \code{value}
#'     \item \code{header}
#'     \item \code{rnames}
#'     \item \code{rgroup}
#'     \item \code{cgroup1}
#'     \item \code{cgroup2}
#'     \item \code{tspanner}
#'   }
#'
#'   Note that unlike in \code{htmlTable} which contains \code{cgroup},
#'   and which may specify a variable number of column groups,
#'   \code{tidyhtmlTable} contains the parameters \code{cgroup1} and
#'   \code{cgroup2}. These parameters correspond to the inward most and outward
#'   most column groups respectively.
#'
#'   Also note that the coordinates of each \code{value} within \code{x} must be
#'   unambiguously mapped to a position within the output \code{htmlTable}.
#'   Therefore, the each row-wise combination the variables specified above
#'   contained in \code{x} must be unique.
#'
#' @section Hidden values:
#'   \code{htmlTable} Allows for some values within \code{rgroup},
#'   \code{cgroup}, etc. to be specified as \code{""}. The following parameters
#'   allow for specific values to be treated as if they were a string of length
#'   zero in the \code{htmlTable} function.
#'
#'   \itemize{
#'     \item \code{hidden_rgroup}
#'     \item \code{hidden_tspanner}
#'   }
#' @section Additional dependencies:
#'    In order to run this function you also must have \code{\link[dplyr]{dplyr}},
#'    \code{\link[tidyr]{tidyr}}, \code{\link[tidyselect]{tidyselect}} and
#'    \code{\link[purrr]{purrr}} packages installed. These have been removed due to
#'    the additional 20 Mb that these dependencies added (issue #47).
#'
#' @param x Tidy data used to build the \code{htmlTable}
#' @param value The column containing values filling individual cells of the
#' output \code{htmlTable}. Defaults to "value" as used by \code{\link[tidyr]{pivot_longer}}.
#' @param header The column in \code{x} specifying column headings
#' @param rnames The column in \code{x} specifying row names. Defaults to "name" as used by
#'  \code{\link[tidyr]{pivot_longer}}.
#' @param rgroup The column in \code{x} specifying row groups
#' @param hidden_rgroup \code{strings} with rgroup values that will be hidden  (the values will
#'  still be there but thhe spanner will be set to "" and thus ignored byt \code{\link{htmlTable}}).
#' @param cgroup The column or columns in \code{x} specifying the column groups
#' @param tspanner The column in \code{x} specifying tspanner groups
#' @param hidden_tspanner \code{strings} with tspanner values that will be hidden (the values will
#'  still be there but thhe spanner will be set to "" and thus ignored byt \code{\link{htmlTable}}).
#' @param skip_removal_warning \code{boolean} supress warning message when removing NA columns.
#' @param ... Additional arguments that will be passed to the inner
#' \code{\link{htmlTable}} function
#' @return Returns html code that will build a pretty table
#' @export
#' @seealso \code{\link{htmlTable}}
#' @example inst/examples/tidyHtmlTable_example.R
tidyHtmlTable <- function(x,
                          value,
                          header,
                          rnames,
                          rgroup,
                          hidden_rgroup,
                          cgroup,
                          tspanner,
                          hidden_tspanner,
                          skip_removal_warning = getOption("htmlTable.skip_removal_warning", FALSE),
                          ...) {
  UseMethod("tidyHtmlTable")
}

#' @export
tidyHtmlTable.default <- function(x, ...) {
  stop("x must be of class data.frame")
}

#' @export
tidyHtmlTable.data.frame <- function(x,
                                     value,
                                     header,
                                     rnames,
                                     rgroup,
                                     hidden_rgroup,
                                     cgroup,
                                     tspanner,
                                     hidden_tspanner,
                                     skip_removal_warning = FALSE,
                                     ...) {
  # You need the suggested package for this function
  safeLoadPkg("dplyr")
  safeLoadPkg("tidyr")
  safeLoadPkg("tidyselect")
  safeLoadPkg("purrr")

  # Re-attach style to the new object at the end
  style_list <- prGetAttrWithDefault(x, which = style_attribute_name, default = NULL)

  # Check if x is a grouped tbl_df
  if (dplyr::is.grouped_df(x)) {
    x <- dplyr::ungroup(x)
  }

  tidyTableDataList <- list(
    value = prAssertAndRetrieveValue(x, value),
    header = prAssertAndRetrieveValue(x, header),
    rnames = prAssertAndRetrieveValue(x, rnames, name = "name"),
    rgroup = prAssertAndRetrieveValue(x, rgroup, optional = TRUE),
    cgroup = prAssertAndRetrieveValue(x, cgroup, optional = TRUE),
    tspanner = prAssertAndRetrieveValue(x, tspanner, optional = TRUE)
  ) %>%
    purrr::keep(~ !is.null(.))

  checkUniqueness(tidyTableDataList)

  tidyTableDataList %<>% removeRowsWithNA(skip_removal_warning = skip_removal_warning)

  # Create tables from which to gather row, column, and tspanner names
  # and indices
  rowRefTbl <- getRowTbl(tidyTableDataList)

  # Hide row groups specified in hidden_rgroup
  if (!missing(hidden_rgroup)) {
    rowRefTbl <- rowRefTbl %>%
      dplyr::mutate_at(
        rgroup,
        function(x) {
          ifelse(x %in% hidden_rgroup, "", x)
        }
      )
  }

  # Hide tspanners specified in hidden_tspanner
  if (!missing(hidden_tspanner)) {
    rowRefTbl <- rowRefTbl %>%
      dplyr::mutate_at(
        tspanner,
        function(x) {
          ifelse(x %in% hidden_tspanner, "", x)
        }
      )
  }

  colRefTbl <- getColTbl(tidyTableDataList)

  # Format the values for display
  formatted_df <- suppressMessages(tidyTableDataList %>%
                                     tibble::as_tibble()  %>%
                                     dplyr::inner_join(getColTbl(tidyTableDataList)) %>%
                                     dplyr::inner_join(getRowTbl(tidyTableDataList))) %>%
    dplyr::select(r_idx, c_idx, value) %>%
    dplyr::mutate_at(vars(value), as.character) %>%
    tidyr::pivot_wider(names_from = "c_idx") %>%
    dplyr::select(-r_idx)

  # Get names and indices for row groups and tspanners
  htmlTable_args <- list(
    x = formatted_df,
    rnames = rowRefTbl %>% dplyr::pull(rnames),
    header = colRefTbl %>% dplyr::pull(header),
    ...
  )

  if (!missing(rgroup)) {
    # This will take care of a problem in which adjacent row groups
    # with the same value will cause rgroup and tspanner collision
    comp_val <- rowRefTbl %>% dplyr::pull(rgroup)

    if (!missing(tspanner)) {
      comp_val <- paste0(
        comp_val,
        rowRefTbl %>% dplyr::pull(tspanner)
      )
    }

    lens <- rle(comp_val)$lengths
    idx <- cumsum(lens)

    htmlTable_args$rgroup <- rowRefTbl %>%
      dplyr::slice(idx) %>%
      dplyr::pull(rgroup)

    htmlTable_args$n.rgroup <- lens
  }

  if (!missing(tspanner)) {
    htmlTable_args$tspanner <-
      rle(rowRefTbl %>% dplyr::pull(tspanner))$value
    htmlTable_args$n.tspanner <-
      rle(rowRefTbl %>% dplyr::pull(tspanner))$lengths
  }

  # Get names and indices for column groups
  if (!missing(cgroup)) {
    cg <- list(values = list(), lengths = list())
    noCgroup <- 1
    if (is.data.frame(tidyTableDataList$cgroup)) {
      noCgroup <- ncol(tidyTableDataList$cgroup)
    }

    for (colNo in 1:noCgroup) {
      counts <- rle(colRefTbl %>% dplyr::pull(colNo))
      cg$values[[colNo]] <- counts$value
      cg$lengths[[colNo]] <- counts$lengths
    }

    maxLen <- sapply(cg$values, length) %>% max
    for (colNo in 1:length(cg$values)) {
      missingNA <- maxLen - length(cg$values[[colNo]])
      if (missingNA > 0) {
        cg$values[[colNo]] <- c(cg$values[[colNo]], rep(NA, times = missingNA))
        cg$lengths[[colNo]] <- c(cg$lengths[[colNo]], rep(NA, times = missingNA))
      }
    }

    if (length(cg$values) == 1) {
      htmlTable_args$cgroup <- cg$values[[1]]
      htmlTable_args$n.cgroup <- cg$lengths[[1]]
    } else {
      htmlTable_args$cgroup <- do.call(rbind, cg$values)
      htmlTable_args$n.cgroup <- do.call(rbind, cg$lengths)
    }
  }

  if (!is.null(style_list)) {
    attr(htmlTable_args$x, style_attribute_name) <- style_list
  }
  do.call(htmlTable, htmlTable_args)
}