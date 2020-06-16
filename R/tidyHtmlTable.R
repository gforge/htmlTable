#' Generate an htmlTable using tidy data as input
#'
#' Builds an \code{htmlTable} by mapping columns from the input data, \code{x},
#' to elements of an output \code{htmlTable} (e.g. rnames, header, etc.). This
#' provides a ggplot2-like interface you can pivot rows/columns as required. The
#' typical use case is when you are using \code{dplyr} together with the
#' \code{tidyverse} data processing functions, see \code{vignette("tidyHtmlTable")}.
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
#'     \item \code{cgroup}
#'     \item \code{tspanner}
#'   }
#'
#'   Also note that the coordinates of each \code{value} within \code{x} must be
#'   unambiguously mapped to a position within the output \code{htmlTable}.
#'   Therefore, the each row-wise combination the variables specified above
#'   contained in \code{x} must be unique.
#'
#' @section Sorting:
#'
#'   Sorting of rows is as of version 2.0 skipped as we may have situations with
#'   repeating inputs and this can easily be performed pre-function by calling
#'   \code{\link[dplyr]{arrange}()} prior to \code{tidyHtmlTable}.
#'
#'   Columns are sorted by \code{arrange(cgroup, header)} where cgroup will be
#'   expanded to the columns of the cgroup argument, e.g. \code{cgroup = c(a, b), header = c}
#'   will become \code{arrange(a, b, c)}. If you want to sort in non-alphabetic order
#'   you can provide a \code{factor} variable and that information will be retained.
#'
#' @section Hidden values:
#'
#'   \code{htmlTable} Allows for some values within \code{rgroup},
#'   \code{cgroup}, etc. to be specified as \code{""}. The following parameters
#'   allow for specific values to be treated as if they were a string of length
#'   zero in the \code{htmlTable} function.
#'
#'   \itemize{
#'     \item \code{hidden_rgroup}
#'     \item \code{hidden_tspanner}
#'   }
#'
#' @section Simple tibble output:
#'
#'   The tibble discourages the use of rownames. There is therefore a convenience
#'   option for \code{tidyHtmlTable} where you can use the function just as you
#'   would with htmlTable where \code{rnames} is populated with "names" column or
#'   the \code{rnames} argument provided using tidyselect syntax.
#'
#' @section Additional dependencies:
#'
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
#' @param table_fn The table function that should receive the input, defaults to \code{\link{htmlTable}}
#'  but you can provide any function that uses the same input formatting. This package was inspired
#'  by the \code{\link[Hmisc]{latex}} function.
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
                          table_fn = htmlTable,
                          ...) {
  UseMethod("tidyHtmlTable")
}

#' @export
tidyHtmlTable.default <- function(x,
                                  value,
                                  header,
                                  rnames,
                                  rgroup,
                                  hidden_rgroup,
                                  cgroup,
                                  tspanner,
                                  hidden_tspanner,
                                  skip_removal_warning = getOption("htmlTable.skip_removal_warning", FALSE),
                                  table_fn = htmlTable,
                                  ...) {
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
                                     table_fn = htmlTable,
                                     ...) {
  # You need the suggested package for this function
  safeLoadPkg("dplyr")
  safeLoadPkg("tidyr")
  safeLoadPkg("tidyselect")
  safeLoadPkg("purrr")
  safeLoadPkg("rlang")

  # Re-attach style to the new object at the end
  style_list <- prGetAttrWithDefault(x, which = style_attribute_name, default = NULL)

  # Check if x is a grouped tbl_df
  if (dplyr::is.grouped_df(x)) {
    x <- dplyr::ungroup(x)
  }

  if (missing(value) && missing(header)) {
    # Sometimes we just want to print a tibble and these don't allow for
    # rownames and htmlTable becomes a little annoying why we want to
    # have a tidyverse compatible option
    if (missing(rnames)) {
      orgName <- rlang::as_name("name")
    } else {
      orgName <- substitute(rnames)
    }

    args <- list(...)
    args$x <- x %>% dplyr::select(-{{orgName}})
    args$rnames <- x[[as.character(orgName)]]
    if (is.null(args$rowlabel)) {
      args$rowlabel <- as.character(orgName)
    }
    return(do.call(htmlTable, args))
  }

  tidyTableDataList <- list(
    value = prAssertAndRetrieveValue(x, value),
    header = prAssertAndRetrieveValue(x, header),
    rnames = prAssertAndRetrieveValue(x, rnames, name = "name"),
    rgroup = prAssertAndRetrieveValue(x, rgroup, optional = TRUE),
    cgroup = prAssertAndRetrieveValue(x, cgroup, optional = TRUE, maxCols = getOption("htmlTabl.tidyHtmlTable.maxCols", default = 5)),
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
  formatted_df <- tidyTableDataList %>%
    prBindDataListIntoColumns  %>%
    innerJoinByCommonCols(colRefTbl) %>%
    innerJoinByCommonCols(rowRefTbl) %>%
    dplyr::select(r_idx, c_idx, value) %>%
    dplyr::mutate_at(dplyr::vars(value), as.character) %>%
    # It is important to sort the rows as below or the data won't be properly
    # displayed, i.e. there will be primarily be a mismatch between columns
    dplyr::arrange(r_idx) %>%
    tidyr::pivot_wider(names_from = "c_idx") %>%
    dplyr::select(-r_idx)

  # Now order the columns so that cgroup and headers match
  formatted_df <- formatted_df[,order(colnames(formatted_df) %>% as.numeric())]

  # Get names and indices for row groups and tspanners
  htmlTable_args <- list(
    formatted_df, # Skip names for direct compatibility with Hmisc::latex
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

    rcnts <- prepGroupCounts(comp_val)
    htmlTable_args$rgroup <- rowRefTbl %>%
      dplyr::slice(rcnts$idx) %>%
      dplyr::pull(rgroup)

    htmlTable_args$n.rgroup <- rcnts$n
  }

  if (!missing(tspanner)) {
    tcnt <- prepGroupCounts(rowRefTbl %>% dplyr::pull(tspanner))
    htmlTable_args$tspanner <- tcnt$names
    htmlTable_args$n.tspanner <- tcnt$n
  }

  # Get names and indices for column groups
  if (!missing(cgroup)) {
    cg <- list(names = list(), n = list())
    noCgroup <- 1
    if (is.data.frame(tidyTableDataList$cgroup)) {
      noCgroup <- ncol(tidyTableDataList$cgroup)
    }

    for (colNo in 1:noCgroup) {
      counts <- prepGroupCounts(colRefTbl %>% dplyr::pull(colNo))
      cg$names[[colNo]] <- counts$names
      cg$n[[colNo]] <- counts$n
    }

    maxLen <- sapply(cg$names, length) %>% max
    for (colNo in 1:length(cg$names)) {
      missingNA <- maxLen - length(cg$names[[colNo]])
      if (missingNA > 0) {
        cg$names[[colNo]] <- c(cg$names[[colNo]], rep(NA, times = missingNA))
        cg$n[[colNo]] <- c(cg$n[[colNo]], rep(NA, times = missingNA))
      }
    }

    if (length(cg$names) == 1) {
      htmlTable_args$cgroup <- cg$names[[1]]
      htmlTable_args$n.cgroup <- cg$n[[1]]
    } else {
      htmlTable_args$cgroup <- do.call(rbind, cg$names)
      htmlTable_args$n.cgroup <- do.call(rbind, cg$n)
    }
  }

  if (!is.null(style_list)) {
    attr(htmlTable_args[[1]], style_attribute_name) <- style_list
  }
  do.call(table_fn, htmlTable_args)
}

`c_idx` <- "Fix no visible binding"
`r_idx` <- "Fix no visible binding"
