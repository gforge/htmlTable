#' Generate an htmlTable using tidy data as input
#'
#' Builds an `htmlTable` by mapping columns from the input data, `x`,
#' to elements of an output `htmlTable` (e.g. `rnames`, `header`, etc.). This
#' provides a \pkg{ggplot2}-like interface you can pivot rows/columns as required. The
#' typical use case is when you are using `dplyr` together with the
#' `tidyverse` data processing functions, see `vignette("tidyHtmlTable")`.
#'
#' @section Column-mapping parameters:
#'
#'   The `tidyHtmlTable` function is designed to work like ggplot2 in that
#'   columns from `x` are mapped to specific parameters from the
#'   `htmlTable` function. At minimum, `x` must contain the names
#'   of columns mapping to `rnames`, `header`, and `rnames`.
#'   `header` and `rnames` retain the same meaning as in the
#'   htmlTable function. `value` contains the individual values that will
#'   be used to fill each cell within the output `htmlTable`.
#'
#'   A full list of parameters from `htmlTable` which may be mapped to
#'   columns within `x` include:
#'
#'   * `value`
#'   * `header`
#'   * `rnames`
#'   * `rgroup`
#'   * `cgroup`
#'   * `tspanner`
#'
#'   Also note that the coordinates of each `value` within `x` must be
#'   unambiguously mapped to a position within the output `htmlTable`.
#'   Therefore, the each row-wise combination the variables specified above
#'   contained in `x` must be unique.
#'
#' @section Sorting:
#'
#'   Sorting of rows is as of version 2.0 skipped as we may have situations with
#'   repeating inputs and this can easily be performed pre-function by calling
#'   [dplyr::arrange()] prior to `tidyHtmlTable`.
#'
#'   Columns are sorted by `arrange(cgroup, header)` where `cgroup` will be
#'   expanded to the columns of the `cgroup` argument, e.g. `cgroup = c(a, b), header = c`
#'   will become `arrange(a, b, c)`. If you want to sort in non-alphabetic order
#'   you can provide a `factor` variable and that information will be retained.
#'
#' @section Hidden values:
#'
#'   `htmlTable` Allows for some values within `rgroup`,
#'   `cgroup`, etc. to be specified as `""`. The following parameters
#'   allow for specific values to be treated as if they were a string of length
#'   zero in the `htmlTable` function.
#'
#'   * `hidden_rgroup`
#'   * `hidden_tspanner`
#'
#' @section Simple tibble output:
#'
#'  The tibble discourages the use of row names. There is therefore a convenience
#'  option for `tidyHtmlTable` where you can use the function just as you
#'  would with [htmlTable()] where `rnames` is populated with
#'  the `rnames` argument provided using `tidyselect` syntax (defaults to
#'  the "names" column if present int the input data).
#'
#' @section Additional dependencies:
#'
#'  In order to run this function you also must have \pkg{dplyr},
#'  \pkg{tidyr}, \pkg{tidyselect} and \pkg{purrr}
#'  packages installed. These have been removed due to
#'  the additional 20 Mb that these dependencies added (issue #47).
#'  *Note:* if you use \pkg{tidyverse} it will already have
#'  all of these and you do not need to worry.
#'
#' @section Row uniqueness:
#'
#' Usually each row should have a unique combination of `rnames`, `header`, `crgroup`, ...
#' Sometimes though rows come in a distinct order and the order identifies
#' the row more than the name. If we are identifying bone fractures using the
#' AO-classification we will have classes ranging in the form of:
#'
#' - A
#' - A1
#' - A1.1
#' - A2
#' - A2.1
#' - A2.2
#' - B
#' - ...
#'
#' we could therefore like to simplify the names to:
#'
#' - A
#' - .1
#' - ...1
#' - .2
#' - ...1
#' - ...2
#' - B
#' - ...
#'
#' And still retain the ability to follow what row corresponds to a given class. To
#' do this you need to set the parameter `skip_check_uniqueness` to `TRUE` or for all
#' tables just set the option at the beginning like this:
#'  `options("htmlTable.skip_check_uniqueness" = TRUE)`
#'
#' *Note* it is recommended that you verify with the full names just to make sure that
#' any unexpected row order change has happened in the underlying pivot functions.
#'
#' @param x Tidy data used to build the `htmlTable`
#' @param value The column containing values filling individual cells of the
#' output `htmlTable`. Defaults to "value" as used by [tidyr::pivot_longer()].
#' @param header The column in `x` specifying column headings
#' @param rnames The column in `x` specifying row names. Defaults to "name" as used by
#'  [tidyr::pivot_longer()].
#' @param rgroup The column in `x` specifying row groups
#' @param hidden_rgroup `strings` with `rgroup` values that will be hidden  (the values will
#'  still be there but the spanner will be set to "" and thus ignored by [htmlTable()]).
#' @param cgroup The column or columns in `x` specifying the column groups
#' @param tspanner The column in `x` specifying `tspanner` groups
#' @param hidden_tspanner `strings` with `tspanner` values that will be hidden (the values will
#'  still be there but the spanner will be set to "" and thus ignored by [htmlTable()]).
#' @param skip_removal_warning `boolean` suppress warning message when removing NA columns.
#' @param skip_check_uniqueness `boolean` skip the cheking of the uniqueness of a row as selected
#'  by the select statement. See section below on *Row uniqueness*.
#' @param table_fn The table function that should receive the input, defaults to [htmlTable()]
#'  but you can provide any function that uses the same input formatting. This package was inspired
#'  by the [Hmisc::latex()] function.
#' @param ... Additional arguments that will be passed to the inner
#' [htmlTable()] function
#' @return Returns html code that will build a pretty table
#' @export
#' @seealso [htmlTable()]
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
                          skip_check_uniqueness = getOption("htmlTable.skip_check_uniqueness", FALSE),
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
                                  skip_check_uniqueness = getOption("htmlTable.skip_check_uniqueness", FALSE),
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
                                     skip_check_uniqueness = getOption("htmlTable.skip_check_uniqueness", FALSE),
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
    args$x <- x %>% dplyr::select(-{{ orgName }})
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

  if (!skip_check_uniqueness) {
    checkUniqueness(tidyTableDataList)
  }


  tidyTableDataList %<>% removeRowsWithNA(skip_removal_warning = skip_removal_warning)

  # Create tables from which to gather row, column, and tspanner names
  # and indices
  rowRefTbl <- getRowTbl(tidyTableDataList)

  colRefTbl <- getColTbl(tidyTableDataList)

  # Format the values for display
  formatted_df <- tidyTableDataList %>%
    prBindDataListIntoColumns() %>%
    innerJoinByCommonCols(colRefTbl) %>%
    innerJoinByCommonCols(rowRefTbl) %>%
    dplyr::select(r_idx, c_idx, value) %>%
    dplyr::mutate_at(dplyr::vars(value), as.character) %>%
    # It is important to sort the rows as below or the data won't be properly
    # displayed, i.e. there will be primarily be a mismatch between columns
    dplyr::arrange(r_idx) %>%
    tidyr::pivot_wider(names_from = "c_idx") %>%
    dplyr::select(-r_idx)

  # Hide row groups specified in hidden_rgroup
  if (!missing(hidden_rgroup)) {
    rowRefTbl <- rowRefTbl %>%
      dplyr::mutate(rgroup = ifelse(rgroup %in% hidden_rgroup, "", rgroup))
  }

  # Hide tspanners specified in hidden_tspanner
  if (!missing(hidden_tspanner)) {
    rowRefTbl <- rowRefTbl %>%
      dplyr::mutate(tspanner = ifelse(tspanner %in% hidden_tspanner, "", tspanner))
  }

  # Now order the columns so that cgroup and headers match
  formatted_df <- formatted_df[, order(colnames(formatted_df) %>% as.numeric())]

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

    maxLen <- sapply(cg$names, length) %>% max()
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

  ret <- do.call(table_fn, htmlTable_args)
  attr(ret, "htmlTable_args") <- htmlTable_args
  return(ret)
}

`c_idx` <- "Fix no visible binding"
`r_idx` <- "Fix no visible binding"