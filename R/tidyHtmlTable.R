#' Generate an htmlTable using tidy data as input
#'
#' This function maps columns from the input data, `x`, to [htmlTable()] parameters.
#' It's designed to provide a fluent interface for those familiar with the `tidyverse` ecosystem.
#'
#' @param x Tidy data used to build the `htmlTable`
#' @param value Column containing values for individual table cells. Defaults to "value" (same as [tidyr::pivot_wider]).
#' @param header Column in `x` specifying column headings
#' @param rnames Column in `x` specifying row names. Defaults to "name" (same as [tidyr::pivot_wider()]).
#' @param rgroup Column in `x` specifying row groups.
#' @param hidden_rgroup Strings indicating `rgroup` values to be hidden.
#' @param cgroup Columns in `x` specifying the column groups.
#' @param tspanner Column in `x` specifying `tspanner` groups.
#' @param hidden_tspanner Strings indicating `tspanner` values to be hidden.
#' @param skip_removal_warning Boolean to suppress warnings when removing `NA` columns.
#' @param rnames_unique Designates unique row names when regular names lack uniqueness.
#' @param table_fn Function to format the table, defaults to [htmlTable()].
#' @param ... Additional arguments passed to [htmlTable()].
#'
#' @section Column-mapping:
#'
#' Columns from `x` are mapped (transformed) to specific parameters of the [htmlTable()]
#' The following columns are converted to match the intended input structure:
#'
#' * `value`
#' * `header`
#' * `rnames`
#' * `rgroup`
#' * `cgroup`
#' * `tspanner`
#'
#' Each combination of the variables in `x` should be unique to map correctly to the output table.
#'
#' @section Row uniqueness:
#'
#' Usually each row should have a unique combination of the mappers.
#' Sometimes though rows come in a distinct order and the order identifies
#' the row more than the name. E.g. if we are identifying bone fractures using the
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
#' we can simplify the names while retaining the key knowledge to:
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
#' This will though result in non-unique rows and thus we need to provide the original
#' names in addition to the `rnames` argument. To do this we have `rnames_unique` as a parameter,
#' without this `tidyHtmlTable` we risk unintended merging of cells, generating > 1 value per cell.
#'
#' *Note* it is recommended that you verify with the full names just to make sure that
#' any unexpected row order change has happened in the underlying pivot functions.
#'
#' @section Sorting:
#'
#' Rows can be pre-sorted using [dplyr::arrange()] before passing to `tidyHtmlTable`.
#' Column sorting is based on `arrange(cgroup, header)`. If you want to sort in non-alphabetic
#' order you can provide a factor variable and that information will be retained.
#'
#' @section Hidden values:
#'
#' `htmlTable` Allows for some values within `rgroup`,
#' `cgroup`, etc. to be specified as `""`. The following parameters
#' allow for specific values to be treated as if they were a string of length
#' zero in the `htmlTable` function.
#'
#' * `hidden_rgroup`
#' * `hidden_tspanner`
#'
#' @section Simple tibble output:
#'
#' The tibble discourages the use of row names. There is therefore a convenience
#' option for `tidyHtmlTable` where you can use the function just as you
#' would with [htmlTable()] where `rnames` is populated with
#' the `rnames` argument provided using `tidyselect` syntax (defaults to
#' the "names" column if present int the input data).
#'
#' @section Additional dependencies:
#'
#' In order to run this function you also must have \pkg{dplyr},
#' \pkg{tidyr}, \pkg{tidyselect} and \pkg{purrr}
#' packages installed. These have been removed due to
#' the additional 20 Mb that these dependencies added (issue #47).
#' *Note:* if you use \pkg{tidyverse} it will already have
#' all of these and you do not need to worry.
#'
#'
#' @return Returns the HTML code that, when rendered, displays a formatted table.
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
                          rnames_unique,
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
                                  rnames_unique,
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
                                     rnames_unique,
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
    rnames_unique = prAssertAndRetrieveValue(x, rnames_unique, optional = TRUE),
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