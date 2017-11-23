#' Generate an htmlTable using a ggplot2-like interface
#'
#' A wrapper script for \code{htmlTable} that will build a table using
#' tidy data and mapping elements of the table to specific columns of the input
#' data.
#'
#' @param x Tidy data used to build the table
#' @param value The individual values which will fill each cell of the
#' table
#' @param header The column in \code{x} specifying column headings
#' @param rnames The column in \code{x} specifying row names
#' @param rgroup The column in \code{x} specifying row groups
#' @param hidden_rgroup rgroups that will be hidden.
#' @param cgroup1 The column in \code{x} specifying the inner most column
#'  groups
#' @param cgroup2 The column in \code{x} specifying the outer most column
#'  groups
#' @param tspanner The column in \code{x} specifying tspanner groups
#' @param hidden_tspanner tspanners that will be hidden.
#' @param ... Additional arguments that will be passed to the inner
#' \code{htmlTable} function
#' @return Returns html code that will build a pretty table
#' @export
#' @seealso \code{\link{htmlTable}}
#' @examples
#' \dontrun{
#' library(tidyverse)
#' mtcars %>%
#'     rownames_to_column %>%
#'     select(rowname, cyl, gear, hp, mpg, qsec) %>%
#'     gather(per_metric, value, hp, mpg, qsec) %>%
#'     group_by(cyl, gear, per_metric) %>%
#'     summarise(Mean = round(mean(value), 1),
#'               SD = round(sd(value), 1),
#'               Min = round(min(value), 1),
#'               Max = round(max(value), 1)) %>%
#'      gather(summary_stat, value, Mean, SD, Min, Max) %>%
#'      ungroup %>%
#'      mutate(gear = paste(gear, "Gears"),
#'             cyl = paste(cyl, "Cylinders")) %>%
#'      tidyHtmlTable(header = "gear",
#'                   cgroup1 = "cyl",
#'                   cell_value = "value",
#'                   rnames = "summary_stat",
#'                   rgroup = "per_metric")
#' }
tidyHtmlTable <- function(x,
                          value = "value",
                          header = "header",
                          rnames = "rnames",
                          rgroup = NULL,
                          hidden_rgroup = NULL,
                          cgroup1 = NULL,
                          cgroup2 = NULL,
                          tspanner = NULL,
                          hidden_tspanner = NULL,
                          ...) {
  UseMethod("tidyHtmlTable")
}

#' @export
tidyHtmlTable.data.frame <- function(x,
                                     value = "value",
                                     header = "header",
                                     rnames = "rnames",
                                     rgroup = NULL,
                                     hidden_rgroup = NULL,
                                     cgroup1 = NULL,
                                     cgroup2 = NULL,
                                     tspanner = NULL,
                                     hidden_tspanner = NULL,
                                     ...) {

  argument_checker(x,
                   value = value,
                   header = header,
                   rnames = rnames,
                   rgroup = rgroup,
                   hidden_rgroup = NULL,
                   cgroup1 = cgroup1,
                   cgroup2 = cgroup2,
                   tspanner = tspanner,
                   hidden_tspanner = NULL)

  check_uniqueness(x,
                   header = header,
                   rnames = rnames,
                   rgroup = rgroup,
                   cgroup1 = cgroup1,
                   cgroup2 = cgroup2,
                   tspanner = tspanner)

  x <- remove_na_rows(x,
                      header = header,
                      rnames = rnames,
                      rgroup = rgroup,
                      cgroup1 = cgroup1,
                      cgroup2 = cgroup2,
                      tspanner = tspanner)

  # Create tables from which to gather row, column, and tspanner names
  # and indices
  row_ref_tbl <- x %>%
    get_row_tbl(rnames = rnames,
                rgroup = rgroup,
                tspanner = tspanner)

  # Hide row groups specified in hidden_rgroup
  if (!(is.null(hidden_rgroup))) {

    row_ref_tbl <- row_ref_tbl %>%
      mutate_at(rgroup,
                function(x){ifelse(x %in% hidden_rgroup, "", x)})
  }

  # Hide tspanners specified in hidden_tspanner
  if (!(is.null(hidden_tspanner))) {

    row_ref_tbl <- row_ref_tbl %>%
      mutate_at(tspanner,
                function(x){ifelse(x %in% hidden_tspanner, "", x)})
  }

  col_ref_tbl <- x %>%
    get_col_tbl(header = header,
                cgroup1 = cgroup1,
                cgroup2 = cgroup2)

  # Format the values for display
  to_select <- c("r_idx", "c_idx", value)

  formatted_df <- x %>%
    add_col_idx(header = header,
                cgroup1 = cgroup1,
                cgroup2 = cgroup2) %>%
    add_row_idx(rnames = rnames,
                rgroup = rgroup,
                tspanner = tspanner) %>%
    dplyr::select(to_select) %>%
    dplyr::mutate_at(value, as.character) %>%
    # Spread will fill missing values (both explict and implicit) with the
    # same value, so we need to convert these values to a character if we want
    # them to show up correctly in the final table
    tidyr::spread(key = c_idx,
                  value = value,
                  fill = "") %>%
    dplyr::select(-r_idx)

  # Get names and indices for row groups and tspanners
  htmlTable_args <- list(x = formatted_df,
                         rnames = row_ref_tbl %>% dplyr::pull(rnames),
                         header = col_ref_tbl %>% dplyr::pull(header),
                         ...)

  if (!is.null(rgroup)) {

    # This will take care of a problem in which adjacent row groups
    # with the same value will cause rgroup and tspanner collision
    comp_val <- row_ref_tbl %>% dplyr::pull(rgroup)

    if (!is.null(tspanner)) {
      comp_val <- paste0(comp_val,
                         row_ref_tbl %>% dplyr::pull(tspanner))
    }

    lens <- rle(comp_val)$lengths
    idx <- cumsum(lens)

    htmlTable_args$rgroup <- row_ref_tbl %>%
      dplyr::slice(idx) %>%
      dplyr::pull(rgroup)

    htmlTable_args$n.rgroup <- lens
  }

  if (!is.null(tspanner)) {
    htmlTable_args$tspanner <-
      rle(row_ref_tbl %>% dplyr::pull(tspanner))$value
    htmlTable_args$n.tspanner <-
      rle(row_ref_tbl %>% dplyr::pull(tspanner))$lengths
  }

  # Get names and indices for column groups
  if(!is.null(cgroup1)) {
    cgroup1_out <- rle(col_ref_tbl %>% dplyr::pull(cgroup1))$value
    n.cgroup1 <- rle(col_ref_tbl %>% dplyr::pull(cgroup1))$lengths
    if(!is.null(cgroup2)) {
      cgroup2_out <- rle(col_ref_tbl %>% dplyr::pull(cgroup2))$value
      n.cgroup2 <- rle(col_ref_tbl %>% dplyr::pull(cgroup2))$lengths
      len_diff <- length(cgroup1_out) - length(cgroup2_out)
      if (len_diff < 0) {
        stop("cgroup2 cannot contain more categories than cgroup1")
      } else if (len_diff > 0) {
        cgroup2_out <- c(cgroup2, rep(NA, len_diff))
        n.cgroup2 <- c(n.cgroup2, rep(NA, len_diff))
      }
      cgroup1_out <- rbind(cgroup2, cgroup1)
      n.cgroup1 <- rbind(n.cgroup2, n.cgroup1)
    }
    htmlTable_args$cgroup <- cgroup1_out
    htmlTable_args$n.cgroup <- n.cgroup1
  }

  do.call(htmlTable::htmlTable, htmlTable_args)
}

remove_na_rows <- function(x, ...) {
  cols <- as.character(get_col_vars(...))
  na.log <- x %>%
    dplyr::select(cols) %>%
    is.na

  na.row.sums <- na.log %>%
    rowSums

  keep.idx <- na.row.sums == 0
  removed <- sum(na.row.sums > 0)

  if (removed != 0) {
    na.col.sums <- na.log %>%
      colSums
    na.cols <- colnames(na.log)[na.col.sums > 0]
    warning(paste0("NA values were detected in the following columns of ",
                   "the tidy dataset: ",
                   paste(na.cols, collapse = ", "), ". ",
                   removed, " row(s) in the tidy dataset were removed."))
  }
  return(x[keep.idx,])
}

check_uniqueness <- function(x, ...) {
  # Get arguments
  args <- simplify_arg_list(...)
  cols <- as.character(args)
  dupes <- x %>%
    dplyr::select(cols) %>%
    duplicated
  if (sum(dupes) != 0) {

    stop(paste0("The input parameters ",
                paste(paste0("\"", names(args), "\""), collapse = ", "),
                " do not specify unique rows. The following rows ",
                "are duplicated: ",
                paste(which(dupes), collapse = ", ")))
  }
}

simplify_arg_list <- function(...) {
  x <- list(...)
  idx <- sapply(x, is.null)
  return(x[!idx])
}

get_col_vars <- function(...) {
  out <- simplify_arg_list(...)
  return(out[names(out) %in%
               c("value", "header",
                 "rnames", "rgroup",
                 "cgroup1", "cgroup2",
                 "tspanner")])
}

argument_checker <- function(x, ...) {

  # Check that all the input are characters
  all_args <- simplify_arg_list(...)
  idx <- which(!sapply(all_args, is.character))

  if (length(idx) > 0) {
    stop("The following parameters must be of type character: ",
         paste(names(all_args)[idx], collapse = ", "))
  }

  # Check that all of the arguments that would be used map columns to
  # character attributes are of length 1
  col_vars <- get_col_vars(...)

  idx <- which(sapply(col_vars, length) > 1)
  if (length(idx) > 0) {
    stop("The following parameters must be of length 1: ",
         paste(names(col_vars)[idx], collapse = ", "))
  }

  # Find column variables that are not columns in the dataset
  idx <- which(!(as.character(col_vars) %in% colnames(x)))
  if (length(idx) > 0) {
    stop("The following arguments need values that correspond to column ",
         "names in x: ",
         paste0(names(col_vars), " = ",
                as.character(col_vars),
                collapse = ", "))
  }
}

get_col_tbl <- function(x,
                        header,
                        cgroup1 = NULL,
                        cgroup2 = NULL) {

  cols <- c(cgroup2, cgroup1, header)

  out <- x %>%
    dplyr::select(cols) %>%
    unique %>%
    dplyr::arrange_(.dots = cols) %>%
    # This is necessary in order to not generate NA values when setting
    # hidden elements to ""
    dplyr::mutate_if(is.factor, as.character)

  out$c_idx <- 1:nrow(out)

  return(out)
}

get_row_tbl <- function(x,
                        rnames,
                        rgroup = NULL,
                        tspanner = NULL) {

  cols <- c(tspanner, rgroup, rnames)

  out <- x %>%
    dplyr::select(cols) %>%
    unique %>%
    dplyr::arrange_(.dots = cols) %>%
    # This is necessary in order to not generate NA values when setting
    # hidden elements to ""
    dplyr::mutate_if(is.factor, as.character)

  out$r_idx <- 1:nrow(out)

  return(out)
}

add_col_idx <- function(x,
                        header,
                        cgroup1 = NULL,
                        cgroup2 = NULL) {

  cols <- c(cgroup2, cgroup1, header)

  col_idx_df <- x %>%
    get_col_tbl(header = header,
                cgroup1 = cgroup1,
                cgroup2 = cgroup2)

  out <- x %>%
    dplyr::left_join(col_idx_df, cols)

  return(out)
}

add_row_idx <- function(x,
                        rnames,
                        rgroup = NULL,
                        tspanner = NULL) {

  cols <- c(tspanner, rgroup, rnames)

  row_idx_df <- x %>%
    get_row_tbl(rnames = rnames,
                rgroup = rgroup,
                tspanner = tspanner)

  out <- x %>%
    dplyr::left_join(row_idx_df, by = cols)

  return(out)
}
