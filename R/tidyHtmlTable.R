#' Generate an htmlTable using a ggplot2-like interface
#'
#' Builds an \code{htmlTable} by mapping columns from the input data, \code{x},
#' to elements of an output \code{htmlTable} (e.g. rnames, header, etc.)
#'
#' @section Column-mapping parameters:
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
#'  In order to run this function you also must have \code{\link[dplyr]{dplyr}} and
#'  \code{\link[tidyr]{tidyr}} packages installed. These have been removed due to
#'  the additional 20 Mb that these dependencies added (issue #47). The particular
#'  functions required are:
#'
#'  \itemize{
#'    \item \code{\link[dplyr]{dplyr}}:
#'    \code{mutate_at},
#'    \code{select},
#'    \code{pull},
#'    \code{slice},
#'    \code{filter},
#'    \code{arrange_at},
#'    \code{mutate_if},
#'    \code{is.grouped_df},
#'    \code{left_join}
#'    \item \code{\link[tidyr]{tidyr}}: \code{spread}
#'  }
#'
#' @param x Tidy data used to build the \code{htmlTable}
#' @param value The column containing values filling individual cells of the
#' output \code{htmlTable}
#' @param header The column in \code{x} specifying column headings
#' @param rnames The column in \code{x} specifying row names
#' @param rgroup The column in \code{x} specifying row groups
#' @param hidden_rgroup rgroup values that will be hidden.
#' @param cgroup1 The column in \code{x} specifying the inner most column
#'  groups
#' @param cgroup2 The column in \code{x} specifying the outer most column
#'  groups
#' @param tspanner The column in \code{x} specifying tspanner groups
#' @param hidden_tspanner tspanner values that will be hidden.
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
tidyHtmlTable.default <- function(x, ...) {
  stop("x must be of class data.frame")
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
  # You need the suggested package for this function
  safeLoadPkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("The package ", pkg, " is needed for this function to work. Please install it.",
           call. = FALSE)
    }
  }
  safeLoadPkg("dplyr")
  safeLoadPkg("tidyr")

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
    select(to_select) %>%
    mutate_at(value, as.character) %>%
    # Spread will fill missing values (both explict and implicit) with the
    # same value, so we need to convert these values to a character if we want
    # them to show up correctly in the final table
    spread(key = "c_idx",
           value = value,
           fill = "")
  formatted_df$r_idx <- NULL

  # Get names and indices for row groups and tspanners
  htmlTable_args <- list(x = formatted_df,
                         rnames = row_ref_tbl %>% pull(rnames),
                         header = col_ref_tbl %>% pull(header),
                         ...)

  if (!is.null(rgroup)) {

    # This will take care of a problem in which adjacent row groups
    # with the same value will cause rgroup and tspanner collision
    comp_val <- row_ref_tbl %>% pull(rgroup)

    if (!is.null(tspanner)) {
      comp_val <- paste0(comp_val,
                         row_ref_tbl %>% pull(tspanner))
    }

    lens <- rle(comp_val)$lengths
    idx <- cumsum(lens)

    htmlTable_args$rgroup <- row_ref_tbl %>%
      slice(idx) %>%
      pull(rgroup)

    htmlTable_args$n.rgroup <- lens
  }

  if (!is.null(tspanner)) {
    htmlTable_args$tspanner <-
      rle(row_ref_tbl %>% pull(tspanner))$value
    htmlTable_args$n.tspanner <-
      rle(row_ref_tbl %>% pull(tspanner))$lengths
  }

  # Get names and indices for column groups
  if(!is.null(cgroup1)) {
    cgroup1_out <- rle(col_ref_tbl %>% pull(cgroup1))$value
    n.cgroup1 <- rle(col_ref_tbl %>% pull(cgroup1))$lengths
    if(!is.null(cgroup2)) {
      cgroup2_out <- rle(col_ref_tbl %>% pull(cgroup2))$value
      n.cgroup2 <- rle(col_ref_tbl %>% pull(cgroup2))$lengths
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

  do.call(htmlTable, htmlTable_args)
}

# Removes rows containing NA values in any mapped columns from the tidy dataset
remove_na_rows <- function(x, ...) {
  cols <- as.character(get_col_vars(...))
  na.log <- x %>%
    select(cols) %>%
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
  return(x %>% filter(keep.idx))
}

# This checks to make sure that the mapping columns of the tidy dataset
# uniquely specify a given value
check_uniqueness <- function(x, ...) {
  # Get arguments
  args <- simplify_arg_list(...)
  cols <- as.character(args)
  dupes <- x %>%
    select(cols) %>%
    duplicated
  if (sum(dupes) != 0) {

    stop(paste0("The input parameters ",
                paste(paste0("\"", names(args), "\""), collapse = ", "),
                " do not specify unique rows. The following rows ",
                "are duplicated: ",
                paste(which(dupes), collapse = ", ")))
  }
}

# Converts arguments from ... into a list and removes those that have been set
# to NULL
simplify_arg_list <- function(...) {
  x <- list(...)
  idx <- sapply(x, is.null)
  return(x[!idx])
}

# This function gets arguments from ..., removes those that are NULL,
# and then subsets those that should map tidy data columns to htmlTable
# parameters
get_col_vars <- function(...) {
  out <- simplify_arg_list(...)
  return(out[names(out) %in%
               c("value", "header",
                 "rnames", "rgroup",
                 "cgroup1", "cgroup2",
                 "tspanner")])
}

# Checks a variety of assumptions about input arguments and prepares an
# appropriate error message if those assumptions are violated
argument_checker <- function(x, ...) {

  # Check if x is a grouped tbl_df
  if(is.grouped_df(x)) {
    stop("x cannot be a grouped_df")
  }

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
    select(cols) %>%
    unique %>%
    arrange_at(cols) %>%
    # This is necessary in order to not generate NA values when setting
    # hidden elements to ""
    mutate_if(is.factor, as.character)

  out$c_idx <- 1:nrow(out)
  return(out)
}

get_row_tbl <- function(x,
                        rnames,
                        rgroup = NULL,
                        tspanner = NULL) {

  cols <- c(tspanner, rgroup, rnames)

  out <- x %>%
    select(cols) %>%
    unique %>%
    arrange_at(cols) %>%
    # This is necessary in order to not generate NA values when setting
    # hidden elements to ""
    mutate_if(is.factor, as.character)

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

  out <- suppressWarnings(
    x %>%
      left_join(col_idx_df, cols)
  )
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

  out <- suppressWarnings(
    x %>%
      left_join(row_idx_df, by = cols)
  )
  return(out)
}
