
#' Gets the CSS style element
#'
#' A function for checking, merging, and more
#' with a variety of different style formats.
#'
#' @param styles The styles can be provided as \code{vector},
#'  \code{named vector}, or \code{string}.
#' @param ... All styles here are merged with the first parameter.
#'  If you provide a name, e.g. \code{styles="background: blue", align="center"}
#'  the function will convert the \code{align} into proper \code{align: center}.
#' @return \code{string} Returns the codes merged into one string with
#'  correct CSS ; and : structure.
#' @keywords internal
#' @import magrittr
#' @family hidden helper functions for htmlTable
prGetStyle <- function(...) {
  mergeNames <- function(sv) {
    sv <- sv[!is.na(sv)]
    if (!is.null(names(sv))) {
      sv <-
        mapply(function(n, v) {
          if (n == "") {
            return(v)
          }
          paste0(n, ": ", v)
        }, n = names(sv), v = sv, USE.NAMES = FALSE)
    }
    return(sv)
  }
  spltNames <- function(sv) {
    ret_sv <- c()
    for (i in 1:length(sv)) {
      ret_sv <- c(
        ret_sv,
        # Split on the ; in case it is not at the end/start
        unlist(strsplit(sv[i], "\\b;(\\b|\\W+)", perl = TRUE))
      )
    }
    return(ret_sv)
  }

  styles <- c()
  dots <- list(...)
  dots <- dots[sapply(dots, function(x) any(!is.na(x) & !is.null(x)))]
  if (length(dots) == 0) {
    return("")
  }

  for (i in 1:length(dots)) {
    element <- dots[[i]]
    if (length(element) == 1) {
      if (element == "") {
        next
      }

      if (!grepl("\\b[:](\\b|\\W+)", element, perl = TRUE)) {
        if (!is.null(names(element))) {
          element <-
            paste0(names(element), ": ", element)
        } else if (!is.null(names(dots)) &&
          names(dots)[i] != "") {
          element <-
            paste0(names(dots)[i], ": ", element)
        } else if (element != "none") {
          stop(
            "The style should be formatted according to 'style_name: value'",
            " you have provided style '", element, "'"
          )
        }
      }
      styles %<>%
        c(element)
    } else {
      if (!is.null(names(element))) {
        element <- mergeNames(element)
      }

      styles <- c(
        styles,
        spltNames(element)
      )
    }
  }

  if (!all(grepl("^[^:]+:.+", styles))) {
    stop(
      "Invalid styles detected, one or more styles lack the needed style 'name: value': ",
      paste(paste0("'", styles[!grepl("^[^:]+:.+", styles)], "'"), collapse = ", ")
    )
  }

  # Remove empty background colors - sometimes a background color appears with
  #  just background-color:; for some unknown reason
  if (any(grepl("^background-color:( none|[ ]*;*$)", styles))) {
    styles <- styles[-grep("^background-color:( none|[ ]*;*$)", styles)]
  }

  # Merge background colors
  if (sum(grepl("^background-color:", styles)) > 1) {
    clrs <- styles[grep("^background-color:", styles)]
    clrs <- gsub("^background-color:[ ]*([^;]+);*", "\\1", clrs)
    clr <- prMergeClr(clrs)
    # Pick a color merge
    styles <- styles[-grep("^background-color:", styles)]
    styles <-
      c(
        styles,
        paste0("background-color: ", clr)
      )
  }

  style_names <- gsub("^([^:]+).+", "\\1", styles)
  if (!any(duplicated(style_names))) {
    unique_styles <- styles
  } else {
    # Only select the last style if two of the same type
    # exist. This in order to avoid any conflicts.
    unique_styles <- c()
    for (n in unique(style_names)) {
      unique_styles <-
        c(
          unique_styles,
          styles[max(which(n == style_names))]
        )
    }
  }

  unique_styles <- sapply(unique_styles, prAddSemicolon2StrEnd, USE.NAMES = FALSE)
  paste(unique_styles, collapse = " ")
}
