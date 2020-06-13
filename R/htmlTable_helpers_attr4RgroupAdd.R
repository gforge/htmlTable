#' Get the add attribute element
#'
#' Gets the add element attribute if it exists. If non-existant it will
#' return NULL.
#'
#' @param rgroup_iterator The rgroup number of interest
#' @param no_cols The \code{ncol(x)} of the core htmlTable x argument
#' @inheritParams htmlTable
#' @keywords internal
#' @importFrom stats na.omit
prAttr4RgroupAdd <- function(rgroup, rgroup_iterator, no_cols) {
  if (is.null(attr(rgroup, "add"))) {
    return(NULL)
  }

  add_elmnt <- attr(rgroup, "add")
  if (is.null(names(add_elmnt))) {
    if (is.null(dim(add_elmnt)) &&
      length(add_elmnt) == sum(rgroup != "")) {
      if (!is.list(add_elmnt)) {
        add_elmnt <- as.list(add_elmnt)
      }
      names(add_elmnt) <- (1:length(rgroup))[rgroup != ""]
    } else if (!is.null(dim(add_elmnt)) &&
      ncol(add_elmnt) %in% c(1, no_cols)) {

      # Convert matrix to stricter format
      tmp <- list()
      for (i in 1:nrow(add_elmnt)) {
        if (ncol(add_elmnt) == 1) {
          tmp[[i]] <- add_elmnt[i, ]
        } else {
          tmp2 <- as.list(add_elmnt[i, ])
          names(tmp2) <- 1:no_cols
          tmp[[i]] <- tmp2
        }
      }
      if (nrow(add_elmnt) == sum(rgroup != "")) {
        names(tmp) <- (1:length(rgroup))[rgroup != ""]
      } else if (!is.null(rownames(add_elmnt))) {
        names(tmp) <- rownames(add_elmnt)
      } else {
        stop(
          "You have provided a matrix as the
             add attribute to rgroups without rows that either
             match the number of rgroups available '", length(rgroup[rgroup != ""]), "'",
          " (you provided '", nrow(add_elmnt), "' rows).",
          " And you also failed to have rownames."
        )
      }
      add_elmnt <- tmp
    } else {
      stop(
        "The length of the rgroup 'add' attribute must either match",
        " (1) the length of the rgroup",
        " (2) or have names corresponding to the mapping integers"
      )
    }
  }

  if (!is.list(add_elmnt) &&
    !is.vector(add_elmnt)) {
    stop("The rgroup mus either be a list or a vector")
  }

  add_pos <- ifelse(grepl(
    "^[123456789][0-9]*$",
    names(add_elmnt)
  ),
  as.integer(names(add_elmnt)),
  NA
  )
  if (any(is.na(add_pos))) {
    # Look for rgroup names that match to those not
    # found through the integer match
    # If found the number is assigned to the add_pos
    available_rgroups <- rgroup
    if (!all(is.na(add_pos))) {
      available_rgroups <- available_rgroups[-na.omit(add_pos)]
    }
    for (missing_pos in which(is.na(add_pos))) {
      row_label <- names(add_elmnt)
      if (row_label %in% available_rgroups) {
        available_rgroups <-
          available_rgroups[available_rgroups != row_label]
        pos <- which(rgroup == row_label)
        if (length(pos) > 1) {
          stop(
            "There seem to be two identical row groups ('", row_label, "')",
            " that you whish to assign a add columns to through the 'add'",
            " attribute for the rgroup element."
          )
        } else {
          add_pos[missing_pos] <- pos
        }
      }
    }
    if (any(is.na(add_pos))) {
      failed_elements <- paste(names(add_elmnt)[is.na(add_pos)], collapse = "', '")
      available <- paste(rgroup, collapse = "', '")
      stop(
        "Failed to find matchin rgroup elements for: ",
        "'", failed_elements, "'",
        " from availabel rgroups: ",
        "'", available, "'"
      )
    }
    names(add_elmnt) <- add_pos
  }

  if (!is.list(add_elmnt)) {
    add_elmnt <- as.list(add_elmnt)
  }

  if (any(add_pos < 1)) {
    stop("The rgroup 'add' attribute cannot have integer names below 1")
  }

  if (any(!add_pos <= length(rgroup)) || any(rgroup[add_pos] == "")) {
    no_rgroups_empty <- paste(which(rgroup == ""), collapse = ", ")
    prob_positions <- paste(add_pos[add_pos > length(rgroup) | add_pos %in% which(rgroup == "")], collapse = "', '")
    stop(
      "The rgroup 'add' attribute cannot have integer names indicating",
      " positions larger than the length of the rgroup",
      " (=", length(rgroup), ") or matches",
      " one of the empty groups (no. ", no_rgroups_empty, ").",
      " The problematic position(s):",
      " '", prob_positions, "'"
    )
  }

  # Return the matching iterator
  if (rgroup_iterator %in% names(add_elmnt)) {
    return(add_elmnt[[as.character(rgroup_iterator)]])
  }

  return(NULL)
}
