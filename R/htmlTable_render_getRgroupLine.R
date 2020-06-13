#' Gets the number of rgroup htmlLine
#'
#' @param total_columns The total number of columns including the rowlabel and the
#'  spacer cells
#' @param cspan The column span of the current rgroup
#' @param style The css style corresponding to the rgroup css style that includes
#'  the color specific for the rgroup, i.e. \code{col.rgroup}.
#' @param cgroup_spacer_cells The vector indicating the position of the cgroup
#'  spacer cells
#' @param prepped_row_css The css.cell information for this particular row.
#' @param rgroup_iterator An integer indicating the rgroup
#' @inheritParams htmlTable
#' @keywords internal
prGetRgroupLine <- function(x,
                            total_columns,
                            rgroup,
                            rgroup_iterator,
                            cspan,
                            rnames,
                            style,
                            cgroup_spacer_cells,
                            style_list,
                            prepped_row_css) {
  ret_str <- ""
  rgroup_elmnt <- rgroup[rgroup_iterator]
  add_elmnt <- prAttr4RgroupAdd(
    rgroup = rgroup,
    rgroup_iterator = rgroup_iterator,
    no_cols = ncol(x)
  )

  ## this will allow either css.rgroup or col.rgroup to
  ## color the rgroup label rows
  if (is.numeric(cspan) &&
    cspan < ncol(x) ||
    !is.null(add_elmnt)) {
    filler_cells <- rep("", ncol(x))

    if (!is.null(add_elmnt)) {
      if (!is.numeric(cspan)) {
        cspan <- ncol(x) + 1 * !prSkipRownames(rnames)
      }

      if (length(add_elmnt) > 1) {
        if (is.null(names(add_elmnt))) {
          stop(
            "The rgroup 'add' attribute element no '", rgroup_iterator, "'",
            " either be a single element or a named list/vector"
          )
        }

        add_pos <- as.integer(names(add_elmnt))
        if (any(is.na(add_pos)) ||
          any(add_pos < 1) ||
          any(add_pos > ncol(x))) {
          stop(
            "You have provided invalid element position for rgroup = '", rgroup_elmnt, "'",
            " the attribute seeems to be a list but the names are invalid",
            " '", paste(names(add_elmnt), collapse = "', '"), "'",
            " they should be integers between 1 and ", ncol(x)
          )
        }

        first_pos <- min(add_pos) - 1 + 1 * !prSkipRownames(rnames)
        if (missing(cspan)) {
          cspan <- first_pos
        } else {
          cspan <- min(
            cspan,
            first_pos
          )
        }

        for (ii in 1:length(add_pos)) {
          filler_cells[add_pos[ii]] <- add_elmnt[[ii]]
        }
      } else if (length(add_elmnt) == 1) {
        if (is.null(names(add_elmnt)) ||
          names(add_elmnt) == "last") {
          add_pos <- ncol(x)
        } else {
          add_pos <- as.integer(names(add_elmnt))
          if (is.na(add_pos) ||
            add_pos < 1 ||
            add_pos > ncol(x)) {
            stop(
              "You have provided invalid element position for rgroup = '", rgroup_elmnt, "'",
              " the attribute seeems to be a list but the name is invalid",
              " '", names(add_elmnt), "'",
              " it should be an integer between 1 and ", ncol(x)
            )
          }
        }

        first_pos <- add_pos - 1 + 1 * !prSkipRownames(rnames)
        if (missing(cspan)) {
          cspan <- first_pos
        } else {
          cspan <- min(
            cspan,
            first_pos
          )
        }

        filler_cells[add_pos] <- add_elmnt
      } else {
        stop(
          "The attribute to the rgroup '", rgroup_elmnt, "'",
          " does not have a length!"
        )
      }
    }

    true_span <- cspan +
      sum(cgroup_spacer_cells[0:(cspan -
        1 * !prSkipRownames(rnames))])
    ret_str %<>%
      sprintf(
        "%s\n\t<tr><td colspan='%d' style='%s'>%s</td>",
        .,
        true_span,
        prGetStyle(style),
        paste0(
          style_list$padding.tspanner,
          rgroup_elmnt
        )
      )


    cols_left <- ncol(x) - (cspan - 1 * !prSkipRownames(rnames))
    cell_str <- prAddCells(
      rowcells = filler_cells,
      cellcode = "td",
      style_list = style_list,
      style = style,
      cgroup_spacer_cells = cgroup_spacer_cells,
      has_rn_col = !prSkipRownames(rnames) * 1,
      offset = ncol(x) - cols_left + 1,
      prepped_cell_css = prepped_row_css
    )
    ret_str %<>%
      paste0(cell_str)


    ret_str %<>% paste0("</tr>")
  } else {
    ret_str %<>%
      sprintf(
        "%s\n\t<tr><td colspan='%d' style='%s'>%s</td></tr>",
        .,
        total_columns,
        prGetStyle(style),
        paste0(
          style_list$padding.tspanner,
          rgroup_elmnt
        )
      )
  }

  return(ret_str)
}
