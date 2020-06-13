
#' Prepares the cgroup argument
#'
#' Due to the complicated structure of multilevel cgroups there
#' some preparation for the cgroup options is required.
#'
#' @inheritParams htmlTable
#' @return \code{list(cgroup, n.cgroup, align.cgroup, cgroup_spacer_cells)}
#' @keywords internal
#' @family hidden helper functions for htmlTable
prPrepareCgroup <- function(x, cgroup, n.cgroup, style_list){
  cgroup_spacer_cells <- rep(0, times=(ncol(x)-1))

  # The cgroup is by for compatibility reasons handled as a matrix
  if (is.list(cgroup)) {
    if (!is.list(n.cgroup)) stop("If cgroup is a list then so must n.cgroup")
    if (length(n.cgroup) != length(cgroup)) stop("Different length of cgroup and n.cgroup")
    if (!all(sapply(cgroup, is.vector))) stop("The cgroup list consist of vectors")

    lengths <- sapply(n.cgroup, sum)
    if (any(is.na(lengths))) stop("The cgroup has invalid lengths!")
    for (i in 1:length(cgroup)) {
      if (length(cgroup[[i]]) != length(n.cgroup[[i]]))
        stop("The cgroup and n.cgroup elemennt's lengths don't match for the ", i, "th element")
    }

    ncols <- max(lengths, na.rm=TRUE)
    if (any(sapply(lengths, function(l) ncol(x) %% l != 0))) {
      stop("Invalid size of lists: ", vector2string(lengths),
           " each element should be be able to evenly divide ", ncol(x))
    }
    cgroup_mtrx <- matrix(nrow = length(cgroup), ncol = ncols)
    n.cgroup_mtrx <- matrix(nrow = length(cgroup), ncol = ncols)
    for (i in 1:length(cgroup)) {
      for (ii in 1:length(cgroup[[i]])) {
        cgroup_mtrx[i, ii] <- cgroup[[i]][ii]
        n.cgroup_mtrx[i, ii] <- n.cgroup[[i]][ii]
      }
    }
    cgroup <- cgroup_mtrx
    n.cgroup <- n.cgroup_mtrx
  } else if (!is.matrix(cgroup)){
    cgroup <- matrix(cgroup, nrow=1)
    if (missing(n.cgroup))
      n.cgroup <- matrix(NA, nrow=1)
    else{
      if (any(n.cgroup < 1)){
        warning("You have provided cgroups with less than 1 element,",
                " these will therefore be removed: ",
                paste(sprintf("'%s' = %d", cgroup, n.cgroup)[n.cgroup < 1],
                      collapse=", "))
        cgroup <- cgroup[,n.cgroup >= 1, drop=FALSE]
        n.cgroup <- n.cgroup[n.cgroup >= 1]
      }

      if (ncol(cgroup) != length(n.cgroup)){
        n.cgroup <- n.cgroup[n.cgroup > 0]
        if (ncol(cgroup) < length(n.cgroup))
          stop("You have provided too many n.cgroup,",
               " it should have the same length or one less than the cgroup (", ncol(cgroup), ")",
               " but it has the length of ", length(n.cgroup))
        if (ncol(cgroup) - 1 < length(n.cgroup))
          stop("You have provided too few n.cgroup,",
               " it should have the ate the length or one less than the cgroup (", ncol(cgroup), ")",
               " but it has the length of ", length(n.cgroup))
        if (ncol(cgroup) - 1 == length(n.cgroup))
          n.cgroup <- c(n.cgroup, ncol(x) - sum(n.cgroup))


      }
      n.cgroup <- matrix(n.cgroup, nrow=1)
    }
  }else if(missing(n.cgroup)){
    stop("If you specify the cgroup argument as a matrix you have to",
         " at the same time specify the n.cgroup argument.")
  }

  # Go bottom up as the n.cgroup can be based on the previous
  # n.cgroup row.
  for (i in nrow(cgroup):1){
    # The row is empty and filled with NA's then we check
    # that it is possible to evenly split the cgroups among
    # the columns of the table
    if (all(is.na(n.cgroup[i,])) &&
          ncol(x) %% length(cgroup[i,]) == 0){
      # This generates the n.cgroup if this is missing
      n.cgroup[i,] <- rep(ncol(x)/length(cgroup[i,]), times=length(cgroup[i,]))
    }else if(any(n.cgroup[i,!is.na(n.cgroup[i,])] < 1)){
      stop("You have in n.cgroup row no ", i, " cell(s) with < 1")

    }else if(sum(n.cgroup[i,], na.rm=TRUE) != ncol(x)){
      ncgroupFixFromBelowGroup <- function(nc, i){
        if (i+1 > nrow(nc))
          stop("You have provided an invalid nc",
               " where it has fewer rows than the one of interest")

        # Select those below that are not missing
        row_below <- nc[i + 1, !is.na(nc[i + 1, ])]
        # The first position to start
        start_pos <- 1
        # This is a slightly complicated run that took a while to figure out
        # and I'm still afraid of ever having to debug this section.
        for (ii in 1:ncol(nc)){
          if (!is.na(nc[i, ii])){
            # Need to find where to begin tha addition
            pos <- ifelse(any(start_pos > cumsum(row_below)),
                          tail(which(start_pos > cumsum(row_below)), 1) + 1,
                          1)
            # Change the value to the rows below values that add up to this row
            # if the nc value is 1 and start position is 1 -> 1:(1+1-1) -> 1:1 -> 1
            # if the nc value is 2 and start position is 2 -> 2:(2+2-1) -> 2:3
            # if the nc value is 2 and start position is 1 -> 1:(1+2-1) -> 1:2
            nc[i, ii] <- sum(row_below[pos:(pos + nc[i, ii] - 1)])
            # Update the new start position:
            # if first run and nc is 2 then 1 + 2 -> 3 i.e.
            # next run the start_pos is 3 and lets say that nc is 3 then 3 + 3 -> 6
            start_pos <- start_pos + nc[i, ii]
          }
        }

        # Return the full object
        return(nc)
      }

      # This grouping can be based upon the next row
      if (i < nrow(cgroup) &&
            sum(n.cgroup[i, ], na.rm = TRUE) == sum(!is.na(n.cgroup[i + 1, ])))
      {
        n.cgroup <- ncgroupFixFromBelowGroup(n.cgroup, i)
      }else{
        stop(sprintf("Your columns don't match in the n.cgroup for the %d header row, i.e. %d != %d",
                     i,
                     sum(n.cgroup[i,], na.rm=TRUE),
                     ncol(x)))
      }
    }

    if (!all(is.na(n.cgroup[i, ]) == is.na(cgroup[i, ]))){
      stop("On header row (the cgroup argument) no ", i,
           " you fail to get the NA's matching.",
           "\n  The n.cgroup has elements no:",
           sprintf(" '%s'", paste(which(is.na(n.cgroup[i, ])), collapse=", ")),
           " missing while cgroup has elements no:",
           sprintf(" '%s'", paste(which(is.na(cgroup[i, ])), collapse=", ")),
           " missing.",
           "\n If the NA's don't occur at the same point",
           " the software can't decide what belongs where.",
           "\n The full cgroup row: ", paste(cgroup[i, ], collapse=", "),
           "\n The full n.cgroup row: ", paste(n.cgroup[i, ], collapse=", "),
           "\n Example: for a two row cgroup it would be:",
           " n.cgroup = rbind(c(1, NA), c(2, 1)) and",
           " cgroup = rbind(c('a', NA), c('b', 'c'))")
    }

    # Add a spacer cell for each cgroup. If two cgroups
    # on different rows have the same separation then it
    # is enough to have one spacer.
    for (ii in 1:(length(n.cgroup[i, ])-1)){
      if (!is.na(n.cgroup[i, ii]) && sum(n.cgroup[i, 1:ii], na.rm=TRUE) <= length(cgroup_spacer_cells))
        cgroup_spacer_cells[sum(n.cgroup[i, 1:ii], na.rm=TRUE)] <- 1
    }
  }

  # Get alignment
  if (is.null(style_list$align.cgroup)) {
    style_list$align.cgroup <- apply(n.cgroup, 1,
                          function(nc) paste(rep("c", times=sum(!is.na(nc))), collapse=""))
    style_list$align.cgroup <- matrix(style_list$align.cgroup,
                           ncol = 1)
  } else {
    if (NROW(style_list$align.cgroup) != nrow(n.cgroup))
      stop("You have different dimensions for your style_list$align.cgroup and your cgroups, ",
           NROW(style_list$align.cgroup), " (just) !=", nrow(n.cgroup), " (n.cgroup)")

    # An old leftover behaviour from the latex() function
    if (NCOL(style_list$align.cgroup) > 1)
      style_list$align.cgroup <- apply(style_list$align.cgroup, 1,
                                       function(x) paste(ifelse(is.na(x), "", x), collapse=""))

    style_list$align.cgroup <- mapply(prPrepareAlign,
                           align = style_list$align.cgroup,
                           x = apply(n.cgroup, 1, function(nc) sum(!is.na(nc))),
                           rnames=FALSE)

    style_list$align.cgroup <- matrix(style_list$align.cgroup, ncol=1)
  }

  style_list$css.cgroup <- prPrepareCss(x = cgroup, css = style_list$css.cgroup)
  return(list(cgroup = cgroup,
              n.cgroup = n.cgroup,
              align.cgroup = style_list$align.cgroup,
              cgroup_spacer_cells = cgroup_spacer_cells,
              css.cgroup = style_list$css.cgroup))
}
