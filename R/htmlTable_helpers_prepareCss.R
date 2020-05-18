#' Prepares the cell style
#'
#' @param css The CSS styles that are to be converted into
#'  a matrix.
#' @param name The name of the CSS style that is prepared
#' @inheritParams htmlTable
#' @return \code{matrix}
#' @keywords internal
prPrepareCss <- function(x, css, rnames, header, name = deparse(substitute(css))){
  css.header <- rep("", times = ncol(x))
  css.rnames <- rep("", times = nrow(x) + !missing(header))

  if (is.matrix(css)){
    if (any(grepl("^[^:]*[a-zA-Z]+[:]*:", css))){
      rownames(css) <- NULL
      colnames(css) <- NULL
    }
    if (ncol(css) == ncol(x) + 1 &&
          !prSkipRownames(rnames)){
      if (!missing(header)){
        if (nrow(css) == nrow(x) + 1){
          css.rnames <- css[,1]
        }else if(nrow(css) == nrow(x)){
          css.rnames[2:length(css.rnames)] <- css[,1]
        }else{
          stop("There is an invalid number of rows for the ", name ," matrix.",
               " Your x argument has '", nrow(x), "' rows",
               " while your ", name ," has '", nrow(css), "' rows",
               " and there is a header")
        }
      }else if(nrow(x) == nrow(css)){
        css.rnames <- css[,1]
      }else{
        stop("There is an invalid number of rows for the ", name ," matrix.",
             " Your x argument has '", nrow(x), "' rows",
             " while your ", name ," has '", nrow(css), "' rows",
             " (there is no header)")
      }

      css <-
        css[,-1,drop=FALSE]
    }else if (ncol(css) != ncol(x)){
      stop("There is an invalid number of columns for the ", name ," matrix.",
           " Your x argument has '", ncol(x), "' columns",
           " while your ", name ," has '", ncol(css), "' columns",
           " and there are ", ifelse(prSkipRownames(rnames),
                                     "no", ""),
           " rownames.")
    }

    if (nrow(css) == nrow(x) + 1 &&
          !missing(header)){
      css.header <- css[1,]
      css <- css[-1,,drop=FALSE]
    }else if(nrow(css) != nrow(x)){
      stop("There is an invalid number of rows for the ", name ," matrix.",
           " Your x argument has '", nrow(x), "' rows",
           " while your ", name ," has '", nrow(css), "' rows",
           " and there is ", ifelse(missing(header),
                                    "no", "a"),
           " header")
    }
  }else if(is.vector(css)){
    if (length(css) == ncol(x) + 1){
      css.rnames = rep(css[1], nrow(x) + prSkipRownames(rnames))
      css <-
        css[-1]
    } else if (length(css) == 1) {
      css.rnames <- rep(css, times = nrow(x) + !missing(header))
    } else if(length(css) != ncol(x)) {
      stop("The length of your ", name ," vector '", length(css) ,"'",
           " does not correspond to the column length '", ncol(x) ,"'",
           " (there are ", ifelse(prSkipRownames(rnames),
                                    "no", ""),
           " rownames)")
    }

    css <- matrix(css,
                  nrow=nrow(x),
                  ncol=ncol(x),
                  byrow = TRUE)
  }

  return(structure(css,
                   rnames = css.rnames,
                   header = css.header,
                   class=class(css)))
}