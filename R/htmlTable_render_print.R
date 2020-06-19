#' @rdname htmlTable
#' @param useViewer If you are using RStudio there is a viewer thar can render
#'  the table within that is envoced if in \code{\link[base]{interactive}} mode.
#'  Set this to \code{FALSE} if you want to remove that  functionality. You can
#'  also force the function to call a specific viewer by setting this to a
#'  viewer function, e.g. \code{useViewer = utils::browseURL} if you want to
#'  override the default RStudio viewer. Another option that does the same is to
#'  set the \code{options(viewer=utils::browseURL)} and it will default to that
#'  particular viewer (this is how RStudio decides on a viewer).
#'  \emph{Note:} If you want to force all output to go through the
#'  \code{\link[base]{cat}()} the set \code{\link[base]{options}(htmlTable.cat = TRUE)}.
#' @export
#' @importFrom utils browseURL
print.htmlTable <- function(x, useViewer, ...) {
  args <- attr(x, "...")
  # Use the latest ... from the print call
  # and override the original htmlTable call ...
  # if there is a conflict
  print_args <- list(...)
  for (n in names(print_args)) {
    args[[n]] <- print_args[[n]]
  }

  # Since the print may be called from another print function
  # it may be handy to allow functions to use attributes for the
  # useViewer parameter
  if (missing(useViewer)) {
    if ("useViewer" %in% names(args) &&
      (is.logical(args$useViewer) ||
        is.function(args$useViewer))) {
      useViewer <- args$useViewer
      args$useViewer <- NULL
    } else {
      useViewer <- TRUE
    }
  }

  if (interactive() &&
    !getOption("htmlTable.cat", FALSE) &&
    (is.function(useViewer) ||
      useViewer != FALSE)) {
    if (is.null(args$file)) {
      args$file <- tempfile(fileext = ".html")
    }

    htmlPage <- paste("<html>",
      "<head>",
      "<meta http-equiv=\"Content-type\" content=\"text/html; charset=UTF-8\">",
      "</head>",
      "<body>",
      "<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
      enc2utf8(x),
      "</div>",
      "</body>",
      "</html>",
      sep = "\n"
    )
    # We only want to use those arguments that are actually in cat
    # anything else that may have inadvertadly slipped in should
    # be ignored or it will be added to the output
    cat_args <- args
    cat_args <- cat_args[names(cat_args) %in% names(formals(cat))[-1]]
    do.call(cat, c(htmlPage, cat_args))

    if (is.function(useViewer)) {
      useViewer(args$file)
    } else {
      viewer <- getOption("viewer")
      if (!is.null(viewer) &&
        is.function(viewer)) {
        # (code to write some content to the file)
        viewer(args$file)
      } else {
        utils::browseURL(args$file)
      }
    }
  } else {
    cat_args <- args
    cat_args <- cat_args[names(cat_args) %in% names(formals(cat))[-1]]
    do.call(cat, c(x, cat_args))
  }

  invisible(x)
}
