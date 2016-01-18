#' An interactive table that allows you to limit the size of boxes
#'
#' This function wraps the htmlTable and adds JavaScript code for toggling the amount
#' of text shown in any particular cell.
#'
#' @param ... The exact same parameters as \code{\link{htmlTable}} uses
#' @param txt.maxlen The maximum length of a text
#' @param button Indicator if the cell should be clickable or if a button should appear with a plus/minus
#' @param minimized.columns Notifies if any particular columns should be collapsed from start
#' @param js.scripts If you want to add your own JavaScript code you can just add it here.
#'  All code is merged into one string where each section is wrapped in it's own
#'  \code{<scrip></script>} element.
#' @return An htmlTable with a javascript attribute containing the code that is then printed
#' @export
#' @example inst/examples/interactiveTable_example.R
#' @rdname interactiveTable
interactiveTable <- function(x, ..., txt.maxlen = 20, button = FALSE, minimized.columns, js.scripts = c()){
  UseMethod("interactiveTable")
}

getButtonDiv <- function(sign = "-"){
  template <- system.file("html_components/button.html", package = "htmlTable")
  if (template == "")
    stop("Could not find the button template file")

  template <- readChar(template, nchars = file.info(template)$size)
  gsub("%sign%", sign, template) %>%
    gsub("[\n\r]", " ", .)
}

#' @export
interactiveTable.default <- function(x, ...,
                                     txt.maxlen = 20,
                                     button = FALSE,
                                     minimized.columns,
                                     js.scripts = c()){
  if ("data.frame" %in% class(x))
    x <- prConvertDfFactors(x)
  if (!missing(minimized.columns)){
    if (is.character(minimized.columns)){
      if (minimized.columns != "last")
        stop("If you want to provide a character for columns you must",
             " provide 'last' - '", minimized.columns, "' has not yet",
             " been implemented.")
      minimized.columns <- ncol(x)
    }else if(is.logical(minimized.columns)){
      minimized.columns <- which(minimized.columns)
    }else if(!is.numeric(minimized.columns)){
      stop("Expecting the minimized columns to either be numbers or logical parameters")
    }else if(max(minimized.columns) > ncol(x)){
      stop("You can't minimize columns larger than the number of columns available.",
           "I.e. ", paste(minimized.columns[minimized.columns > ncol(x)], collapse =", "),
           " > ", ncol(x))
    }

    if(!is.null(dim(minimized.columns))){
      stop("Can only handle column vectors for minimization")
    }

    addon_elements <- paste("... ",
                            "<span class='hidden' style='display: none'>%span_inner_text%</span>")
    if (button){
      addon_elements <- paste(addon_elements,
                              getButtonDiv("+"))
    }
    for (col_no in minimized.columns){
      for (row_no in 1:nrow(x)){
        if (nchar(x[row_no, col_no]) > txt.maxlen){
          x[row_no, col_no] <-
            paste0(substr(x[row_no, col_no], 1, txt.maxlen),
                   gsub("%span_inner_text%", x[row_no, col_no], addon_elements))
        }
      }
    }
    # Pass false to allow warning later on
    minimized.columns <- FALSE
  }
  tbl <- htmlTable(x, ...)
  return(interactiveTable(tbl,
                          txt.maxlen = 20,
                          button = button,
                          minimized.columns = minimized.columns,
                          js.scripts = js.scripts))
}

#' @param tbl An htmlTable object can be directly passed into the function
#' @rdname interactiveTable
interactiveTable.htmlTable <- function(tbl,
                                       txt.maxlen = 20,
                                       button = FALSE,
                                       minimized.columns,
                                       js.scripts = c()){
  if (!missing(minimized.columns) && all(minimized.columns != FALSE))
    stop("Can't minimize columns after creating the htmlTable. Try calling the function directly with the input data that you used for htmlTable")

  class(tbl) <- c("interactiveTable", class(tbl))
  if (button) {
    template <- system.file("javascript/button.js", package = "htmlTable")
    if (template == "")
      stop("Could not find the javascript button template file")
    template <- readChar(template, nchars = file.info(template)$size)

    attr(tbl, "javascript") <- c(js.scripts,
                                 template %>%
                                   gsub("%txt.maxlen%", txt.maxlen, .) %>%
                                   gsub("%btn%", getButtonDiv(), .))
  }else{
    template <- system.file("javascript/toggler.js", package = "htmlTable")
    if (template == "")
      stop("Could not find the javascript toggler template file")
    template <- readChar(template, nchars = file.info(template)$size)

    attr(tbl, "javascript") <- c(js.scripts,
                                 template %>%
                                   gsub("%txt.maxlen%", txt.maxlen, .))
  }

  return(tbl)
}

#' @rdname interactiveTable
#' @importFrom knitr knit_print
#' @importFrom knitr asis_output
#' @export
knit_print.interactiveTable<- function(x, ...){
  if (getOption("interactiveTable_knitprint", FALSE)){
    asis_output(x)
  }else{
    options(interactiveTable_knitprint = TRUE)
    asis_output(paste(x,
                      attr(x, "javascript")))
  }
}

#' Gets a string with all the scripts merged into one script tag
#'
#' Each element has it's own script tags in otherwise an error will cause
#' all the scripts to fail.
#'
#' @param x An interactiveTable
#' @return string
#' @keywords internal
prGetScriptString <- function(x){
  scripts <- attr(x, "javascript")
  if (is.null(scripts))
    stop("You have provided an object of class ", class(x), " that does not contain a javascript attribute")

  sapply(scripts,
         USE.NAMES = FALSE,
         FUN = function(s){
           if (s == "")
             return("")

           paste("<script type = \"text/javascript\" language = \"javascript\">",
                 s,
                 "</script>")
         }) %>%
    paste(collapse = "\n\n <!-- *** Next script group *** !-->\n")
}

#' @rdname interactiveTable
#' @param x The interactive table that is to be printed
#' @inheritParams htmlTable
#' @export
print.interactiveTable <- function(x, useViewer, ...){
  args <- attr(x, "...")
  # Use the latest ... from the print call
  # and override the original htmlTable call ...
  # if there is a conflict
  print_args <- list(...)
  for (n in names(print_args)){
    args[[n]] <- print_args[[n]]
  }

  # Since the print may be called from another print function
  # it may be handy to allow functions to use attributes for the
  # useViewer parameter
  if (missing(useViewer)){
    if ("useViewer" %in% names(args) &&
        (is.logical(args$useViewer) ||
         is.function(args$useViewer))){
      useViewer <- args$useViewer
      args$useViewer <- NULL
    }else{
      useViewer <- TRUE
    }
  }

  if (interactive() &&
      !getOption("htmlTable.cat", FALSE) &&
      (is.function(useViewer) ||
       useViewer != FALSE))
  {
    if (is.null(args$file)){
      args$file <- tempfile(fileext=".html")
    }

    htmlPage <- paste("<html>",
                      "<head>",
                      "<meta http-equiv=\"Content-type\" content=\"text/html; charset=UTF-8\">",
                      "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js\"></script>",
                      "</head>",
                      "<body>",
                      "<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
                      x,
                      "</div>",
                      prGetScriptString(x),
                      "</body>",
                      "</html>", sep="\n")
    # We only want to use those arguments that are actually in cat
    # anything else that may have inadvertadly slipped in should
    # be ignored or it will be added to the output
    cat_args <- args
    cat_args <- cat_args[names(cat_args) %in% names(formals(cat))[-1]]
    do.call(cat, c(htmlPage, cat_args))

    if (is.function(useViewer)){
      useViewer(args$file)
    }else{
      viewer <- getOption("viewer")
      if (!is.null(viewer) &&
          is.function(viewer)){
        # (code to write some content to the file)
        viewer(args$file)
      }else{
        utils::browseURL(args$file)
      }
    }
  }else{
    cat_args <- args
    cat_args <- cat_args[names(cat_args) %in% names(formals(cat))[-1]]
    do.call(cat, c(x, cat_args))
    cat(prGetScriptString(x))
  }

  invisible(x)
}
