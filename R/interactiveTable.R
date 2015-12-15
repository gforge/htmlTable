#' An interactive table that allows you to limit the size of boxes
#'
#' This function wraps the htmlTable and adds JavaScript code for toggling the amount
#' of text shown in any particular cell.
#'
#' @param ... The exact same parameters as \code{\link{htmlTable}} uses
#' @param txt.maxlen The maximum length of a text
#' @param button Indicator if the cell should be clickable or if a button should appear with a plus/minus
#' @param minimized.columns Notifies if any particular columns should be collapsed from start
#' @return An htmlTable with a javascript attribute containing the code that is then printed
#' @export
#' @rdname interactiveTable
interactiveTable <- function(x, ..., txt.maxlen = 20, button = FALSE, minimized.columns){
  UseMethod("interactiveTable")
}

getButtonDiv <- function(sign = "-"){
  paste0("<div class='btn'",
         " style='background-color: #ccc; border: 1px solid #999; float: right; font-size: 0.5em; height: 1.8em;",
         " margin: 2px; padding: 0.1em; position: relative; text-align: center; top: 0; width: 1.8em;'",
         ">", sign, "</div>")
}

#' @export
interactiveTable.default <- function(x, ..., txt.maxlen = 20, button = FALSE, minimized.columns){
  if ("data.frame" %in% class(x))
    x <- prConvertDfFactors(x)
  if (!missing(minimized.columns)){
    if(is.logical(minimized.columns))
      minimized.columns <- which(minimized.columns)
    if(!is.numeric(minimized.columns))
      stop("Expecting the minimized columns to either be numbers or logical parameters")
    if(max(minimized.columns) > ncol(x))
      stop("You can't minimize columns larger than the number of columns available.",
           "I.e. ", paste(minimized.columns[minimized.columns > ncol(x)], collapse =", "),
           " > ", ncol(x))
    if(!is.null(dim(minimized.columns)))
      stop("Can only handle column vectors for minimization")

    addon_elements <- paste("... ",
                            "<span class='hidden' style='display: none'>%span_inner_text%</span>")
    if (button){
      addon_elements <- paste(aaddon_elements,
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
                          minimized.columns = minimized.columns))
}

#' @param tbl An htmlTable object can be directly passed into the function
#' @rdname interactiveTable
interactiveTable.htmlTable <- function(tbl, txt.maxlen = 20, button = FALSE, minimized.columns){
  if (!missing(minimized.columns) && all(minimized.columns != FALSE))
    stop("Can't minimize columns after creating the htmlTable. Try calling the function directly with the input data that you used for htmlTable")

  class(tbl) <- c("interactiveTable", class(tbl))
  if (button) {
    attr(tbl, "javascript") <- "
<script type = \"text/javascript\" language = \"javascript\">
$(document).ready(function(){
 btn = \"%btn%\"
 $(\".gmisc_table td\").map(function(index, el){
   if (el.innerHTML.length > %txt.maxlen% && el.getElementsByClassName(\"btn\").length == 0)
     el.innerHTML += btn;
 })
 $(\".gmisc_table td .btn\").map(function(index, el){
   el.onclick =  function(e){
     var hidden = this.parentNode.getElementsByClassName(\"hidden\");
     if (this.textContent === \"+\"){
       this.parentNode.childNodes[0].data = hidden[0].textContent;
       this.textContent = \"-\";
     }else{
       $(this.parentNode).append(\"<span class='hidden' style='display: none'>\" + this.parentNode.childNodes[0].data + \"</span>\")
       this.parentNode.childNodes[0].data = this.parentNode.textContent.substr(0, %txt.maxlen%) + \"... \";
       this.textContent = \"+\";
     }
   }
 })
})
</script>" %>% gsub("%txt.maxlen%", txt.maxlen, .) %>%
      gsub("%btn%", getButtonDiv(), .)
  }else{
    attr(tbl, "javascript") <- "
<script type = \"text/javascript\" language = \"javascript\">
$(document).ready(function(){
 $(\".gmisc_table td .hidden\").map(function(index, el){
   el.parentNode.style[\"background-color\"] = \"#DDD\";
  })

  getSelected = function(){
    var t = '';
    if(window.getSelection){
      t = window.getSelection();
    }else if(document.getSelection){
      t = document.getSelection();
    }else if(document.selection){
      t = document.selection.createRange().text;
    }
    return t.toString();
  }

  $(\".gmisc_table td\").map(function(index, el){
    var org_clr = this.style[\"background-color\"]
    this.style.cursor = \"pointer\";
    el.onmouseup =  function(e){
      if (getSelected().length > 0)
        return;

      var hidden = this.getElementsByClassName(\"hidden\");
      if (hidden.length > 0){
        this.innerHTML = hidden[0].textContent;
        this.style[\"background-color\"] = org_clr;
      }else{
        $(this).append(\"<span class='hidden' style='display: none'>\" + this.innerHTML + \"</span>\")
        this.childNodes[0].data = this.childNodes[0].data.substr(0, 20) + \"... \";
        this.style[\"background-color\"] = \"#DDD\";
      }
    }
  })
})
</script>" %>% gsub("%txt.maxlen%", txt.maxlen, .) %>%
      gsub("%btn%", getButtonDiv(), .)
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

#' @rdname interactiveTable
#' @param x The interactive table that is to be printed
#' @inheritParams htmlTable
#' @export
print.interactiveTable <- print.htmlTable<- function(x, useViewer, ...){
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
                      attr(x, "javascript"),
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
    cat(attr(x, "javascript"))
  }

  invisible(x)
}
