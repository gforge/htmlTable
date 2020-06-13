#' Add a ; at the end
#'
#' The CSS expects a semicolon at the end of each argument
#' this function just adds a semicolong if none is given
#' and remove multiple semicolon if such exist
#'
#' @param my_str The string that is to be processed
#' @return \code{string}
#' @keywords internal
#' @family hidden helper functions for htmlTable
#' @importFrom utils tail
prAddSemicolon2StrEnd <- function(my_str){
  if (!is.null(names(my_str))){
    tmp <- str_trim(my_str)
    names(tmp) <- names(my_str)
    my_str <- tmp
  }else{
    my_str <- str_trim(my_str)
  }
  my_str_n <- sapply(my_str, nchar, USE.NAMES = FALSE)
  if (any(my_str_n == 0))
    my_str <- my_str[my_str_n > 0]

  if(length(my_str) == 0)
    return("")

  if (tail(strsplit(my_str, "")[[1]], 1) != ";"){
    n <- names(my_str)
    my_str <- sprintf("%s;", my_str)
    if (!is.null(n))
      names(my_str) <- n
  }

  # Remove duplicated ;
  my_str <- gsub(";;+", ";", my_str)
  empty_str <- sapply(my_str, function(x) x == ";", USE.NAMES = FALSE)
  if (any(empty_str))
    my_str <- my_str[!empty_str]

  if(length(my_str) == 0)
    return("")

  return (my_str)
}