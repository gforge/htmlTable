# This checks to make sure that the mapping columns of the tidy dataset
# uniquely specify a given value
check_uniqueness <- function(x, ...) {
  # Get arguments
  args <- simplify_arg_list(...)
  cols <- as.character(args)
  dupes <- x %>%
    dplyr::select(cols) %>%
    duplicated
  if (sum(dupes) != 0) {

    stop(paste0("The input parameters ",
                paste(paste0("\"", names(args), "\""), collapse = ", "),
                " do not specify unique rows. The following rows ",
                "are duplicated: ",
                paste(which(dupes), collapse = ", ")))
  }
}