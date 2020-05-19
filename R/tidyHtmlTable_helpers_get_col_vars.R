# This function gets arguments from ..., removes those that are NULL,
# and then subsets those that should map tidy data columns to htmlTable
# parameters
get_col_vars <- function(...) {
  out <- simplify_arg_list(...)
  return(out[names(out) %in%
               c("value", "header",
                 "rnames", "rgroup",
                 "cgroup1", "cgroup2",
                 "tspanner")])
}
