# You need the suggested package for this function
safeLoadPkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("The package ", pkg, " is needed for this function to work. Please install it.",
         call. = FALSE)
  }
}