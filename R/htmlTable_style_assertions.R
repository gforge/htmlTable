prAssertStyleNames <- function(x, message) {
  if (any(x == "")) {
    stop(message, " Empty names not allowed.")
  }

  invalid_names <- prGetInvalidStyleNames(x)
  if (length(invalid_names) > 0) {
    stop(message, " See name(s): ", paste(invalid_names, collapse = ", "))
  }
}

prGetInvalidStyleNames <- function(x) {
  valid_names <- Filter(
    function(x) !(x %in% c("theme", "")),
    names(formals(setHtmlTableTheme))
  )

  checked_names <- args %in% valid_names
  return(args[!checked_names])
}


prAssertStyles <- function(style_list) {
  assert_list(style_list, names = "named", .var.name = deparse(substitute(style_list)))

  css_styles <- names(style_list)[startsWith(names(style_list), "css.")]
  for (style in css_styles) {
    prAssertStyle(style_list[[style]], name = style)
  }

  return(TRUE)
}

prAssertStyle <- function(elmnt, name) {
  assert_character(elmnt,
    min.chars = 0,
    min.len = 1,
    .var.name = name
  )

  elmnts2check <- Filter(
    function(x) nchar(x) > 0,
    elmnt
  )

  if (name != "css.class" && is.null(names(elmnts2check)) && length(elmnts2check) > 0) {
    assert_true(all(sapply(elmnts2check, function(e) grepl(":", e))),
      .var.name = name
    )
  }
}
