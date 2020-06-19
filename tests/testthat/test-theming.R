require(testthat)

context('Theming for htmlTable')

# A simple example
test_that("Get current themes", {
  theme <- getHtmlTableTheme()
  expect_list(theme, names = "unique")
  valid_names <- Filter(function(x) !(x %in% c("theme", "")),
                        names(as.list(setHtmlTableTheme)))
  expect_true(all(names(theme) %in% valid_names))
})

test_that("Set current theme", {
  newTheme <- setHtmlTableTheme(align = "l")
  theme <- getHtmlTableTheme()

  expect_equal(newTheme, theme)
  expect_equal(theme$align, "l")
})


test_that("Style assertions", {
  expect_error(prAssertStyles(list("a")), regexp = "Must have names")
  expect_error(prAssertStyles(list(css.rgroup = "height: 100px", css.rnames = "width")), regexp = "css.rnames")
  expect_true(prAssertStyles(list(css.rnames = "width: 100px")))

  expect_error(prAssertStyles(list(css.rnames = "width: 100px", css.tspanner = list(a = 2))), regexp = "list")
})
