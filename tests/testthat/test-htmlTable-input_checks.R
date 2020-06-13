library(testthat)
library('magrittr', warn.conflicts = FALSE)
library('XML', warn.conflicts = FALSE)

# Check that a css.cell passes without errors
test_that("Check css.cell input", {
  expect_match(matrix(1:6, ncol=3) %>%
                 addHtmlTableStyle(css.cell="background: red") %>%
                 htmlTable,
               "background: red")
})
