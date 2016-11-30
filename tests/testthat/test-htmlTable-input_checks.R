library('testthat')
library('magrittr')
library('XML')
context('htmlTable')

# A simple example
test_that("With empty rownames(mx) it should skip those", {
  mx <- matrix(1:6, ncol=3)
  css.cell ="background: red"
  htmlTable(mx, css.cell=css.cell)

  css.cell = "background: red" %>%
    matrix(ncol=ncol(mx), nrow=nrow(mx)) %>%
    as.data.frame()
  expect_error(htmlTable(mx, css.cell=css.cell))
})

