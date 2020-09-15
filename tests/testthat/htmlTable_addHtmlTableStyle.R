library(testthat)
library(XML)

test_that("Standard addHtmlTableStyle",{
  mx <- matrix(1:6, ncol = 3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])


  expect_true(mx %>%
                addHtmlTableStyle(align = "r|r") %>%
                hasHtmlTableStyle("align"))

  style <- mx %>%
    addHtmlTableStyle(align = "r|r",
                      # Check partial match.arg for "bottom"
                      pos.caption = "bot") %>%
    getHtmlTableStyle()
  expect_list(style)


  expect_equal(style$align, "r|r")
  expect_equal(style$pos.caption, "bottom")

  expect_error(mx %>% addHtmlTableStyle(pos.caption = "invalid option"), regexp = "pos.caption")
})


