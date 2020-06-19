library(testthat)

test_that("Test cell styles", {
  mx <- matrix(1:3, nrow = 2, ncol = 3, byrow = TRUE)
  mx_head <- LETTERS[1:ncol(mx)]
  mx_rnames <- LETTERS[1:nrow(mx)]
  expect_equal(
    dim(prPrepareCss(mx, "")),
    dim(mx)
  )
  expect_equal(
    dim(prPrepareCss(mx, "", header = mx_head, rnames = mx_rnames)),
    dim(mx)
  )

  expect_equal(
    dim(prPrepareCss(mx, "", header = mx_head, rnames = mx_rnames)),
    dim(mx)
  )

  expect_equal(
    dim(prPrepareCss(mx, rep("", times = ncol(mx)))),
    dim(mx)
  )

  expect_error(prPrepareCss(mx, rep("", times = nrow(mx))))


  mx_cell.style <- matrix(c("a", "b", "c", "d"), nrow = 2, ncol = 4, byrow = TRUE)
  expect_equal(
    prPrepareCss(mx, mx_cell.style, rnames = mx_rnames)[2, 1],
    "b"
  )

  expect_error(prPrepareCss(mx, mx_cell.style))

  mx_cell.style <- matrix(c("a", "b", "c", "d"), nrow = 3, ncol = 4, byrow = TRUE)
  expect_equal(
    prPrepareCss(mx, mx_cell.style,
                 header = mx_head,
                 rnames = mx_rnames
    )[2, 1],
    "b"
  )

  expect_error(prPrepareCss(mx, mx_cell.style, rnames = mx_rnames))
})
