library('testthat')
context('txtPval')

test_that("Add zero", {
  expect_equal(txtPval(.5, lim.2dec=10^-1), "0.50")
  expect_equal(txtPval(.06, lim.2dec=10^-1), "0.06")
  expect_equal(txtPval(.06, lim.2dec=10^-2), "0.060")
  expect_equal(txtPval(.06451, lim.2dec=10^-3), "0.065")
  expect_equal(txtPval(.00006451, lim.sig=10^-3), "&lt; 0.001")
})

test_that("Matrix rounder",{
  test_mx <- matrix(c(1, 1.11, 1.25,
                  2.50, 2.55, 2.45,
                  3.2313, 3, pi),
                ncol = 3, byrow=TRUE)
  expect_equivalent(txtRound(test_mx, 1),
                    t(apply(test_mx, 1, function(x) sprintf("%.1f", x))))

  expect_equivalent(txtRound(test_mx, 1, excl.cols = 2)[2,2],
                    as.character(test_mx[2,2]))
  expect_equivalent(txtRound(test_mx, 1, excl.rows = 2)[2,2],
                    as.character(test_mx[2,2]))

  expect_equivalent(txtRound(test_mx, 1, excl.cols = 2)[2,1],
                    sprintf("%.1f", test_mx[2,1]))
  expect_equivalent(txtRound(test_mx, 1, excl.rows = 2)[1,1],
                    sprintf("%.1f", test_mx[1,1]))

  expect_equivalent(txtRound(test_mx, 1, excl.cols = 2)[2,3],
                    sprintf("%.1f", test_mx[2,3]))

  rownames(test_mx) <- letters[1:nrow(test_mx)]
  colnames(test_mx) <- LETTERS[1:ncol(test_mx)]
  expect_equivalent(txtRound(test_mx, 1, excl.cols = "A")[3,"A"],
                    as.character(test_mx[3,"A"]))
  expect_equivalent(txtRound(test_mx, 1, excl.cols = "A")[3,"C"],
                    sprintf("%.1f", test_mx[3,"C"]))

  expect_equivalent(txtRound(test_mx, 1, excl.rows = "a")["a", 3],
                    as.character(test_mx["a", 3]))
  expect_equivalent(txtRound(test_mx, 1, excl.rows = "a")["c", 3],
                    sprintf("%.1f", test_mx["c", 3]))
})