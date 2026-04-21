library(testthat)
library(magrittr, warn.conflicts = FALSE)

context("Row highlighting")

test_that("highlightRow applies preset, color, and raw CSS styles", {
  df <- data.frame(
    Name = c("Max", "Eva"),
    Score = c(10, 20),
    stringsAsFactors = FALSE
  )

  preset_out <- df %>%
    highlightRow(Name == "Max", style = "warning") %>%
    htmlTable(rnames = FALSE)

  expect_match(preset_out, "background-color: #fff3cd;", fixed = TRUE)
  expect_match(preset_out, "color: #856404;", fixed = TRUE)
  expect_match(preset_out, "<tr style='[^']*background-color: #fff3cd;", perl = TRUE)

  color_out <- df %>%
    highlightRow(Name == "Max", style = "khaki") %>%
    htmlTable(rnames = FALSE)

  expect_match(color_out, "background-color: khaki;", fixed = TRUE)

  css_out <- df %>%
    highlightRow(Name == "Max", style = "background-color: black; color: white;") %>%
    htmlTable(rnames = FALSE)

  expect_match(css_out, "background-color: black;", fixed = TRUE)
  expect_match(css_out, "color: white;", fixed = TRUE)
})

test_that("highlightRow works with vector inputs", {
  x <- c("alpha", "beta", "gamma")

  out <- x %>%
    highlightRow(.rowname == "1", style = "info") %>%
    htmlTable(rnames = FALSE)

  expect_match(out, "background-color: #d1ecf1;", fixed = TRUE)
  expect_match(out, "color: #0c5460;", fixed = TRUE)
  expect_match(out, "<tr style='[^']*background-color: #d1ecf1;", perl = TRUE)
  expect_match(out, "<td[^>]*>alpha</td>", perl = TRUE)
  expect_match(out, "<td[^>]*>beta</td>", perl = TRUE)
  expect_match(out, "<td[^>]*>gamma</td>", perl = TRUE)
})

test_that("highlightRow works with matrix inputs", {
  mx <- matrix(
    c("alpha", "beta", "gamma", "delta"),
    nrow = 2,
    byrow = TRUE
  )
  rownames(mx) <- c("row-1", "row-2")

  out <- mx %>%
    highlightRow(.rowname == "row-2", style = "info") %>%
    htmlTable()

  expect_match(out, "background-color: #d1ecf1;", fixed = TRUE)
  expect_match(out, "color: #0c5460;", fixed = TRUE)
  expect_match(out, "<tr style='[^']*background-color: #d1ecf1;", perl = TRUE)
  expect_match(out, "row-2", fixed = TRUE)
  expect_match(out, "gamma", fixed = TRUE)
  expect_match(out, "delta", fixed = TRUE)
})

test_that("Later row highlights override earlier ones", {
  df <- data.frame(
    Name = c("Max", "Eva"),
    Score = c(10, 20),
    stringsAsFactors = FALSE
  )

  out <- df %>%
    highlightRow(Name == "Max", style = "warning") %>%
    highlightRow(Name == "Max", style = "background-color: black; color: white;") %>%
    htmlTable(rnames = FALSE)

  expect_match(out, "background-color: black;", fixed = TRUE)
  expect_match(out, "color: white;", fixed = TRUE)
  expect_false(grepl("background-color: #fff3cd;", out, fixed = TRUE))
})

test_that(".rowname can be used in row highlight expressions", {
  df <- data.frame(
    Name = c("Max", "Eva"),
    Score = c(10, 20),
    stringsAsFactors = FALSE
  )
  rownames(df) <- c("row-1", "row-2")

  out <- df %>%
    highlightRow(.rowname == "row-2", style = "info") %>%
    htmlTable()

  expect_match(out, "row-2", fixed = TRUE)
  expect_match(out, "background-color: #d1ecf1;", fixed = TRUE)
})

test_that("highlightRow rejects invalid condition lengths", {
  df <- data.frame(
    Name = c("Max", "Eva", "Nils"),
    Score = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  expect_error(
    df %>%
      highlightRow(c(TRUE, FALSE), style = "warning") %>%
      htmlTable(rnames = FALSE),
    "length 1 or nrow\\(x\\)"
  )
})
