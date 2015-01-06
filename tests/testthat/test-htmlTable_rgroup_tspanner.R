library(testthat)
library(XML)

context("htmlTable - the rgroup argument")

test_that("Check that rgroup has the appropriate padding",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  rownames(mx) <- sprintf("Row %s", LETTERS[1:NROW(mx)])
  out <- htmlTable(mx, rgroup = paste("rgroup", 1:2), n.rgroup = rep(1, 2))

  expect_match(out,
               "<tr[^>]*><td[^>]*>rgroup 1")

  expect_match(out,
               "<tr[^>]*>[^<]*<td[^>]*>&nbsp;&nbsp;Row A")

  expect_match(out,
               "<tr[^>]*><td[^>]*>rgroup 2")
  expect_match(out,
               "<tr[^>]*>[^<]*<td[^>]*>&nbsp;&nbsp;Row B")

  out <- htmlTable(mx, rgroup = paste("rgroup", 1:2), n.rgroup = rep(1, 2), padding.rgroup = "ll")

  expect_match(out,
               "<tr[^>]*><td[^>]*>rgroup 1")

  expect_match(out,
               "<tr[^>]*>[^<]*<td[^>]*>llRow A")

  out <- htmlTable(mx, rgroup = paste("rgroup", 1:2), n.rgroup = rep(1, 2), tspanner = paste("tspanner", 1:2), n.tspanner = rep(1, 2), padding.tspanner = "ii", padding.rgroup = "ll")

  expect_match(out,
               "<tr[^>]*><td[^>]*>iirgroup 1")

  expect_match(out,
               "<tr[^>]*>[^<]*<td[^>]*>iillRow A")

}

test_that("Check that dimensions are correct with rgroup usage",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <-
    suppressWarnings(htmlTable(mx,
                               rgroup=c("test1", "test2"),
                               n.rgroup=c(1,1)))
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(ncol(parsed_table), ncol(mx), info="Cols did not match")
  expect_equal(nrow(parsed_table), nrow(mx) + 2, info="Rows did not match")
  expect_equal(as.character(parsed_table[1,1]),
               "test1", info="The rgroup did not match")
  expect_equal(as.character(parsed_table[3,1]),
               "test2", info="The rgroup did not match")
  expect_equal(as.character(parsed_table[2,1]),
               as.character(mx[1,1]), info="The row values did not match")
  expect_equal(as.character(parsed_table[4,1]),
               as.character(mx[2,1]), info="The row values did not match")


  expect_warning(htmlTable(mx,
                           rgroup=c("test1", "test2", "test3"),
                           n.rgroup=c(1,1, 0)))

  expect_error(suppressWarnings(htmlTable(mx,
                                          roup=c("test1", "test2", "test3"),
                                          rgroup=c(1,1, 10))))

  mx[2,1] <- "second row"
  table_str <- htmlTable(mx,
                         rnames=letters[1:2],
                         rgroup=c("test1", ""),
                         n.rgroup=c(1,1))
  expect_match(table_str, "<td[^>]*>second row",
               info="The second row should not have any spacers")

  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(nrow(parsed_table), nrow(mx) + 1, info="Rows did not match")
})
