library(testthat)
library(XML)
library(tibble)
library(magrittr)
context("htmlTable")

# A simple example
test_that("With empty rownames(mx) it should skip those", {
  mx <- matrix(1:6, ncol = 3)
  table_str <- htmlTable(mx)
  expect_false(grepl("</th>", table_str))
  expect_false(grepl("<tr>[^>]+>NA</td>", table_str))

  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- htmlTable(mx)
  expect_true(grepl("</th>", table_str))
  expect_false(grepl("<tr>[^>]+>NA</td>", table_str))
})


test_that("Empty cell names should be replaced with ''", {
  mx <- matrix(1:6, ncol = 3)
  mx[1, 1] <- NA
  table_str <- htmlTable(mx)
  expect_false(grepl("<tr>[^>]+>NA</td>", table_str))
})

test_that("The variable name should not be in the tables first row if no rownames(mx)", {
  mx <- matrix(1:6, ncol = 3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- htmlTable(mx)
  expect_false(grepl("<thead>[^<]*<tr>[^>]+>mx</th>", table_str))
})

test_that("A rowlabel without rownames indicates some kind of error and should throw an error", {
  mx <- matrix(1:6, ncol = 3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  expect_error(htmlTable(mx, rowlabel = "not_mx"))
})

# Add rownames
test_that("The rowname should appear", {
  mx <- matrix(1:6, ncol = 3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  rownames(mx) <- LETTERS[1:NROW(mx)]
  table_str <- htmlTable(mx)
  parsed_table <- readHTMLTable(as.character(table_str))[[1]]
  expect_equal(ncol(parsed_table), ncol(mx) + 1)
  expect_match(table_str, "<tr[^>]*>[^>]+>A</td>")
  expect_match(table_str, "<tr[^>]*>[^>]+>B</td>")
})

test_that("Check that basic output are the same as the provided matrix", {
  mx <- matrix(1:6, ncol = 3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- htmlTable(mx)
  parsed_table <- readHTMLTable(as.character(table_str))[[1]]
  expect_equal(ncol(parsed_table), ncol(mx), info = "Cols did not match")
  expect_equal(nrow(parsed_table), nrow(mx), info = "Rows did not match")
  expect_true(all(mx == parsed_table),
    info = "Some cells don't match the inputted cells"
  )
})



test_that("rnames = FALSE it should skip those", {
  mx <- matrix(1:6, ncol = 3)
  rownames(mx) <- c("Row A", "Row B")
  table_str <- htmlTable(mx, rnames = FALSE)
  expect_false(grepl("FALSE", table_str))
  expect_false(grepl("Row A", table_str))
})

test_that("Test align functions", {
  expect_equivalent(
    nchar(prPrepareAlign("lr", x = matrix(1, ncol = 10))),
    10
  )
  expect_equivalent(
    nchar(prPrepareAlign("lr", x = matrix(1, ncol = 2))),
    2
  )
  expect_equivalent(
    nchar(prPrepareAlign("lr", x = matrix(1, ncol = 2), rnames = TRUE)),
    3
  )
  expect_equivalent(
    nchar(prPrepareAlign("l", x = matrix(1, ncol = 2), rnames = TRUE)),
    3
  )
  expect_equivalent(
    nchar(prPrepareAlign("", x = matrix(1, ncol = 2, nrow = 2), rnames = TRUE)),
    3
  )

  expect_equivalent(
    attr(prPrepareAlign("r|rlt|r|", x = matrix(1, ncol = 2, nrow = 2), rnames = TRUE), "n"),
    3
  )

  expect_equivalent(
    attr(prPrepareAlign("r|rcl|lt|r|", x = matrix(1, ncol = 5, nrow = 2), rnames = TRUE), "n"),
    6
  )
  expect_match(
    prPrepareAlign("r|rcl|lt|r|", x = matrix(1, ncol = 5, nrow = 2), rnames = TRUE),
    "^r"
  )

  expect_match(
    prPrepareAlign("l|r|", x = matrix(1, ncol = 3, nrow = 2), rnames = TRUE),
    "^l|r|r|$"
  )

  align_str <- prPrepareAlign("r|rcl|lt|r|", x = matrix(1, ncol = 5, nrow = 2), rnames = TRUE)
  expect_true("right" %in% prGetAlign(align_str, 1))
  expect_true("right" %in% prGetAlign(align_str, 2))
  expect_true("center" %in% prGetAlign(align_str, 3))
  expect_true("left" %in% prGetAlign(align_str, 4))
  expect_true("left" %in% prGetAlign(align_str, 5))
  expect_true("right" %in% prGetAlign(align_str, 6))

  expect_true("border-right" %in% names(prGetAlign(align_str, 1)))
  expect_true("border-right" %in% names(prGetAlign(align_str, 4)))
  expect_true("border-right" %in% names(prGetAlign(align_str, 5)))
  expect_true("border-right" %in% names(prGetAlign(align_str, 6)))

  expect_equivalent(length(prGetAlign(align_str, 1)), 2)
  expect_equivalent(length(prGetAlign(align_str, 2)), 1)
  expect_equivalent(length(prGetAlign(align_str, 6)), 2)

  align_str <- prPrepareAlign("|c|rc", x = matrix(1, ncol = 2, nrow = 2), rnames = TRUE)
  expect_true("border-right" %in% names(prGetAlign(align_str, 1)))
  expect_true("border-left" %in% names(prGetAlign(align_str, 1)))
  expect_true("center" %in% prGetAlign(align_str, 1))

  mx <- matrix(1:6, ncol = 3)
  rownames(mx) <- c("Row A", "Row B")
  table_str <- htmlTable(mx, rname = FALSE)
  expect_match(table_str, "text-align: center;[^>]*>1")
  expect_match(table_str, "text-align: center;[^>]*>3")
  expect_match(table_str, "text-align: center;[^>]*>5")

  table_str <- htmlTable(mx)
  expect_match(table_str, "text-align: left;[^>]*>Row A")
  expect_match(table_str, "text-align: center;[^>]*>1")
  expect_match(table_str, "text-align: center;[^>]*>3")
  expect_match(table_str, "text-align: center;[^>]*>5")

  table_str <- htmlTable(mx, align = "r")
  expect_match(table_str, "text-align: left;[^>]*>Ro")
  expect_match(table_str, "text-align: right;[^>]*>1")
  expect_match(table_str, "text-align: right;[^>]*>3")
  expect_match(table_str, "text-align: right;[^>]*>5")

  table_str <- htmlTable(mx, align = "|ll|r|r|")
  expect_match(table_str, "text-align: left;[^>]*>Ro")
  expect_match(table_str, "text-align: left;[^>]*>1")
  expect_match(table_str, "text-align: right;[^>]*>3")
  expect_match(table_str, "text-align: right;[^>]*>5")

  expect_match(table_str, "border-left:[^>]*>Ro")
  expect_match(table_str, "border-right:[^>]*>1")
  expect_match(table_str, "border-right:[^>]*>3")
  expect_match(table_str, "border-right:[^>]*>5")
})

test_that("Check color function", {
  expect_equivalent(
    prPrepareColors(c("white", "#444444"), 2),
    c("#ffffff", "#444444")
  )
  expect_equivalent(
    prPrepareColors(c("white", "#444444"), 3),
    c("#ffffff", "#444444", "#ffffff")
  )
  expect_equivalent(
    prPrepareColors(c("white", "#444"), 3),
    c("#ffffff", "#444444", "#ffffff")
  )

  expect_null(attr(prPrepareColors(c("white", "#444444"), 3), "groups"))
  expect_equivalent(
    attr(prPrepareColors(c("white", "#444444"),
      n = 3,
      ng = c(2, 3, 1),
      gtxt = c("a", "b", "c")
    ), "groups")[[1]],
    c("#ffffff", "#ffffff")
  )
  expect_equivalent(
    attr(prPrepareColors(c("white", "#444444"),
      n = 3,
      ng = c(2, 3, 1),
      gtxt = c("a", "b", "c")
    ), "groups")[[2]],
    c("#444444", "#444444", "#444444")
  )
  expect_equivalent(
    attr(prPrepareColors(c("white", "#444444"),
      n = 3,
      ng = c(2, 3, 1),
      gtxt = c("a", "b", "c")
    ), "groups")[[3]],
    c("#ffffff")
  )

  expect_equivalent(
    attr(prPrepareColors(c("white", "#444444", "none"),
      n = 3,
      ng = c(2, 3, 1),
      gtxt = c("a", "b", "c")
    ), "groups")[[3]],
    c("none")
  )

  expect_equivalent(
    attr(prPrepareColors(c("white", "none"),
      n = 3,
      ng = c(2, 3, 1),
      gtxt = c("a", "b", "c")
    ), "groups")[[2]],
    c("none", "none", "none")
  )

  ## Test the merge colors
  expect_equal(
    prMergeClr(c("white", "#444444")),
    colorRampPalette(c("#FFFFFF", "#444444"))(3)[2]
  )

  expect_equal(
    prMergeClr(c("red", "#444444")),
    colorRampPalette(c("red", "#444444"))(3)[2]
  )

  expect_equal(
    prMergeClr(c("#444444", "red")),
    colorRampPalette(c("red", "#444444"))(3)[2]
  )

  expect_equal(
    prMergeClr(c("#FFFFFF", "#FFFFFF", "#FFFFFF")),
    "#FFFFFF"
  )

  expect_equal(
    prMergeClr(c("#FFFFFF", "#FFFFFF", "#000000", "#000000")),
    prMergeClr(c("#FFFFFF", "#000000"))
  )

  expect_equal(
    prMergeClr(c("#000000", "#FFFFFF", "#FFFFFF")),
    prMergeClr(c("#FFFFFF", "#FFFFFF", "#000000"))
  )

  expect_equal(
    prMergeClr(c("#000000", "#FFFFFF", "#000000")),
    prMergeClr(c("#FFFFFF", "#000000", "#FFFFFF"))
  )
})



test_that("Test prAddSemicolon2StrEnd", {
  test_str <- "background: white"
  expect_equal(
    prAddSemicolon2StrEnd(test_str),
    paste0(test_str, ";")
  )
  test_str <- c("", "", `background-color` = "none")
  expect_equivalent(
    prAddSemicolon2StrEnd(test_str),
    paste0(test_str[3], ";")
  )

  expect_equal(
    names(prAddSemicolon2StrEnd(test_str)),
    names(test_str[3])
  )
})


test_that("Problem with naming in stringr 1.0.0", {
  style_bug <- structure(c("", "font-weight: 900;", "#f7f7f7"),
    .Names = c("", "", "background-color")
  )
  expect_false(is.null(names(prAddSemicolon2StrEnd(style_bug))))
  expect_match(prGetStyle(style_bug),
    regexp = "^font-weight: 900; background-color: #f7f7f7"
  )
})

test_that("Handling data.frames with factors", {
  tmp <- data.frame(
    a = 1:3,
    b = factor(
      x = 1:3,
      labels = c(
        "Unique_Factor_1",
        "Factor_2",
        "Factor_3"
      )
    )
  )

  str <- htmlTable(tmp)
  expect_true(grepl("Unique_Factor_1", str))

  tmp <- data.frame(
    a = 1.23,
    b = factor(
      x = 1,
      labels = c("1.2")
    )
  ) %>%
    txtRound()

  expect_true(tmp$a == "1",
              tmp$b == "1.2")
})

context("htmlTable - empty table")

test_that("has header elements", {
  empty_dataframe <- data.frame(
    a = numeric(),
    b = factor(levels = c(
      "level one",
      "level two"
    ))
  )
  expect_warning({
    table_str <- htmlTable(empty_dataframe)
  })

  th_cell_regex <- function(content) str_interp("[^<]*<th[^>]*>${CONTENT}</th>", list(CONTENT = content))
  expect_match(table_str,
               str_interp("<thead>[^<]*<tr>${CELL1}${CELL2}[^<]*</tr>",
                          list(CELL1 = th_cell_regex("a"),
                               CELL2 = th_cell_regex("b"))))
  expect_match(table_str, "<tbody>[^<]+</tbody>")

  expect_warning({
    table_str <- htmlTable(empty_dataframe,
      rnames = TRUE,
      rowlabel = "Row number",
      cgroup = "Spanner",
      n.cgroup = 2,
      col.rgroup = c(
        "white",
        "gray"
      ),
      caption = "This is a caption",
      tfoot = "This is a footnote"
    )
  })

  expect_match(table_str,
               str_interp("<tr>${CELL_LABEL}${CELL1}${CELL2}[^<]*</tr>",
                          list(CELL_LABEL = th_cell_regex("Row number"),
                               CELL1 = th_cell_regex("a"),
                               CELL2 = th_cell_regex("b"))))
  expect_match(table_str, "<tbody>[^<]+</tbody>")
  expect_match(table_str, "<tfoot><tr><td[^>]+>\\s*This is a footnote</td></tr>", perl = TRUE)
  expect_match(table_str, "<tr><td[^>]+>\\s*This is a caption</td></tr>", perl = TRUE)
})

test_that("An empty dataframe returns an empty table with a warning", {
  empty_dataframe <- data.frame(
    a = numeric(),
    b = factor(levels = c(
      "level one",
      "level two"
    ))
  )
  expect_warning(htmlTable(empty_dataframe), regexp = "empty_dataframe")

  empty_matrix <- empty_dataframe %>%
    as.matrix()
  expect_warning(htmlTable(empty_matrix), regexp = "empty_matrix")

  expect_warning(htmlTable(empty_dataframe))

  expect_warning(htmlTable(empty_dataframe,
    cgroup = "Spanner",
    n.cgroup = 2
  ))

  expect_warning(htmlTable(empty_dataframe,
    cgroup = "Spanner",
    n.cgroup = 2,
    caption = "Caption",
    tfoot = "Footnote"
  ))

  expect_warning(htmlTable(empty_dataframe,
    col.rgroup = c(
      "white",
      "gray"
    )
  ))

  expect_warning(htmlTable(empty_dataframe,
    rnames = TRUE,
    rowlabel = "Row number",
    cgroup = "Spanner",
    n.cgroup = 2,
    col.rgroup = c(
      "white",
      "gray"
    )
  ))

  expect_warning(htmlTable(empty_dataframe,
    rnames = TRUE,
    rowlabel = "Row number",
    cgroup = "Spanner",
    n.cgroup = 2,
    col.rgroup = c(
      "white",
      "gray"
    ),
    caption = "This is a caption",
    tfoot = "This is a footnote"
  ))
})
