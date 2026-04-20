library(testthat)
library(htmlTable)

context("Public API compatibility")

test_that("Exported namespace stays stable", {
  expected_exports <- c(
    "setHtmlTableTheme",
    "interactiveTable",
    "hasHtmlTableStyle",
    "htmlTableWidget",
    "htmlTable",
    "getHtmlTableTheme",
    "tblNoNext",
    "addHtmlTableStyle",
    "txtPval",
    "htmlTableWidgetOutput",
    "outputInt",
    "txtInt",
    "getHtmlTableStyle",
    "splitLines4Table",
    "vector2string",
    "txtMergeLines",
    "tidyHtmlTable",
    "tblNoLast",
    "pvalueFormatter",
    "txtRound",
    "renderHtmlTableWidget",
    "prepGroupCounts",
    "concatHtmlTables",
    "highlightRow"
  )

  expect_setequal(getNamespaceExports("htmlTable"), expected_exports)
})

test_that("Main public signatures stay stable", {
  expect_identical(
    names(formals(htmlTable)),
    c(
      "x", "header", "rnames", "rowlabel", "caption", "tfoot", "label",
      "rgroup", "n.rgroup", "cgroup", "n.cgroup", "tspanner", "n.tspanner",
      "total", "ctable", "compatibility", "cspan.rgroup", "escape.html", "..."
    )
  )

  expect_identical(
    names(formals(tidyHtmlTable)),
    c(
      "x", "value", "header", "rnames", "rgroup", "hidden_rgroup", "cgroup",
      "tspanner", "hidden_tspanner", "skip_removal_warning", "rnames_unique",
      "table_fn", "..."
    )
  )

  expect_identical(
    names(formals(interactiveTable)),
    c("x", "...", "txt.maxlen", "button", "minimized.columns", "js.scripts")
  )

  expect_identical(
    names(formals(addHtmlTableStyle)),
    c(
      "x", "align", "align.header", "align.cgroup", "css.rgroup",
      "css.rgroup.sep", "css.tspanner", "css.tspanner.sep", "css.total",
      "css.cell", "css.cgroup", "css.header", "css.header.border_bottom",
      "css.class", "css.table", "pos.rowlabel", "pos.caption", "col.rgroup",
      "col.columns", "padding.rgroup", "padding.tspanner", "spacer.celltype",
      "spacer.css.cgroup.bottom.border", "spacer.css", "spacer.content"
    )
  )

  expect_identical(
    names(formals(setHtmlTableTheme)),
    c(
      "theme", "align", "align.header", "align.cgroup", "css.rgroup",
      "css.rgroup.sep", "css.tspanner", "css.tspanner.sep", "css.total",
      "css.cell", "css.cgroup", "css.header", "css.header.border_bottom",
      "css.class", "css.table", "pos.rowlabel", "pos.caption", "col.rgroup",
      "col.columns", "padding.rgroup", "padding.tspanner", "spacer.celltype",
      "spacer.css.cgroup.bottom.border", "spacer.css", "spacer.content"
    )
  )

  expect_identical(
    names(formals(highlightRow)),
    c("x", "condition", "style")
  )

  expect_identical(
    names(formals(concatHtmlTables)),
    c("tables", "headers")
  )

  expect_identical(
    names(formals(txtInt)),
    c("x", "language", "html", "...")
  )

  expect_identical(
    names(formals(txtPval)),
    c("pvalues", "lim.2dec", "lim.sig", "html", "...")
  )

  expect_identical(
    names(formals(txtMergeLines)),
    c("...", "html")
  )
})
