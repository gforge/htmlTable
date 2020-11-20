library(testthat)
library(dplyr)
library(tibble)
library(purrr)
library(glue)
library(XML)
library(xml2)
library(stringr)

# Add row names
test_that("Basic tidyHtmlTable functionality", {
  mx <- tribble(~value, ~header, ~name, ~rgroup, ~cgroup1, ~cgroup2,
                     1,       2,     3,       1,        1,        3,
                     2,       3,     4,       1,        2,        3,
                     3,       4,     5,       2,        2,        4) %>%
    mutate_at(vars(starts_with("cgroup")), ~glue("{name} cg", name = .)) %>%
    mutate(rgroup = glue("{name}_rg", name = rgroup),
           header = glue("{name}_h", name = header))
  table_str <- mx %>%
    tidyHtmlTable(header = header,
                  rowlabel = 'row',
                  label = "test_table")

  parsed_table <- readHTMLTable(as.character(table_str))[["test_table"]]
  expect_equal(ncol(parsed_table), 4)
  expect_equal(nrow(parsed_table), length(mx$value))
  expect_equal(parsed_table %>%
                 filter(row == 3) %>%
                 pluck("2_h") %>%
                 as.character(),
               mx %>%
                 filter(name == 3) %>%
                 pluck("value") %>%
                 as.character())

  expect_equal(parsed_table %>%
                 filter(row == 4) %>%
                 pluck("3_h") %>%
                 as.character(),
               mx %>%
                 filter(name == 4) %>%
                 pluck("value") %>%
                 as.character())

  expect_equal(parsed_table %>%
                 filter(row == 5) %>%
                 pluck("4_h") %>%
                 as.character(),
               mx %>%
                 filter(name == 5) %>%
                 pluck("value") %>%
                 as.character())

  table_str <- mx %>%
    tidyHtmlTable(header = header,
                  rgroup = rgroup,
                  label = "test_table")

  parsed_table <- readHTMLTable(as.character(table_str))[["test_table"]]
  expect_equal(ncol(parsed_table), 4)
  expect_equal(nrow(parsed_table), length(mx$value) + length(mx$rgroup %>% unique))

  table_str <- mx %>%
    tidyHtmlTable(header = header,
                  rgroup = rgroup,
                  hidden_rgroup = "1_rg",
                  label = "test_table")
  parsed_table <- readHTMLTable(as.character(table_str))[["test_table"]]
  expect_equal(ncol(parsed_table), 4)
  expect_equal(nrow(parsed_table), length(mx$value) + length(mx$rgroup %>% unique) - 1)
  expect_match(table_str, "2_rg")
  expect_false(grepl("1_rg", table_str))

  table_str <- mx %>%
    tidyHtmlTable(header = header,
                  tspanner = rgroup,
                  hidden_tspanner = "1_rg",
                  label = "test_table")
  expect_match(table_str, "2_rg")
  expect_false(grepl("1_rg", table_str))

  table_str <- mx %>%
    tidyHtmlTable(header = header,
                  rgroup = rgroup,
                  cgroup = cgroup1,
                  label = "test_table")

  parsed_table <- readHTMLTable(as.character(table_str))[["test_table"]]
  expect_equal(colnames(parsed_table) %>% keep(~grepl("[0-9]", .)) %>% length,
               unique(mx$header) %>% length)
  expect_equal(ncol(parsed_table), 5)
  expect_equal(nrow(parsed_table), length(mx$value) + length(mx$rgroup %>% unique))

  table_str <- mx %>%
    tidyHtmlTable(header = header,
                  rgroup = rgroup,
                  cgroup = starts_with("cgroup"),
                  label = "test_table")

  parsed_table <- readHTMLTable(as.character(table_str))[["test_table"]]
  expect_equal(colnames(parsed_table) %>% keep(~grepl("[0-9]", .)) %>% length,
               unique(mx$header) %>% length)
  # Each cgroup generates a empty cell in-between which is how we detect the
  # cgroup as it adds these for layout purpose
  expect_equal(ncol(parsed_table), 3 + 1 + 2)
  expect_equal(nrow(parsed_table), length(mx$value) + length(mx$rgroup %>% unique))
})



