library('testthat')
library('dplyr')
library('XML')
library('tibble')
library('purrr')
library('glue')

# Add rownames
test_that("Basic tidyHtmlTable functionality", {
  mx <- tribble(~value, ~header, ~name, ~rgroup, ~cgroup1, ~cgroup2,
                     1,       2,     3,       1,        1,        3,
                     2,       3,     4,       1,        2,        3,
                     3,       4,     5,       2,        2,        4) %>%
    mutate_at(vars(starts_with("cgroup")), ~glue("{name} cg", name = .)) %>%
    mutate(rgroup = glue("{name} rg", name = rgroup),
           header = glue("{name} h", name = header))
  table_str <- mx %>%
    tidyHtmlTable(header = header,
                  label = "test_table")
  parsed_table <- readHTMLTable(as.character(table_str))[["test_table"]]
  expect_equal(ncol(parsed_table), 4)
  expect_equal(nrow(parsed_table), length(mx$value))

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

test_that("Correct table sort", {
  header_output_from_pre_v2 <- "<thead>
<tr>
<th style='border-top: 2px solid grey;'></th>
<th colspan='3' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>4 Cylinders</th><th style='border-top: 2px solid grey;; border-bottom: hidden;'>&nbsp;</th>
<th colspan='3' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>6 Cylinders</th><th style='border-top: 2px solid grey;; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>8 Cylinders</th>
</tr>
<tr>
<th style='border-bottom: 1px solid grey;'> </th>
<th style='border-bottom: 1px solid grey; text-align: center;'>3 Gears</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>4 Gears</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>5 Gears</th>
<th style='border-bottom: 1px solid grey; text-align: center;' colspan='1'>&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>3 Gears</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>4 Gears</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>5 Gears</th>
<th style='border-bottom: 1px solid grey; text-align: center;' colspan='1'>&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>3 Gears</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>5 Gears</th>
</tr>
</thead>"


  mtcatr_proc_data <- structure(
    list(cyl = c("4 Cylinders", "4 Cylinders", "4 Cylinders",
                 "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders",
                 "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders",
                 "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders",
                 "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders",
                 "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders",
                 "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders", "4 Cylinders",
                 "4 Cylinders", "4 Cylinders", "4 Cylinders", "6 Cylinders", "6 Cylinders",
                 "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders",
                 "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders",
                 "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders",
                 "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders",
                 "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders",
                 "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders",
                 "6 Cylinders", "6 Cylinders", "6 Cylinders", "6 Cylinders", "8 Cylinders",
                 "8 Cylinders", "8 Cylinders", "8 Cylinders", "8 Cylinders", "8 Cylinders",
                 "8 Cylinders", "8 Cylinders", "8 Cylinders", "8 Cylinders", "8 Cylinders",
                 "8 Cylinders", "8 Cylinders", "8 Cylinders", "8 Cylinders", "8 Cylinders",
                 "8 Cylinders", "8 Cylinders", "8 Cylinders", "8 Cylinders", "8 Cylinders",
                 "8 Cylinders", "8 Cylinders", "8 Cylinders"),
         gear = c("3 Gears",
                  "3 Gears", "3 Gears", "3 Gears", "3 Gears", "3 Gears", "3 Gears",
                  "3 Gears", "3 Gears", "3 Gears", "3 Gears", "3 Gears", "4 Gears",
                  "4 Gears", "4 Gears", "4 Gears", "4 Gears", "4 Gears", "4 Gears",
                  "4 Gears", "4 Gears", "4 Gears", "4 Gears", "4 Gears", "5 Gears",
                  "5 Gears", "5 Gears", "5 Gears", "5 Gears", "5 Gears", "5 Gears",
                  "5 Gears", "5 Gears", "5 Gears", "5 Gears", "5 Gears", "3 Gears",
                  "3 Gears", "3 Gears", "3 Gears", "3 Gears", "3 Gears", "3 Gears",
                  "3 Gears", "3 Gears", "3 Gears", "3 Gears", "3 Gears", "4 Gears",
                  "4 Gears", "4 Gears", "4 Gears", "4 Gears", "4 Gears", "4 Gears",
                  "4 Gears", "4 Gears", "4 Gears", "4 Gears", "4 Gears", "5 Gears",
                  "5 Gears", "5 Gears", "5 Gears", "5 Gears", "5 Gears", "5 Gears",
                  "5 Gears", "5 Gears", "5 Gears", "5 Gears", "5 Gears", "3 Gears",
                  "3 Gears", "3 Gears", "3 Gears", "3 Gears", "3 Gears", "3 Gears",
                  "3 Gears", "3 Gears", "3 Gears", "3 Gears", "3 Gears", "5 Gears",
                  "5 Gears", "5 Gears", "5 Gears", "5 Gears", "5 Gears", "5 Gears",
                  "5 Gears", "5 Gears", "5 Gears", "5 Gears", "5 Gears"),
         per_metric = c("hp",
                        "hp", "hp", "hp", "mpg", "mpg", "mpg", "mpg", "qsec", "qsec",
                        "qsec", "qsec", "hp", "hp", "hp", "hp", "mpg", "mpg", "mpg",
                        "mpg", "qsec", "qsec", "qsec", "qsec", "hp", "hp", "hp", "hp",
                        "mpg", "mpg", "mpg", "mpg", "qsec", "qsec", "qsec", "qsec", "hp",
                        "hp", "hp", "hp", "mpg", "mpg", "mpg", "mpg", "qsec", "qsec",
                        "qsec", "qsec", "hp", "hp", "hp", "hp", "mpg", "mpg", "mpg",
                        "mpg", "qsec", "qsec", "qsec", "qsec", "hp", "hp", "hp", "hp",
                        "mpg", "mpg", "mpg", "mpg", "qsec", "qsec", "qsec", "qsec", "hp",
                        "hp", "hp", "hp", "mpg", "mpg", "mpg", "mpg", "qsec", "qsec",
                        "qsec", "qsec", "hp", "hp", "hp", "hp", "mpg", "mpg", "mpg",
                        "mpg", "qsec", "qsec", "qsec", "qsec"),
         summary_stat = c("Mean",
                          "SD", "Min", "Max", "Mean", "SD", "Min", "Max", "Mean", "SD",
                          "Min", "Max", "Mean", "SD", "Min", "Max", "Mean", "SD", "Min",
                          "Max", "Mean", "SD", "Min", "Max", "Mean", "SD", "Min", "Max",
                          "Mean", "SD", "Min", "Max", "Mean", "SD", "Min", "Max", "Mean",
                          "SD", "Min", "Max", "Mean", "SD", "Min", "Max", "Mean", "SD",
                          "Min", "Max", "Mean", "SD", "Min", "Max", "Mean", "SD", "Min",
                          "Max", "Mean", "SD", "Min", "Max", "Mean", "SD", "Min", "Max",
                          "Mean", "SD", "Min", "Max", "Mean", "SD", "Min", "Max", "Mean",
                          "SD", "Min", "Max", "Mean", "SD", "Min", "Max", "Mean", "SD",
                          "Min", "Max", "Mean", "SD", "Min", "Max", "Mean", "SD", "Min",
                          "Max", "Mean", "SD", "Min", "Max"),
         value = c(97, NA, 97, 97,
                   21.5, NA, 21.5, 21.5, 20, NA, 20, 20, 76, 20.1, 52, 109, 26.9,
                   4.8, 21.4, 33.9, 19.6, 1.5, 18.5, 22.9, 102, 15.6, 91, 113, 28.2,
                   3.1, 26, 30.4, 16.8, 0.1, 16.7, 16.9, 107.5, 3.5, 105, 110, 19.8,
                   2.3, 18.1, 21.4, 19.8, 0.6, 19.4, 20.2, 116.5, 7.5, 110, 123,
                   19.8, 1.6, 17.8, 21, 17.7, 1.1, 16.5, 18.9, 175, NA, 175, 175,
                   19.7, NA, 19.7, 19.7, 15.5, NA, 15.5, 15.5, 194.2, 33.4, 150,
                   245, 15.1, 2.8, 10.4, 19.2, 17.1, 0.8, 15.4, 18, 299.5, 50.2,
                   264, 335, 15.4, 0.6, 15, 15.8, 14.6, 0.1, 14.5, 14.6)),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -96L))

  out_str <- mtcatr_proc_data  %>%
    arrange(per_metric, summary_stat) %>%
    tidyHtmlTable(header = gear,
                  cgroup = cyl,
                  rnames = summary_stat,
                  rgroup = per_metric,
                  skip_removal_warning = TRUE)

  expect_match(out_str, header_output_from_pre_v2, fixed = TRUE)
})
