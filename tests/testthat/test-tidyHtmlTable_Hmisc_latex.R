library(testthat)
library(dplyr)
library(tibble)
library(purrr)
library(glue)
library(XML)
library(xml2)
library(stringr)

test_that("Works with Hmisc::latex", {
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

  skip_if_not_installed("tidyr")
  expect_output(mtcatr_proc_data  %>%
                  arrange(desc(cyl), gear) %>%
                  mutate(per_metric = factor(per_metric, levels = c("qsec", "hp", "mpg"))) %>%
                  tidyHtmlTable(header = summary_stat,
                                cgroup = per_metric,
                                rnames = gear,
                                rgroup = cyl,
                                skip_removal_warning = TRUE,
                                label = "test_table",
                                rowlabel = "row",
                                table_fn = Hmisc::latex,
                                file = ""),
                regexp = "8 Cylinders")
})