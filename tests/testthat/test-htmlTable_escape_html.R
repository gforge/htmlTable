library(testthat)

test_that("HTML code is properly escaped", {
  expect_match(
    object = htmlTable(data.frame(a = "<3"),
                       rnames = FALSE,
                       escape.html = TRUE
    ),
    regexp = "&lt;3"
  )

  df_test <- data.frame(
    a = c("<3", "<3"),
    b = c("&2", ">2"),
    stringsAsFactors = FALSE
  )
  matrix_test <- as.matrix(df_test,
                           ncol = 2
  )

  getCellContext <- function(tout) {
    tout %>%
      str_split("\n") %>%
      extract2(1) %>%
      as.list() %>%
      c(collapse = "_") %>%
      do.call(paste, .) %>%
      str_replace(".*<tbody>(.+)</tbody>.*", "\\1") %>%
      str_split("<td") %>%
      extract2(1) %>%
      Filter(function(x) grepl("</td>", x), .) %>%
      str_replace(".*>([^<]+)</td>.*", "\\1")
  }

  expect_equivalent(
    htmlTable(df_test,
              rnames = FALSE,
              escape.html = TRUE
    ) %>% getCellContext(),
    htmlEscape(c(df_test[1,], df_test[2,]))
  )

  expect_equivalent(
    htmlTable(matrix_test,
              rnames = FALSE,
              escape.html = TRUE
    ) %>% getCellContext(),
    htmlEscape(c(df_test[1,], df_test[2,]))
  )

  tibble_test <- tibble::as_tibble(df_test)
  expect_equivalent(
    htmlTable(tibble_test,
              rnames = FALSE,
              escape.html = TRUE
    ) %>% getCellContext(),
    htmlEscape(c(df_test[1,], df_test[2,]))
  )

  expect_equal(prEscapeHtml("$")[[1]], "&#36;")
})
