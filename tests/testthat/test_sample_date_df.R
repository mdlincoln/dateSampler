context("Sample dates onto a data frame")
set.seed(100)
suppressPackageStartupMessages({
  library(lubridate)
})

test_that("Bad inputs are rejected", {
  expect_error(sample_date_df(iris, year_min = NA, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, n = NULL))
  expect_error(sample_date_df(iris, year_min = 1812, year_max = 1800, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, month_min = -2, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, month_min = 25, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, month_max = -2, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, month_max = 25, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, day_min = -2, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, day_min = 35, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, day_max = -2, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, day_max = 35, n = 5))
  expect_error(sample_date_df(iris, year_min = 1830, year_max = 1812, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, month_min = 2, month_max = 1, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, day_min = 2, day_max = 1, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, month_min = 2, month_max = 2, day_min = 30, n = 5))
  expect_error(sample_date_df(iris, year_min = 1812, .p = is_monday, n = 5))
})

test_that("Sample date follows restrictions", {
  expect_true(all(year(sample_date_df(iris, year_min = 1812, n = 5)[["sampled_date"]]) == 1812))
  expect_true(all(year(sample_date_df(iris, year_min = 1812, year_max = 1830, n = 5)[["sampled_date"]]) %in% 1812:1830))
  expect_true(all(day(sample_date_df(iris, year_min = 1812, day_min = 12, day_max = 12, n = 5)[["sampled_date"]]) == 12))
  expect_true(all(day(sample_date_df(iris, year_min = 1812, day_min = 12, day_max = 18, n = 5)[["sampled_date"]]) %in% 12:18))
  expect_true(all(month(sample_date_df(iris, year_min = 1812, month_min = 2, month_max = 2, n = 5)[["sampled_date"]]) == 2))
  expect_true(all(month(sample_date_df(iris, year_min = 1812, month_min = 2, month_max = 10, n = 5)[["sampled_date"]]) %in% 2:10))
  expect_true(all(month(sample_date_df(iris, year_min = 1812, month_min = 2, month_max = 4, day_min = 30, n = 5)[["sampled_date"]]) == 3))

  expect_true(all(wday(sample_date_df(iris, year_min = 1990, .p = list(is_monday), n = 5, quiet = TRUE)[["sampled_date"]]) == 2))
})

test_that("Column customization arguments work.", {
  expect_equal(names(sample_date_df(iris, year_min = 1812, n = 5))[6], "replicate")
  expect_equal(names(sample_date_df(iris, year_min = 1812, n = 5))[7], "sampled_date")
  expect_equal(names(sample_date_df(iris, year_min = 1812, n = 5, .id = "sim_no"))[6], "sim_no")
  expect_equal(names(sample_date_df(iris, year_min = 1812, n = 5, .name = "new_date"))[7], "new_date")
})

test_that("Messages are sent when regenerating bad dates", {
  expect_silent(sample_date_df(iris, year_min = 1812, month_min = 2, month_max = 2, day_min = 28, day_max = 31, n = 5))
  expect_message(sample_date_df(iris, year_min = 1812, month_min = 2, month_max = 2, day_min = 28, day_max = 31, n = 5, quiet = FALSE), regexp = "Regenerating \\d illegal date[sS]\\.\\.\\.")
})
