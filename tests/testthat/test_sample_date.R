context("Sample dates")
library(lubridate)

test_that("Bad inputs are rejected", {
  expect_error(sample_date(NA, 1, 2, 3))
  expect_error(sample_date(1812, 13, NA, 3))
  expect_error(sample_date(1812, NA, 50, 5))
})

test_that("Sample date follows restrictions", {
  expect_true(all(year(sample_date(1812, NA, NA, 5)) == 1812))
})