library(testthat)
library(lubridate)

test_that("add_datetime works", {
  start <- as.POSIXct("2023-01-01 12:00:00", tz = "UTC")

  # 1. Test Days (Fix verification)
  expect_equal(add_datetime(start, "Days", 1), as.POSIXct("2023-01-02 12:00:00", tz = "UTC"))

  # 2. Test Hours
  expect_equal(add_datetime(start, "Hours", 2), as.POSIXct("2023-01-01 14:00:00", tz = "UTC"))

  # 3. Test Minutes
  expect_equal(add_datetime(start, "Minutes", 30), as.POSIXct("2023-01-01 12:30:00", tz = "UTC"))

  # 4. Test Months (Edge case: End of Month)
  jan_end <- as.POSIXct("2023-01-31 12:00:00", tz = "UTC")
  # Note: months() in R can result in NA if the target day doesn't exist (e.g., Feb 31)
  # unless handled by lubridate or specific logic. Your design uses base months().
  expect_s3_class(add_datetime(jan_end, "Months", 1), "POSIXct")
  expect_equal(add_datetime(jan_end, "Months", 1), as.POSIXct("2023-02-28 12:00:00", tz = "UTC"))
  # 5. Test Years (Default case)
  expect_equal(add_datetime(start, "Years", 1), as.POSIXct("2024-01-01 12:00:00", tz = "UTC"))
})

test_that("Epic 1: minus_datetime works", {
  start <- as.POSIXct("2023-01-02 12:00:00", tz = "UTC")

  # 1. Test Days
  expect_equal(minus_datetime(start, "Days", 1), as.POSIXct("2023-01-01 12:00:00", tz = "UTC"))

  # 2. Test Hours
  expect_equal(minus_datetime(start, "Hours", 1), as.POSIXct("2023-01-02 11:00:00", tz = "UTC"))

  # 3. Test Minutes
  expect_equal(minus_datetime(start, "Minutes", 30), as.POSIXct("2023-01-02 11:30:00", tz = "UTC"))

  # 4. Test Months
  expect_equal(minus_datetime(start, "Months", 1), as.POSIXct("2022-12-02 12:00:00", tz = "UTC"))

  # 5. Test Years
  expect_equal(minus_datetime(start, "Years", 1), as.POSIXct("2022-01-02 12:00:00", tz = "UTC"))
})

test_that("DataManager integration of add and minus datetime", {
  r6 <- DataManager$new()

  # Set a fixed start time for testing
  test_time <- as.POSIXct("2023-12-01 12:00:00", tz = "Asia/Taipei")
  r6$horoscope_datetime <- test_time

  # Action: Add 1 Day
  # This calls add_datetime(test_time, "Days", 1)
  expect_silent(r6$adjust_time(operation = "add", value = 1, unit = "Days"))

  # Verification
  expected_time <- as.POSIXct("2023-12-02 12:00:00", tz = "Asia/Taipei")
  expect_equal(r6$horoscope_datetime, expected_time)

  # Verify that the chart actually updated (not NA)
  expect_false(any(is.na(r6$planet_position$planetary_position$deg)))
})
