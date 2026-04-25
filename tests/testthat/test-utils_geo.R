library(testthat)
library(mockery)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Produces a minimal maxmind() return value for mocking.
# Column is "time_zone" to match the GeoLite2-City MMDB field name used by
# rgeolocate::maxmind() when requested as "time_zone".
mock_maxmind_result <- function(tz = "Europe/London",
                                lat = 51.5,
                                lng = -0.1) {
  data.frame(
    time_zone = tz,
    latitude  = lat,
    longitude = lng,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# get_client_ip
# ---------------------------------------------------------------------------

test_that("get_client_ip returns the first X-Forwarded-For address", {
  mock_session <- list(
    request = list(
      HTTP_X_FORWARDED_FOR = "203.0.113.5, 10.0.0.1",
      REMOTE_ADDR = "192.168.1.1"
    )
  )
  expect_equal(get_client_ip(mock_session), "203.0.113.5")
})

test_that("get_client_ip falls back to REMOTE_ADDR when no forwarded header", {
  mock_session <- list(
    request = list(HTTP_X_FORWARDED_FOR = NULL, REMOTE_ADDR = "203.0.113.99")
  )
  expect_equal(get_client_ip(mock_session), "203.0.113.99")
})

test_that("get_client_ip returns 127.0.0.1 when no IP headers are present", {
  mock_session <- list(request = list(HTTP_X_FORWARDED_FOR = NULL, REMOTE_ADDR = NULL))
  expect_equal(get_client_ip(mock_session), "127.0.0.1")
})

# ---------------------------------------------------------------------------
# get_ip_location — happy path (valid public IP, DB present)
# ---------------------------------------------------------------------------

test_that("get_ip_location returns ok result for valid public IP", {
  stub(get_ip_location, "rgeolocate::maxmind", function(...) {
    mock_maxmind_result("Europe/London", 51.5, -0.1)
  })
  stub(get_ip_location, "file.exists", function(...) TRUE)
  stub(get_ip_location, "system.file", function(...) "/fake/GeoLite2-City.mmdb")

  res <- get_ip_location("8.8.8.8")

  expect_equal(res$status,   "ok")
  expect_equal(res$timezone, "Europe/London")
  expect_equal(res$latitude, 51.5)
})

test_that("get_ip_location returns Asia/Taipei timezone for a TW IP", {
  stub(get_ip_location, "rgeolocate::maxmind", function(...) {
    mock_maxmind_result("Asia/Taipei", 25.05, 121.52)
  })
  stub(get_ip_location, "file.exists", function(...) TRUE)
  stub(get_ip_location, "system.file", function(...) "/fake/GeoLite2-City.mmdb")

  res <- get_ip_location("1.34.0.0")

  expect_equal(res$status,   "ok")
  expect_equal(res$timezone, "Asia/Taipei")
})

# ---------------------------------------------------------------------------
# get_ip_location — fallback conditions (AC 13)
# ---------------------------------------------------------------------------

test_that("get_ip_location falls back when mmdb file is absent", {
  expect_warning(
    res <- get_ip_location("8.8.8.8"),
    "GeoLite2-City.mmdb not found"
  )
  expect_equal(res$status,   "fallback")
  expect_equal(res$timezone, "Asia/Taipei")
  expect_true(is.na(res$latitude))
})

test_that("get_ip_location falls back to Asia/Taipei for NULL ip", {
  res <- get_ip_location(NULL)
  expect_equal(res$status,   "fallback")
  expect_equal(res$timezone, "Asia/Taipei")
})

test_that("get_ip_location falls back to Asia/Taipei for empty string ip", {
  res <- get_ip_location("")
  expect_equal(res$status,   "fallback")
  expect_equal(res$timezone, "Asia/Taipei")
})

test_that("get_ip_location falls back for private loopback IP", {
  res <- get_ip_location("127.0.0.1")
  expect_equal(res$status,   "fallback")
  expect_equal(res$timezone, "Asia/Taipei")
})

test_that("get_ip_location falls back for RFC-1918 IP (10.x)", {
  res <- get_ip_location("10.0.0.1")
  expect_equal(res$status,   "fallback")
})

test_that("get_ip_location falls back for RFC-1918 IP (192.168.x)", {
  res <- get_ip_location("192.168.1.1")
  expect_equal(res$status,   "fallback")
})

test_that("get_ip_location falls back when maxmind returns invalid timezone", {
  stub(get_ip_location, "rgeolocate::maxmind", function(...) {
    mock_maxmind_result(tz = "Not/ATimezone", lat = 0, lng = 0)
  })
  stub(get_ip_location, "file.exists", function(...) TRUE)
  stub(get_ip_location, "system.file", function(...) "/fake/GeoLite2-City.mmdb")

  res <- get_ip_location("8.8.8.8")

  expect_equal(res$status,   "fallback")
  expect_equal(res$timezone, "Asia/Taipei")
})

test_that("get_ip_location falls back and warns when maxmind throws an error", {
  stub(get_ip_location, "rgeolocate::maxmind", function(...) stop("DB read error"))
  stub(get_ip_location, "file.exists", function(...) TRUE)
  stub(get_ip_location, "system.file", function(...) "/fake/GeoLite2-City.mmdb")

  expect_warning(
    res <- get_ip_location("8.8.8.8"),
    "IP geolocation failed"
  )
  expect_equal(res$status,   "fallback")
  expect_equal(res$timezone, "Asia/Taipei")
})

# ---------------------------------------------------------------------------
# Internal helper: .is_private_ip
# ---------------------------------------------------------------------------

test_that(".is_private_ip correctly identifies private ranges", {
  expect_true(astrocalculation:::.is_private_ip("127.0.0.1"))
  expect_true(astrocalculation:::.is_private_ip("10.0.0.1"))
  expect_true(astrocalculation:::.is_private_ip("172.16.0.1"))
  expect_true(astrocalculation:::.is_private_ip("172.31.255.255"))
  expect_true(astrocalculation:::.is_private_ip("192.168.0.1"))
  expect_true(astrocalculation:::.is_private_ip("::1"))
})

test_that(".is_private_ip does not block public IPs", {
  expect_false(astrocalculation:::.is_private_ip("8.8.8.8"))
  expect_false(astrocalculation:::.is_private_ip("1.34.0.0"))
  expect_false(astrocalculation:::.is_private_ip("203.69.0.1"))
})
