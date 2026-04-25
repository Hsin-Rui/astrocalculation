library(testthat)
library(mockery)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Produces a minimal DBI::dbGetQuery return value for mocking.
# Columns match the SQL SELECT: time_zone, latitude, longitude.
mock_geo_result <- function(tz = "Europe/London", lat = 51.5, lng = -0.1) {
  data.frame(time_zone = tz, latitude = lat, longitude = lng,
             stringsAsFactors = FALSE)
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
# get_ip_location — happy path
# ---------------------------------------------------------------------------

test_that("get_ip_location returns ok result for a valid public IP", {
  fake_pool <- structure(list(), class = "Pool")
  stub(get_ip_location, "connect_postgres_ipgeo_db", function() fake_pool)
  stub(get_ip_location, "pool::poolClose",           function(...) invisible(NULL))
  stub(get_ip_location, "DBI::dbGetQuery", function(pool, sql, params)
    mock_geo_result("Europe/London", 51.5, -0.1))

  res <- get_ip_location("8.8.8.8")

  expect_equal(res$status,   "ok")
  expect_equal(res$timezone, "Europe/London")
  expect_equal(res$latitude,  51.5)
})

test_that("get_ip_location returns Asia/Taipei timezone for a TW IP", {
  fake_pool <- structure(list(), class = "Pool")
  stub(get_ip_location, "connect_postgres_ipgeo_db", function() fake_pool)
  stub(get_ip_location, "pool::poolClose",           function(...) invisible(NULL))
  stub(get_ip_location, "DBI::dbGetQuery", function(pool, sql, params)
    mock_geo_result("Asia/Taipei", 25.05, 121.52))

  res <- get_ip_location("1.34.0.0")

  expect_equal(res$status,   "ok")
  expect_equal(res$timezone, "Asia/Taipei")
})

# ---------------------------------------------------------------------------
# get_ip_location — fallback conditions (AC 13)
# ---------------------------------------------------------------------------

test_that("get_ip_location falls back when DB returns zero rows", {
  fake_pool <- structure(list(), class = "Pool")
  stub(get_ip_location, "connect_postgres_ipgeo_db", function() fake_pool)
  stub(get_ip_location, "pool::poolClose",           function(...) invisible(NULL))
  stub(get_ip_location, "DBI::dbGetQuery", function(pool, sql, params)
    data.frame(time_zone = character(0), latitude = numeric(0), longitude = numeric(0)))

  res <- get_ip_location("8.8.8.8")
  expect_equal(res$status,   "fallback")
  expect_equal(res$timezone, "Asia/Taipei")
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

test_that("get_ip_location falls back for loopback IP", {
  res <- get_ip_location("127.0.0.1")
  expect_equal(res$status,   "fallback")
  expect_equal(res$timezone, "Asia/Taipei")
})

test_that("get_ip_location falls back for RFC-1918 IP (10.x)", {
  res <- get_ip_location("10.0.0.1")
  expect_equal(res$status, "fallback")
})

test_that("get_ip_location falls back for RFC-1918 IP (192.168.x)", {
  res <- get_ip_location("192.168.1.1")
  expect_equal(res$status, "fallback")
})

test_that("get_ip_location falls back when DB returns an invalid timezone", {
  fake_pool <- structure(list(), class = "Pool")
  stub(get_ip_location, "connect_postgres_ipgeo_db", function() fake_pool)
  stub(get_ip_location, "pool::poolClose",           function(...) invisible(NULL))
  stub(get_ip_location, "DBI::dbGetQuery", function(pool, sql, params)
    mock_geo_result(tz = "Not/ATimezone", lat = 0, lng = 0))

  res <- get_ip_location("8.8.8.8")

  expect_equal(res$status,   "fallback")
  expect_equal(res$timezone, "Asia/Taipei")
})

test_that("get_ip_location falls back and warns when DB connection fails", {
  stub(get_ip_location, "connect_postgres_ipgeo_db",
       function() stop("Connection refused"))

  expect_warning(
    res <- get_ip_location("8.8.8.8"),
    "IP geolocation failed"
  )
  expect_equal(res$status,   "fallback")
  expect_equal(res$timezone, "Asia/Taipei")
})

test_that("get_ip_location falls back and warns when DBI::dbGetQuery throws", {
  fake_pool <- structure(list(), class = "Pool")
  stub(get_ip_location, "connect_postgres_ipgeo_db", function() fake_pool)
  stub(get_ip_location, "pool::poolClose",           function(...) invisible(NULL))
  stub(get_ip_location, "DBI::dbGetQuery",
       function(...) stop("query execution failed"))

  expect_warning(
    res <- get_ip_location("5.5.5.5"),
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

