library(testthat)

test_that("lookup_city_data fetches correct coordinates and timezone", {

  # Test 1: Taipei, Taiwan (Known Data)
  # From your data: Taiwan/Taipei -> Asia/Taipei
  res <- lookup_city_data("Taiwan", "Taipei")

  expect_equal(res$timezone, "Asia/Taipei")
  expect_true(is.numeric(res$lat))
  expect_true(is.numeric(res$lng))
  # Approx check (Taipei is roughly 25N, 121E)
  expect_gt(res$lat, 24)
  expect_lt(res$lat, 26)

  # Test 2: London, United Kingdom (Join Check)
  # Ensure it correctly joins country "United Kingdom"
  res_london <- lookup_city_data("United Kingdom", "London")
  expect_equal(res_london$timezone, "Europe/London")

  # Test 3: Fallback Behavior (Non-existent City)
  expect_warning(
    res_fail <- lookup_city_data("Atlantis", "Lost City"),
    "Location lookup failed"
  )
  expect_equal(res_fail$timezone, "UTC")
  expect_equal(res_fail$lat, 0)
})

test_that("DataManager updates timezone upon city change", {

  # Initialize Manager
  r6 <- suppressMessages(DataManager$new())

  # 1. Set to Tokyo, Japan manually
  r6$horoscope_country <- "Japan"
  r6$horoscope_city <- "Tokyo"

  # 2. Trigger Update
  # This should call lookup -> find Tokyo -> update TZ to Asia/Tokyo
  r6$update_chart()

  # 3. Verify
  expect_equal(r6$horoscope_timezone, "Asia/Tokyo")
  # Verify Coordinates updated (Tokyo is approx 35N)
  expect_gt(r6$horoscope_latitude, 34)

  # 4. Change to New York, United States
  r6$horoscope_country <- "United States"
  r6$horoscope_city <- "New York City"
  r6$update_chart()

  # 5. Verify Timezone switched automatically
  expect_equal(r6$horoscope_timezone, "America/New_York")
})

test_that("Frontend helper functions return correct bilingual lists", {

  # 1. Test Country Options
  countries <- get_country_options()

  # Check Taiwan entry
  # We expect the name (key) to contain Chinese and value to be English
  expect_true("Taiwan" %in% countries)
  # Check if the label for "Taiwan" contains "台灣"
  # names(countries) returns the labels
  taiwan_label <- names(countries)[countries == "Taiwan"]
  expect_match(taiwan_label, "台灣")
  expect_match(taiwan_label, "Taiwan")

  # 2. Test City Options (Taipei)
  cities <- get_city_options("Taiwan")
  expect_true("Taipei" %in% cities)

  taipei_label <- names(cities)[cities == "Taipei"]
  expect_match(taipei_label, "台北市")
  expect_match(taipei_label, "Taipei")

  # 3. Test Empty Case
  empty_cities <- get_city_options("Antarctica") # Assuming no cities there in DB
  expect_length(empty_cities, 0)
})

test_that("DataManager returns correct bilingual lists", {
  r6 <- suppressMessages(DataManager$new())

  # 1. Test Country Options
  countries <- r6$get_country_options()

  expect_true("Taiwan" %in% countries)
  # Check label contains Chinese
  taiwan_label <- names(countries)[countries == "Taiwan"]
  expect_match(taiwan_label, "台灣")

  # 2. Test City Options
  cities <- r6$get_city_options("Taiwan")
  expect_true("Taipei" %in% cities)

  taipei_label <- names(cities)[cities == "Taipei"]
  expect_match(taipei_label, "台北市")
})
