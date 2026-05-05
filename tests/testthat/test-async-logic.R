library(testthat)
library(mockery)

# ---------------------------------------------------------------------------
# Story 2.2: Async Static Rendering — unit tests for the two exported helpers
# that future_promise() workers call so no ggplot/R6 objects cross the process
# boundary (AC 4, 5).
#
# MOCKING STRATEGY:
#   with_mocked_bindings(.env = asNamespace("astrocalculation")) has a known
#   testthat/pkgload interaction in dev mode: it fails to restore the original
#   closure environments, permanently replacing namespace bindings with the mock
#   and causing failures in subsequent test files.
#
#   We use mockery::stub() instead. stub() intercepts calls at the specific
#   call-site inside the target function without touching the namespace binding,
#   so the original function is untouched after the test concludes.
# ---------------------------------------------------------------------------

# Shared lightweight mock data ------------------------------------------------

make_mock_planet_df <- function() {
  data.frame(
    deg          = c(265.85, 292.81),
    deg_in_sign  = c(25, 22),
    min_in_sign  = c(51, 49),
    sign         = c(9L, 10L),
    speed        = c(1.0, 13.0),
    planet_color = c("black", "black"),
    planet_glyphs = c("Q", "R"),
    font_gpyphs  = c("HamburgSymbols", "HamburgSymbols"),
    font_size    = c(6, 6),
    row.names    = c("sun", "moon")
  )
}

make_mock_aspect_df <- function() {
  data.frame(
    planet1 = character(0),
    planet2 = character(0),
    aspect  = character(0),
    deg_p1  = numeric(0),
    deg_p2  = numeric(0),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# render_natal_chart_to_file()
# ---------------------------------------------------------------------------

test_that("render_natal_chart_to_file returns a valid JPEG path", {
  planet_df <- make_mock_planet_df()
  aspect_df <- make_mock_aspect_df()
  mock_date <- as.POSIXct("1963-12-18 06:33:00", tz = "America/Chicago")

  # Stub draw_whole_sign_chart WITHIN render_natal_chart_to_file so we don't
  # depend on fonts or full ggplot rendering infrastructure in tests.
  stub(render_natal_chart_to_file, "draw_whole_sign_chart",
       function(...) ggplot2::ggplot())

  result_path <- render_natal_chart_to_file(
    planet_position = planet_df,
    chart_name      = "Test Chart",
    date            = mock_date,
    city            = "Taipei",
    country         = "Taiwan",
    timezone        = "Asia/Taipei",
    aspect_table    = aspect_df
  )

  on.exit(unlink(result_path, force = TRUE), add = TRUE)

  expect_type(result_path, "character")
  expect_true(file.exists(result_path))
  expect_true(grepl("\\.jpg$", result_path, ignore.case = TRUE))
  expect_gt(file.info(result_path)$size, 0L)
})

test_that("render_natal_chart_to_file always closes the graphics device", {
  planet_df <- make_mock_planet_df()
  aspect_df <- make_mock_aspect_df()

  dev_count_before <- length(grDevices::dev.list())

  stub(render_natal_chart_to_file, "draw_whole_sign_chart",
       function(...) ggplot2::ggplot())

  path <- render_natal_chart_to_file(
    planet_position = planet_df,
    chart_name      = "Device Test",
    date            = Sys.time(),
    city            = "Taipei",
    country         = "Taiwan",
    timezone        = "Asia/Taipei",
    aspect_table    = aspect_df
  )
  on.exit(unlink(path, force = TRUE), add = TRUE)

  # Device count must return to baseline even though jpeg() was opened inside
  expect_equal(length(grDevices::dev.list()), dev_count_before)
})

# ---------------------------------------------------------------------------
# calculate_and_render_natal_chart()
# Strategy: stub calculation, lookup, and render call-sites so this stays a
# narrow contract test for the future boundary payload.
# ---------------------------------------------------------------------------

test_that("calculate_and_render_natal_chart returns only the JPEG path (AC 5)", {
  tmp_jpeg <- tempfile(fileext = ".jpg")
  writeLines("stub-jpeg", tmp_jpeg)
  on.exit(unlink(tmp_jpeg, force = TRUE), add = TRUE)

  stub(calculate_and_render_natal_chart, "lookup_city_data",
       function(country, city) list(lat = 35.3273, lng = -96.9253,
                                    timezone = "America/Chicago"))
  stub(calculate_and_render_natal_chart, "calculate_planet_position",
       function(...) list(planetary_position = make_mock_planet_df()))
  stub(calculate_and_render_natal_chart, "calculate_aspect",
       function(data) make_mock_aspect_df())
  stub(calculate_and_render_natal_chart, "render_natal_chart_to_file",
       function(...) tmp_jpeg)

  result <- calculate_and_render_natal_chart(
    country          = "United States",
    city             = "Shawnee",
    datetime         = as.POSIXct("1963-12-18 06:33:00", tz = "America/Chicago"),
    chart_name       = "Natal Chart",
    selected_planets = c("sun", "moon", "mercury", "venus", "mars",
                         "jupiter", "saturn", "asc", "mc")
  )

  expect_identical(result, tmp_jpeg)
  expect_true(file.exists(result))
})

test_that("calculate_and_render_natal_chart respects selected_planets filter", {
  tmp_jpeg      <- tempfile(fileext = ".jpg")
  writeLines("stub", tmp_jpeg)
  on.exit(unlink(tmp_jpeg, force = TRUE), add = TRUE)
  captured_data <- NULL

  stub(calculate_and_render_natal_chart, "lookup_city_data",
       function(country, city) list(lat = 35.3273, lng = -96.9253,
                                    timezone = "America/Chicago"))
  stub(calculate_and_render_natal_chart, "calculate_planet_position",
       function(...) list(planetary_position = make_mock_planet_df()))
  stub(calculate_and_render_natal_chart, "calculate_aspect", function(data) {
    captured_data <<- data
    make_mock_aspect_df()
  })
  stub(calculate_and_render_natal_chart, "render_natal_chart_to_file",
       function(...) tmp_jpeg)

  calculate_and_render_natal_chart(
    country          = "United States",
    city             = "Shawnee",
    datetime         = as.POSIXct("1963-12-18 06:33:00", tz = "America/Chicago"),
    chart_name       = "Filter Test",
    selected_planets = c("sun", "moon")
  )

  expect_true("sun"     %in% row.names(captured_data))
  expect_true("moon"    %in% row.names(captured_data))
  expect_false("mars"   %in% row.names(captured_data))
  expect_false("saturn" %in% row.names(captured_data))
})

test_that("calculate_and_render_natal_chart uses lookup fallback values for rendering", {
  tmp_jpeg <- tempfile(fileext = ".jpg")
  writeLines("stub", tmp_jpeg)
  on.exit(unlink(tmp_jpeg, force = TRUE), add = TRUE)
  captured_args <- NULL

  stub(calculate_and_render_natal_chart, "lookup_city_data",
       function(country, city) list(lat = 0, lng = 0, timezone = "UTC"))
  stub(calculate_and_render_natal_chart, "calculate_planet_position",
       function(datetime, timezone, longitude, latitude) {
         captured_args <<- list(
           timezone = timezone,
           longitude = longitude,
           latitude = latitude
         )
         list(planetary_position = make_mock_planet_df())
       })
  stub(calculate_and_render_natal_chart, "calculate_aspect",
       function(data) make_mock_aspect_df())
  stub(calculate_and_render_natal_chart, "render_natal_chart_to_file",
       function(...) tmp_jpeg)

  result <- calculate_and_render_natal_chart(
    country          = "Atlantis",
    city             = "Lost City",
    datetime         = as.POSIXct("2000-01-01", tz = "UTC"),
    chart_name       = "Fallback Test",
    selected_planets = c("sun", "moon")
  )

  expect_equal(captured_args$timezone, "UTC")
  expect_equal(captured_args$latitude, 0)
  expect_equal(captured_args$longitude, 0)
  expect_equal(result, tmp_jpeg)
})
