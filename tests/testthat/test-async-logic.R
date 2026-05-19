library(testthat)

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
#   These tests use explicit namespace binding restoration. This avoids the
#   dev-mode binding leakage that can corrupt later tests.
# ---------------------------------------------------------------------------

# Shared lightweight mock data ------------------------------------------------

make_mock_planet_df <- function() {
  data.frame(
    deg = c(265.85, 292.81),
    deg_in_sign = c(25, 22),
    min_in_sign = c(51, 49),
    sign = c(9L, 10L),
    speed = c(1.0, 13.0),
    planet_color = c("black", "black"),
    planet_glyphs = c("Q", "R"),
    font_glyphs = c("HamburgSymbols", "HamburgSymbols"),
    font_size = c(6, 6),
    row.names = c("sun", "moon")
  )
}

make_mock_aspect_df <- function() {
  data.frame(
    planet = character(0),
    planet2 = character(0),
    aspect = character(0),
    deg_p1 = numeric(0),
    deg_p2 = numeric(0),
    orb1 = numeric(0),
    orb2 = numeric(0),
    separation = character(0),
    draw_line = logical(0),
    stringsAsFactors = FALSE
  )
}

make_mock_payload <- function(planets = c("sun", "moon")) {
  pos_df <- make_mock_planet_df()
  pos_df <- pos_df[row.names(pos_df) %in% planets, , drop = FALSE]
  list(
    planetary_positions = pos_df,
    house_cusps = data.frame(
      whole_sign = rep(0, 12), equal = rep(0, 12),
      placidus = rep(0, 12), koch = rep(0, 12),
      regiomontanus = rep(0, 12)
    ),
    aspects = make_mock_aspect_df(),
    planetary_conditions = data.frame(),
    greek_lots = data.frame(),
    selected_bodies = planets,
    tables = list(
      aspects    = make_mock_aspect_df(),
      conditions = data.frame()
    )
  )
}

with_namespace_bindings <- function(bindings, code, ns = asNamespace("astrocalculation")) {
  code <- substitute(code)
  binding_names <- names(bindings)
  old_values <- mget(binding_names, envir = ns, inherits = FALSE)
  was_locked <- vapply(binding_names, bindingIsLocked, logical(1), env = ns)

  for (name in binding_names) {
    if (was_locked[[name]]) unlockBinding(name, ns)
    assign(name, bindings[[name]], envir = ns)
    if (was_locked[[name]]) lockBinding(name, ns)
  }

  on.exit(
    {
      for (name in rev(binding_names)) {
        if (bindingIsLocked(name, ns)) unlockBinding(name, ns)
        assign(name, old_values[[name]], envir = ns)
        if (was_locked[[name]]) lockBinding(name, ns)
      }
    },
    add = TRUE
  )

  eval.parent(code)
}

with_sequential_future <- function(code) {
  code <- substitute(code)
  previous_plan <- future::plan()
  future::plan(future::sequential)
  on.exit(future::plan(previous_plan), add = TRUE)
  eval.parent(code)
}

# ---------------------------------------------------------------------------
# render_natal_chart_to_file()
# ---------------------------------------------------------------------------

test_that("render_natal_chart_to_file returns a valid JPEG path", {
  planet_df <- make_mock_planet_df()
  aspect_df <- make_mock_aspect_df()
  mock_date <- as.POSIXct("1963-12-18 06:33:00", tz = "America/Chicago")

  result_path <- with_namespace_bindings(
    list(draw_natal_chart = function(...) ggplot2::ggplot()),
    render_natal_chart_to_file(
      planet_position = planet_df,
      chart_name      = "Test Chart",
      date            = mock_date,
      city            = "Taipei",
      country         = "Taiwan",
      timezone        = "Asia/Taipei",
      aspect_table    = aspect_df
    )
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

  path <- with_namespace_bindings(
    list(draw_natal_chart = function(...) ggplot2::ggplot()),
    render_natal_chart_to_file(
      planet_position = planet_df,
      chart_name      = "Device Test",
      date            = Sys.time(),
      city            = "Taipei",
      country         = "Taiwan",
      timezone        = "Asia/Taipei",
      aspect_table    = aspect_df
    )
  )
  on.exit(unlink(path, force = TRUE), add = TRUE)

  # Device count must return to baseline even though jpeg() was opened inside
  expect_equal(length(grDevices::dev.list()), dev_count_before)
})

# ---------------------------------------------------------------------------
# DataManager$update_chart_async()
# Strategy: stub calculation and render call-sites so this stays a narrow
# contract test for the R6 async chart boundary.
# ---------------------------------------------------------------------------

test_that("DataManager$update_chart_async returns path and display tables (AC 5, Story 2.5.3)", {
  tmp_jpeg <- tempfile(fileext = ".jpg")
  writeLines("stub-jpeg", tmp_jpeg)
  on.exit(unlink(tmp_jpeg, force = TRUE), add = TRUE)

  with_namespace_bindings(
    list(
      connect_postgres_db = function() list(conn = TRUE),
      Logger = list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
      lookup_city_data = function(country, city) list(lat = 35.3273, lng = -96.9253, timezone = "America/Chicago"),
      calculate_natal_payload = function(...) make_mock_payload(c("sun", "moon")),
      draw_natal_chart = function(...) ggplot2::ggplot(),
      render_natal_chart_to_file = function(...) tmp_jpeg
    ),
    with_sequential_future({
      r6 <- suppressMessages(DataManager$new())
      on.exit(r6$pool <- NULL, add = TRUE)
      r6$horoscope_datetime <- as.POSIXct("1963-12-18 06:33:00", tz = "America/Chicago")
      r6$horoscope_timezone <- "America/Chicago"
      r6$horoscope_latitude <- 35.3273
      r6$horoscope_longitude <- -96.9253
      r6$horoscope_city <- "Shawnee"
      r6$horoscope_country <- "United States"
      r6$chart_name <- "Natal Chart"
      r6$selected_planets <- c("sun", "moon")

      result <- future::value(r6$update_chart_async())

      expect_type(result, "list")
      expect_named(result, c("path", "tables", "greek_lots"), ignore.order = TRUE)
      expect_identical(result$path, tmp_jpeg)
      expect_true(file.exists(result$path))
      expect_named(result$tables, c("aspects", "conditions"), ignore.order = TRUE)
    })
  )
})

test_that("DataManager$update_chart_async respects selected_planets filter", {
  tmp_jpeg <- tempfile(fileext = ".jpg")
  writeLines("stub", tmp_jpeg)
  on.exit(unlink(tmp_jpeg, force = TRUE), add = TRUE)
  captured_bodies <- NULL

  with_namespace_bindings(
    list(
      connect_postgres_db = function() list(conn = TRUE),
      Logger = list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
      lookup_city_data = function(country, city) list(lat = 35.3273, lng = -96.9253, timezone = "America/Chicago"),
      calculate_natal_payload = function(date, timezone, longitude, latitude,
                                         selected_bodies, house_system) {
        captured_bodies <<- selected_bodies
        make_mock_payload(selected_bodies)
      },
      draw_natal_chart = function(...) ggplot2::ggplot(),
      render_natal_chart_to_file = function(...) tmp_jpeg
    ),
    with_sequential_future({
      r6 <- suppressMessages(DataManager$new())
      on.exit(r6$pool <- NULL, add = TRUE)
      r6$selected_planets <- c("sun")
      future::value(r6$update_chart_async())
    })
  )

  expect_true("sun" %in% captured_bodies)
  expect_false("moon" %in% captured_bodies)
})

test_that("DataManager$update_chart_async refreshes location metadata like update_chart", {
  tmp_jpeg <- tempfile(fileext = ".jpg")
  writeLines("stub", tmp_jpeg)
  on.exit(unlink(tmp_jpeg, force = TRUE), add = TRUE)

  with_namespace_bindings(
    list(
      connect_postgres_db = function() list(conn = TRUE),
      Logger = list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
      lookup_city_data = function(country, city) {
        if (identical(country, "Japan") && identical(city, "Tokyo")) {
          return(list(lat = 35.6895, lng = 139.6917, timezone = "Asia/Tokyo"))
        }

        list(lat = 25.03, lng = 121.56, timezone = "Asia/Taipei")
      },
      calculate_natal_payload = function(...) make_mock_payload(c("sun", "moon")),
      draw_natal_chart = function(...) ggplot2::ggplot(),
      render_natal_chart_to_file = function(...) tmp_jpeg
    ),
    with_sequential_future({
      r6 <- suppressMessages(DataManager$new())
      on.exit(r6$pool <- NULL, add = TRUE)
      r6$horoscope_country <- "Japan"
      r6$horoscope_city <- "Tokyo"

      result <- future::value(r6$update_chart_async())

      expect_equal(result$path, tmp_jpeg)
      expect_equal(r6$horoscope_timezone, "Asia/Tokyo")
      expect_equal(r6$horoscope_latitude, 35.6895)
      expect_equal(r6$horoscope_longitude, 139.6917)
    })
  )
})

test_that("DataManager$update_chart_async uses optional IP-geolocation coordinates", {
  tmp_jpeg <- tempfile(fileext = ".jpg")
  writeLines("stub", tmp_jpeg)
  on.exit(unlink(tmp_jpeg, force = TRUE), add = TRUE)
  captured_args <- NULL

  with_namespace_bindings(
    list(
      connect_postgres_db = function() list(conn = TRUE),
      Logger = list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
      lookup_city_data = function(country, city) list(lat = 35.3273, lng = -96.9253, timezone = "America/Chicago"),
      calculate_natal_payload = function(date, timezone, longitude, latitude,
                                         selected_bodies, house_system) {
        captured_args <<- list(
          timezone  = timezone,
          longitude = longitude,
          latitude  = latitude
        )
        make_mock_payload(selected_bodies)
      },
      draw_natal_chart = function(...) ggplot2::ggplot(),
      render_natal_chart_to_file = function(...) tmp_jpeg
    ),
    with_sequential_future({
      r6 <- suppressMessages(DataManager$new())
      on.exit(r6$pool <- NULL, add = TRUE)
      result <- future::value(r6$update_chart_async(timezone = "UTC", latitude = 0, longitude = 0))
      expect_equal(result$path, tmp_jpeg)
    })
  )

  expect_equal(captured_args$timezone, "UTC")
  expect_equal(captured_args$latitude, 0)
  expect_equal(captured_args$longitude, 0)
})
