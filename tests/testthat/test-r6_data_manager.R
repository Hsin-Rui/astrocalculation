library(testthat)

test_that("login_with_google sets user state and returns session token", {
  calls <- list()

  with_mocked_bindings(
    {
      with_mocked_bindings(
        {
          r6 <- suppressMessages(DataManager$new())
          on.exit(
            {
              r6$pool <- NULL
            },
            add = TRUE
          )

          token <- r6$login_with_google("a@b.com", "gid-1", "Alice")

          expect_equal(token, "session-token")
          expect_equal(r6$user_id, "uid-123")
          expect_equal(r6$user_profile$display_name, "Display")
          expect_equal(r6$user_library$name, "Lib")
          expect_equal(calls$oauth$name, "Alice")
          expect_equal(calls$create_session, "uid-123")
        },
        poolWithTransaction = function(pool, code) code(NULL),
        .env = asNamespace("pool")
      )
    },
    connect_postgres_db = function() list(conn = TRUE),
    Logger = list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
    lookup_city_data = function(country, city) data.frame(lat = 0, lng = 0, timezone = "UTC"),
    calculate_planet_position = function(...) list(planetary_position = data.frame(dummy = 1)),
    calculate_aspect = function(data) data.frame(),
    draw_whole_sign_chart = function(...) list(),
    auth_handle_oauth_user = function(pool, email, google_id, name) {
      calls$oauth <<- list(email = email, google_id = google_id, name = name)
      "uid-123"
    },
    auth_create_session = function(pool, uid) {
      calls$create_session <<- uid
      "session-token"
    },
    db_get_profile = function(pool, uid) data.frame(display_name = "Display", birth_timestamp = as.POSIXct("2020-01-01", tz = "UTC"), timezone = "UTC", city_name = "City", country = "Country", lat = 0, lng = 0),
    db_get_library = function(pool, uid) data.frame(entity_id = "lib1", name = "Lib", birth_timestamp = as.POSIXct("2020-01-01", tz = "UTC"), timezone = "UTC", city_name = "City", country = "Country", lat = 0, lng = 0),
    auth_validate_session = function(pool, token) "uid-123",
    poolWithTransaction = function(pool, code) code(NULL),
    .env = asNamespace("astrocalculation")
  )
})

test_that("validate_session updates user_id and refreshes data", {
  with_mocked_bindings(
    {
      with_mocked_bindings(
        {
          r6 <- suppressMessages(DataManager$new())
          on.exit(
            {
              r6$pool <- NULL
            },
            add = TRUE
          )
          res <- r6$validate_session("tok")

          expect_equal(res, "uid-validated")
          expect_equal(r6$user_id, "uid-validated")
          expect_equal(r6$user_profile$display_name, "Display")
        },
        poolWithTransaction = function(pool, code) code(NULL),
        .env = asNamespace("pool")
      )
    },
    connect_postgres_db = function() list(conn = TRUE),
    Logger = list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
    lookup_city_data = function(country, city) data.frame(lat = 0, lng = 0, timezone = "UTC"),
    calculate_planet_position = function(...) list(planetary_position = data.frame(dummy = 1)),
    calculate_aspect = function(data) data.frame(),
    draw_whole_sign_chart = function(...) list(),
    auth_validate_session = function(pool, token) "uid-validated",
    db_get_profile = function(pool, uid) data.frame(display_name = "Display", birth_timestamp = as.POSIXct("2020-01-01", tz = "UTC"), timezone = "UTC", city_name = "City", country = "Country", lat = 0, lng = 0),
    db_get_library = function(pool, uid) data.frame(entity_id = "lib1", name = "Lib", birth_timestamp = as.POSIXct("2020-01-01", tz = "UTC"), timezone = "UTC", city_name = "City", country = "Country", lat = 0, lng = 0),
    poolWithTransaction = function(pool, code) code(NULL),
    .env = asNamespace("astrocalculation")
  )
})

test_that("adjust_time delegates to add/minus helpers and refreshes chart", {
  calls <- list()

  with_mocked_bindings(
    {
      with_mocked_bindings(
        {
          r6 <- suppressMessages(DataManager$new())
          on.exit(
            {
              r6$pool <- NULL
            },
            add = TRUE
          )
          r6$horoscope_datetime <- as.POSIXct("2020-01-01", tz = "UTC")

          r6$adjust_time("add", 1, "Days")
          expect_equal(calls$add$unit, "Days")
          expect_equal(calls$add$value, 1)

          r6$adjust_time("minus", 2, "Days")
          expect_equal(calls$minus$value, 2)
        },
        poolWithTransaction = function(pool, code) code(NULL),
        .env = asNamespace("pool")
      )
    },
    connect_postgres_db = function() list(conn = TRUE),
    Logger = list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
    lookup_city_data = function(country, city) data.frame(lat = 0, lng = 0, timezone = "UTC"),
    calculate_planet_position = function(...) list(planetary_position = data.frame(dummy = 1)),
    calculate_aspect = function(data) data.frame(),
    draw_whole_sign_chart = function(...) list(),
    add_datetime = function(time, unit, value) {
      calls$add <<- list(time = time, unit = unit, value = value)
      time + 1
    },
    minus_datetime = function(time, unit, value) {
      calls$minus <<- list(time = time, unit = unit, value = value)
      time - 1
    },
    db_get_profile = function(pool, uid) data.frame(display_name = "Display", birth_timestamp = as.POSIXct("2020-01-01", tz = "UTC"), timezone = "UTC", city_name = "City", country = "Country", lat = 0, lng = 0),
    db_get_library = function(pool, uid) data.frame(entity_id = "lib1", name = "Lib", birth_timestamp = as.POSIXct("2020-01-01", tz = "UTC"), timezone = "UTC", city_name = "City", country = "Country", lat = 0, lng = 0),
    poolWithTransaction = function(pool, code) code(NULL),
    .env = asNamespace("astrocalculation")
  )
})
