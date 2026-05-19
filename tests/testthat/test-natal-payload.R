library(testthat)

# ---------------------------------------------------------------------------
# Story 2.5.1: Calculation Contract Cleanup
#
# Tests for calculate_natal_payload() — the single canonical entry point
# that both the synchronous and asynchronous chart paths resolve to before
# any rendering-only file handoff occurs (AC 1–7).
#
# MOCKING STRATEGY: same namespace-binding pattern as test-async-logic.R.
# All swephR / ephemeris calls are replaced with deterministic stubs so the
# suite runs without external data files.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

FIXTURE_DATE <- as.POSIXct("1988-06-15 12:30:00", tz = "Asia/Taipei")
FIXTURE_TZ <- "Asia/Taipei"
FIXTURE_LON <- 121.52639 # Taipei
FIXTURE_LAT <- 25.05306

# Minimal 3-body planet position data.frame (sun, moon, asc) that lets all
# downstream helpers run without error.
make_fixture_planet_pos <- function() {
  bodies <- c(
    "sun", "moon", "mercury", "venus", "mars", "jupiter", "saturn",
    "uranus", "neptune", "pluto", "chiron", "mean_node", "true_node",
    "asc", "mc", "vertex"
  )

  n <- length(bodies)
  # Spread evenly around the zodiac so no pathological edge cases
  degs <- seq(0, 359, length.out = n) %% 360

  df <- data.frame(
    deg = degs,
    speed = rep(1.0, n),
    sign = as.integer(floor(degs / 30)) + 1L,
    deg_in_sign = as.integer(degs %% 30),
    min_in_sign = rep(0L, n),
    sec_in_sign = rep(0L, n),
    planet_glyphs = LETTERS[seq_len(n)],
    planet_color = rep("black", n),
    font_glyphs = rep("AstroDotBasic", n),
    font_size = rep(6.5, n),
    row.names = bodies,
    stringsAsFactors = FALSE
  )
  df
}

make_fixture_house_cusps <- function(asc_deg = 0) {
  cusp_degs <- (asc_deg + seq(0, 330, by = 30)) %% 360
  data.frame(
    whole_sign    = cusp_degs,
    equal         = cusp_degs,
    placidus      = cusp_degs,
    koch          = cusp_degs,
    regiomontanus = cusp_degs
  )
}

make_fixture_planet_position_list <- function() {
  pos <- make_fixture_planet_pos()
  list(
    planetary_position = pos,
    house_cusps        = make_fixture_house_cusps(asc_deg = pos$deg[match("asc", row.names(pos))])
  )
}

# Minimal conditions data.frame (traditional 7 planets only)
make_fixture_conditions <- function() {
  trad <- c("sun", "moon", "mercury", "venus", "mars", "jupiter", "saturn")
  df <- data.frame(
    house              = 1:7,
    is_in_sect         = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
    element            = c("Fire", "Water", "Air", "Earth", "Fire", "Air", "Earth"),
    is_domicile_lord   = rep(FALSE, 7),
    is_exaltation_lord = rep(FALSE, 7),
    is_triplicity_lord = rep(FALSE, 7),
    is_term_lord       = rep(FALSE, 7),
    is_face_lord       = rep(FALSE, 7),
    is_in_detriment    = rep(FALSE, 7),
    is_in_fall         = rep(FALSE, 7),
    is_peregrine       = rep(TRUE, 7),
    row.names          = trad,
    stringsAsFactors   = FALSE
  )
  df
}

# Minimal Greek Lots data.frame (7 lots)
make_fixture_lots <- function() {
  lot_names <- c("spirit", "fortune", "necessity", "eros", "courage", "victory", "nemesis")
  degs <- seq(15, 195, by = 30)
  data.frame(
    deg = degs,
    speed = rep(0, 7),
    sign = as.integer(floor(degs / 30)) + 1L,
    deg_in_sign = as.integer(degs %% 30),
    min_in_sign = rep(0L, 7),
    sec_in_sign = rep(0L, 7),
    planet_glyphs = letters[1:7],
    planet_color = rep("black", 7),
    font_glyphs = rep("AstroParts", 7),
    font_size = rep(4.8, 7),
    row.names = lot_names,
    stringsAsFactors = FALSE
  )
}

# Empty aspect data.frame with the expected columns
make_fixture_aspects <- function() {
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

# ---------------------------------------------------------------------------
# Namespace-binding helper (same pattern as test-async-logic.R)
# ---------------------------------------------------------------------------

with_natal_payload_mocks <- function(code, extra_bindings = list()) {
  pos_list <- make_fixture_planet_position_list()
  conditions <- make_fixture_conditions()
  lots <- make_fixture_lots()

  default_bindings <- list(
    calculate_planet_position = function(...) pos_list,
    get_planetary_conditions = function(...) conditions,
    calculate_greek_lots = function(...) lots,
    normalize_degree = function(planet_position) {
      asc <- planet_position$planetary_position$deg[
        match("asc", row.names(planet_position$planetary_position))
      ]
      list(
        normalized_planet_degree = (planet_position$planetary_position$deg - asc) %% 360,
        normalized_house_cusps   = planet_position$house_cusps
      )
    },
    find_house_placement = function(norm_deg, norm_cusps, house_system) {
      rep(1L, length(norm_deg))
    },
    calculate_aspect = function(...) make_fixture_aspects()
  )

  bindings <- c(default_bindings, extra_bindings)
  ns <- asNamespace("astrocalculation")
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

  force(code)
}

# ---------------------------------------------------------------------------
# Task 1 & 4: Canonical payload shape — required top-level field names
# ---------------------------------------------------------------------------

test_that("calculate_natal_payload returns list with all required top-level names", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT
    )

    expect_type(payload, "list")
    required_names <- c(
      "planetary_positions", "house_cusps", "aspects",
      "planetary_conditions", "greek_lots", "selected_bodies", "tables"
    )
    expect_true(all(required_names %in% names(payload)),
      info = paste("Missing:", paste(setdiff(required_names, names(payload)), collapse = ", "))
    )
  })
})

# ---------------------------------------------------------------------------
# planetary_positions
# ---------------------------------------------------------------------------

test_that("payload$planetary_positions is a data.frame with required columns", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT
    )

    pp <- payload$planetary_positions
    expect_s3_class(pp, "data.frame")
    for (col in c("deg", "speed", "sign", "deg_in_sign", "min_in_sign", "sec_in_sign")) {
      expect_true(col %in% names(pp), info = paste("Missing column:", col))
    }
  })
})

test_that("payload$planetary_positions is filtered to selected_bodies", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT,
      selected_bodies = c("sun", "moon")
    )

    expect_true("sun" %in% row.names(payload$planetary_positions))
    expect_true("moon" %in% row.names(payload$planetary_positions))
    expect_false("mars" %in% row.names(payload$planetary_positions))
  })
})

# ---------------------------------------------------------------------------
# house_cusps
# ---------------------------------------------------------------------------

test_that("payload$house_cusps is a data.frame with 12 rows", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT
    )

    expect_s3_class(payload$house_cusps, "data.frame")
    expect_equal(nrow(payload$house_cusps), 12L)
  })
})

# ---------------------------------------------------------------------------
# aspects
# ---------------------------------------------------------------------------

test_that("payload$aspects is a data.frame with required columns", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT
    )

    asp <- payload$aspects
    expect_s3_class(asp, "data.frame")
    for (col in c(
      "planet", "planet2", "aspect", "deg_p1", "deg_p2",
      "orb1", "orb2", "separation", "draw_line"
    )) {
      expect_true(col %in% names(asp), info = paste("Missing aspect column:", col))
    }
  })
})

# ---------------------------------------------------------------------------
# planetary_conditions
# ---------------------------------------------------------------------------

test_that("payload$planetary_conditions is a data.frame with required columns", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT
    )

    cond <- payload$planetary_conditions
    expect_s3_class(cond, "data.frame")
    for (col in c(
      "house", "is_in_sect", "element",
      "is_domicile_lord", "is_exaltation_lord",
      "is_in_detriment", "is_in_fall", "is_peregrine"
    )) {
      expect_true(col %in% names(cond), info = paste("Missing conditions column:", col))
    }
  })
})

# ---------------------------------------------------------------------------
# greek_lots
# ---------------------------------------------------------------------------

test_that("payload$greek_lots is a data.frame with 7 rows and a 'house' column", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT
    )

    lots <- payload$greek_lots
    expect_s3_class(lots, "data.frame")
    expect_equal(nrow(lots), 7L)
    expect_true("house" %in% names(lots),
      info = "Greek Lots must carry house placement in payload"
    )
    expected_lots <- c("spirit", "fortune", "necessity", "eros", "courage", "victory", "nemesis")
    expect_true(all(expected_lots %in% row.names(lots)),
      info = paste("Missing lots:", paste(setdiff(expected_lots, row.names(lots)), collapse = ", "))
    )
  })
})

# ---------------------------------------------------------------------------
# selected_bodies
# ---------------------------------------------------------------------------

test_that("payload$selected_bodies echoes the selected_bodies argument", {
  with_natal_payload_mocks({
    bodies <- c("sun", "moon", "mars")
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT,
      selected_bodies = bodies
    )

    expect_identical(payload$selected_bodies, bodies)
  })
})

# ---------------------------------------------------------------------------
# tables — display-ready sub-list (Task 3 / AC 6)
# ---------------------------------------------------------------------------

test_that("payload$tables is a list with 'aspects' and 'conditions' entries", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT
    )

    expect_type(payload$tables, "list")
    expect_true("aspects" %in% names(payload$tables))
    expect_true("conditions" %in% names(payload$tables))
  })
})

test_that("tables$aspects contains only the display columns", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT
    )

    tbl <- payload$tables$aspects
    expect_s3_class(tbl, "data.frame")
    display_cols <- c("planet", "planet2", "aspect", "orb1", "separation", "draw_line")
    expect_true(all(display_cols %in% names(tbl)),
      info = paste("Missing display columns:", paste(setdiff(display_cols, names(tbl)), collapse = ", "))
    )
    # Raw intermediate columns should NOT be present
    expect_false("deg_p1" %in% names(tbl))
    expect_false("orb2" %in% names(tbl))
  })
})

test_that("tables$conditions carries a 'sign' column enriched from planetary positions", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      FIXTURE_DATE, FIXTURE_TZ, FIXTURE_LON, FIXTURE_LAT
    )

    tbl <- payload$tables$conditions
    expect_s3_class(tbl, "data.frame")
    expect_true("sign" %in% names(tbl),
      info = "conditions display table must carry 'sign' column"
    )
  })
})

# ---------------------------------------------------------------------------
# Task 2: Sync vs async shape parity (AC 2, 3)
#
# Both DataManager paths call calculate_natal_payload() with the same arguments
# and must receive structurally identical payloads before rendering.
# ---------------------------------------------------------------------------

payload_shape <- function(payload) {
  list(
    names = names(payload),
    planetary_positions = list(
      names = names(payload$planetary_positions),
      rows = row.names(payload$planetary_positions)
    ),
    house_cusps = names(payload$house_cusps),
    aspects = names(payload$aspects),
    planetary_conditions = names(payload$planetary_conditions),
    greek_lots = names(payload$greek_lots),
    selected_bodies = payload$selected_bodies,
    tables = list(
      names = names(payload$tables),
      aspects = names(payload$tables$aspects),
      conditions = names(payload$tables$conditions)
    )
  )
}

test_that("sync and async DataManager paths resolve the same payload shape", {
  tmp_jpeg <- tempfile(fileext = ".jpg")
  writeLines("stub", tmp_jpeg)
  on.exit(unlink(tmp_jpeg, force = TRUE), add = TRUE)

  captured <- list()
  payload_for <- function(selected_bodies) {
    pos_list <- make_fixture_planet_position_list()
    list(
      planetary_positions = pos_list$planetary_position[
        row.names(pos_list$planetary_position) %in% selected_bodies, ,
        drop = FALSE
      ],
      house_cusps = pos_list$house_cusps,
      aspects = make_fixture_aspects(),
      planetary_conditions = make_fixture_conditions(),
      greek_lots = make_fixture_lots(),
      selected_bodies = selected_bodies,
      tables = list(
        aspects = make_fixture_aspects(),
        conditions = make_fixture_conditions()
      )
    )
  }

  ns <- asNamespace("astrocalculation")
  bindings <- list(
    connect_postgres_db = function() list(conn = TRUE),
    Logger = list(new = function(pool) {
      list(
        log_info = function(...) NULL,
        log_error = function(...) NULL
      )
    }),
    lookup_city_data = function(country, city) {
      list(lat = FIXTURE_LAT, lng = FIXTURE_LON, timezone = FIXTURE_TZ)
    },
    calculate_natal_payload = function(date, timezone, longitude, latitude,
                                       selected_bodies, house_system) {
      payload <- payload_for(selected_bodies)
      captured[[length(captured) + 1L]] <<- list(
        args = list(
          date = date,
          timezone = timezone,
          longitude = longitude,
          latitude = latitude,
          selected_bodies = selected_bodies,
          house_system = house_system
        ),
        shape = payload_shape(payload)
      )
      payload
    },
    draw_natal_chart = function(...) ggplot2::ggplot(),
    render_natal_chart_to_file = function(...) tmp_jpeg
  )

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

  previous_plan <- future::plan()
  future::plan(future::sequential)
  on.exit(future::plan(previous_plan), add = TRUE)

  make_chart_manager <- function() {
    r6 <- suppressMessages(DataManager$new())
    r6$horoscope_datetime <- FIXTURE_DATE
    r6$horoscope_country <- "Taiwan"
    r6$horoscope_city <- "Taipei"
    r6$chart_name <- "Fixture Chart"
    r6$selected_planets <- c("sun", "moon", "mars")
    r6$house_system <- "whole_sign"
    r6
  }

  sync_r6 <- make_chart_manager()
  async_r6 <- make_chart_manager()
  on.exit(sync_r6$pool <- NULL, add = TRUE)
  on.exit(async_r6$pool <- NULL, add = TRUE)

  # Ignore initialize-time update_chart() calls; compare explicit sync/async runs.
  captured <- list()

  sync_r6$update_chart()
  async_result <- future::value(async_r6$update_chart_async())

  expect_identical(async_result$path, tmp_jpeg)
  expect_length(captured, 2L)
  expect_identical(captured[[1]]$args, captured[[2]]$args)
  expect_identical(captured[[1]]$shape, captured[[2]]$shape)
})

# ---------------------------------------------------------------------------
# Task 4: Async boundary — no R6 / ggplot objects in worker result (AC 5)
# ---------------------------------------------------------------------------

test_that("update_chart_async returns a character path, not an R6 or ggplot object", {
  tmp_jpeg <- tempfile(fileext = ".jpg")
  writeLines("stub", tmp_jpeg)
  on.exit(unlink(tmp_jpeg, force = TRUE), add = TRUE)

  ns <- asNamespace("astrocalculation")
  bindings <- list(
    connect_postgres_db = function() list(conn = TRUE),
    Logger = list(new = function(pool) {
      list(
        log_info  = function(...) NULL,
        log_error = function(...) NULL
      )
    }),
    lookup_city_data = function(country, city) {
      list(lat = FIXTURE_LAT, lng = FIXTURE_LON, timezone = FIXTURE_TZ)
    },
    calculate_natal_payload = function(...) {
      pos_list <- make_fixture_planet_position_list()
      list(
        planetary_positions = pos_list$planetary_position[c("sun", "moon"), ],
        house_cusps = pos_list$house_cusps,
        aspects = make_fixture_aspects(),
        planetary_conditions = make_fixture_conditions(),
        greek_lots = make_fixture_lots(),
        selected_bodies = c("sun", "moon"),
        tables = list(
          aspects = make_fixture_aspects(),
          conditions = data.frame()
        )
      )
    },
    draw_natal_chart = function(...) ggplot2::ggplot(),
    render_natal_chart_to_file = function(...) tmp_jpeg
  )

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

  previous_plan <- future::plan()
  future::plan(future::sequential)
  on.exit(future::plan(previous_plan), add = TRUE)

  r6 <- suppressMessages(DataManager$new())
  on.exit(r6$pool <- NULL, add = TRUE)
  r6$horoscope_datetime <- FIXTURE_DATE
  r6$horoscope_timezone <- FIXTURE_TZ
  r6$horoscope_latitude <- FIXTURE_LAT
  r6$horoscope_longitude <- FIXTURE_LON
  r6$selected_planets <- c("sun", "moon")

  result <- future::value(r6$update_chart_async())

  # Result must be a serializable list with a path and tables — not R6 or ggplot
  expect_type(result, "list")
  expect_named(result, c("path", "tables", "greek_lots"), ignore.order = TRUE)
  expect_false(inherits(result, "R6"))
  expect_false(inherits(result, "ggplot"))
  expect_true(file.exists(result$path))
})

# ---------------------------------------------------------------------------
# Task 4: Regression test with deterministic fixture (AC 7)
#
# Verifies that the payload structure is stable for a fixed birth
# date / time / location.  Inner calculation functions are stubbed so the
# test does not depend on swephR ephemeris files.
# ---------------------------------------------------------------------------

test_that("calculate_natal_payload produces stable structure for deterministic fixture", {
  with_natal_payload_mocks({
    payload <- calculate_natal_payload(
      date = as.POSIXct("1988-06-15 12:30:00", tz = "Asia/Taipei"),
      timezone = "Asia/Taipei",
      longitude = 121.52639,
      latitude = 25.05306,
      selected_bodies = c(
        "sun", "moon", "mercury", "venus", "mars",
        "jupiter", "saturn", "asc", "mc",
        "spirit", "fortune"
      ),
      house_system = "whole_sign"
    )

    # Top-level keys
    expect_setequal(
      names(payload),
      c(
        "planetary_positions", "house_cusps", "aspects",
        "planetary_conditions", "greek_lots", "selected_bodies", "tables"
      )
    )

    # Positions include only the requested bodies
    expect_true(all(c("sun", "moon", "spirit", "fortune") %in% row.names(payload$planetary_positions)))
    expect_false("neptune" %in% row.names(payload$planetary_positions))

    # Greek Lots always 7 rows regardless of selected_bodies filter
    expect_equal(nrow(payload$greek_lots), 7L)

    # house column present in greek_lots
    expect_true("house" %in% names(payload$greek_lots))

    # tables$aspects has exactly the display columns
    expect_setequal(
      names(payload$tables$aspects),
      c("planet", "planet2", "aspect", "orb1", "separation", "draw_line")
    )

    # tables$conditions has a sign column
    expect_true("sign" %in% names(payload$tables$conditions))
  })
})
