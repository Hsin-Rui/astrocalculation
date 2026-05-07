library(testthat)

# ── Load Chinese test fixtures from YAML ─────────────────────────────────────
# All Chinese text lives in fixtures/tarot_test_fixtures.yaml so this file
# contains only ASCII and fixture references.
fix <- yaml::read_yaml(testthat::test_path("fixtures", "tarot_test_fixtures.yaml"))

# ── validate_interpretation ───────────────────────────────────────────────────

VALID_JSON_6 <- jsonlite::toJSON(fix$valid_json_6, auto_unbox = TRUE)

test_that("validate_interpretation returns valid=TRUE for well-formed 6-field JSON", {
  result <- validate_interpretation(VALID_JSON_6)
  expect_true(result$valid)
  expect_equal(result$data$title,         fix$valid_json_6$title)
  expect_equal(result$data$general,       fix$valid_json_6$general)
  expect_equal(result$data$work,          fix$valid_json_6$work)
  expect_equal(result$data$health,        fix$valid_json_6$health)
  expect_equal(result$data$relationships, fix$valid_json_6$relationships)
})

test_that("validate_interpretation strips markdown code fences", {
  json <- paste0("```json\n", VALID_JSON_6, "\n```")
  result <- validate_interpretation(json)
  expect_true(result$valid)
  expect_equal(result$data$title, fix$valid_json_6$title)
})

test_that("validate_interpretation returns valid=FALSE for non-JSON input", {
  result <- validate_interpretation("not json at all")
  expect_false(result$valid)
  expect_match(result$error, "schema", ignore.case = TRUE)
})

test_that("validate_interpretation returns valid=FALSE when a field is missing", {
  # relationships field missing
  json <- '{"title":"T","body":"B","general":"G","work":"W","health":"H"}'
  result <- validate_interpretation(json)
  expect_false(result$valid)
  expect_match(result$error, "schema", ignore.case = TRUE)
})

test_that("validate_interpretation rejects old 3-field contract (wisdom_tag)", {
  json <- '{"title":"T","body":"B","wisdom_tag":"W"}'
  result <- validate_interpretation(json)
  expect_false(result$valid)
})

test_that("validate_interpretation returns valid=FALSE for empty string", {
  result <- validate_interpretation("")
  expect_false(result$valid)
})

test_that("validate_interpretation returns valid=FALSE for extra fields", {
  json <- paste0(
    '{"title":"T","body":"B","general":"G","work":"W","health":"H",',
    '"relationships":"R","extra":"nope"}'
  )
  result <- validate_interpretation(json)
  expect_false(result$valid)
  expect_match(result$error, "schema", ignore.case = TRUE)
})

test_that("validate_interpretation returns valid=FALSE for empty required values", {
  json <- '{"title":"","body":"B","general":"G","work":"W","health":"H","relationships":"R"}'
  result <- validate_interpretation(json)
  expect_false(result$valid)
})

test_that("validate_interpretation rejects numeric field values", {
  json <- '{"title":42,"body":"B","general":"G","work":"W","health":"H","relationships":"R"}'
  result <- validate_interpretation(json)
  expect_false(result$valid)
  expect_match(result$error, "schema", ignore.case = TRUE)
})


# ── get_tarot_interpretation: fallback paths ──────────────────────────────────

test_that("get_tarot_interpretation returns 6-field fallback when api_key is NULL", {
  result <- get_tarot_interpretation(
    card_name    = fix$card_names$fool,
    card_meanings = fix$card_meanings$fool,
    api_key      = NULL
  )
  expect_type(result, "list")
  expect_named(result, c("title", "body", "general", "work", "health", "relationships", "is_local_fallback"))
  expect_equal(result$title, fix$card_names$fool)
  expect_match(result$body,    fix$card_meanings$fool[[1]])
  expect_match(result$general, fix$card_meanings$fool[[1]])
  expect_true(isTRUE(result$is_local_fallback))
})

test_that("get_tarot_interpretation returns fallback when api_key is empty string", {
  result <- get_tarot_interpretation(
    card_name    = fix$card_names$star,
    card_meanings = fix$card_meanings$star,
    api_key      = ""
  )
  expect_equal(result$title, fix$card_names$star)
  expect_named(result, c("title", "body", "general", "work", "health", "relationships", "is_local_fallback"))
  expect_true(isTRUE(result$is_local_fallback))
})

test_that("get_tarot_interpretation returns fallback when api_key is whitespace", {
  result <- get_tarot_interpretation(
    card_name    = fix$card_names$moon,
    card_meanings = fix$card_meanings$moon,
    api_key      = "   "
  )
  expect_equal(result$title, fix$card_names$moon)
})

test_that("get_tarot_interpretation empty meanings fallback is safe", {
  result <- get_tarot_interpretation(
    card_name = "",
    card_meanings = character(0),
    api_key = NULL
  )

  expect_equal(result$title, "Daily Tarot")
  expect_match(result$body, "No interpretation available")
  expect_match(result$general, "No interpretation available")
})


# ── get_tarot_interpretation: mocked HTTP success ────────────────────────────

test_that("get_tarot_interpretation parses valid 6-field LLM response", {
  llm_content <- jsonlite::toJSON(fix$mock_llm_response, auto_unbox = TRUE)
  api_body    <- jsonlite::toJSON(list(
    choices = list(list(message = list(content = llm_content)))
  ), auto_unbox = TRUE)

  fake_resp <- structure(
    list(
      status_code = 200L,
      body = charToRaw(api_body),
      headers = list(`content-type` = "application/json")
    ),
    class = "httr2_response"
  )

  mockery::stub(get_tarot_interpretation, "httr2::req_perform", function(...) fake_resp)
  mockery::stub(get_tarot_interpretation, "httr2::resp_status",      function(...) 200L)
  mockery::stub(get_tarot_interpretation, "httr2::resp_body_string", function(...) api_body)

  result <- get_tarot_interpretation(
    card_name     = fix$card_names$star,
    card_meanings  = fix$card_meanings$star,
    api_key       = "test-key-abc"
  )

  expect_equal(result$title,         fix$mock_llm_response$title)
  expect_equal(result$body,          fix$mock_llm_response$body)
  expect_equal(result$general,       fix$mock_llm_response$general)
  expect_equal(result$work,          fix$mock_llm_response$work)
  expect_equal(result$health,        fix$mock_llm_response$health)
  expect_equal(result$relationships, fix$mock_llm_response$relationships)
})

test_that("get_tarot_interpretation falls back when LLM returns invalid JSON", {
  bad_body <- jsonlite::toJSON(list(
    choices = list(list(message = list(content = "not json")))
  ), auto_unbox = TRUE)

  mockery::stub(get_tarot_interpretation, "httr2::req_perform",      function(...) structure(list(), class = "httr2_response"))
  mockery::stub(get_tarot_interpretation, "httr2::resp_status",      function(...) 200L)
  mockery::stub(get_tarot_interpretation, "httr2::resp_body_string", function(...) bad_body)

  result <- get_tarot_interpretation(
    card_name     = "Tower",
    card_meanings  = c("Upheaval"),
    api_key       = "test-key-abc"
  )

  # Should fall back to card_meanings
  expect_equal(result$title, "Tower")
  expect_match(result$body, "Upheaval")
})

test_that("get_tarot_interpretation falls back on network error", {
  mockery::stub(get_tarot_interpretation, "httr2::req_perform", function(...) stop("network error"))

  result <- get_tarot_interpretation(
    card_name     = "Sun",
    card_meanings  = c("Joy"),
    api_key       = "test-key-abc"
  )

  expect_equal(result$title, "Sun")
})

test_that("get_tarot_interpretation falls back on non-200 status", {
  mockery::stub(get_tarot_interpretation, "httr2::req_perform", function(...) structure(list(), class = "httr2_response"))
  mockery::stub(get_tarot_interpretation, "httr2::resp_status", function(...) 429L)

  result <- get_tarot_interpretation(
    card_name     = "Wheel",
    card_meanings  = c("Change"),
    api_key       = "test-key-abc"
  )

  expect_equal(result$title, "Wheel")
})


# ── DataManager: draw_status & shuffle_and_prepare ───────────────────────────

test_that("DataManager initialises with draw_status = 'idle'", {
  ns <- asNamespace("astrocalculation")
  mock_names <- c("connect_postgres_db", "Logger", "lookup_city_data",
                  "calculate_planet_position", "calculate_aspect", "draw_natal_chart")
  originals <- setNames(lapply(mock_names, function(nm) get(nm, envir = ns)), mock_names)
  on.exit({
    for (nm in mock_names) {
      if (bindingIsLocked(nm, ns)) unlockBinding(nm, ns)
      assign(nm, originals[[nm]], envir = ns)
    }
  }, add = TRUE)

  unlockBinding("connect_postgres_db", ns)
  assign("connect_postgres_db", function() list(conn = TRUE), envir = ns)
  unlockBinding("Logger", ns)
  assign("Logger",
    list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
    envir = ns)
  unlockBinding("lookup_city_data", ns)
  assign("lookup_city_data",
    function(country, city) data.frame(lat = 0, lng = 0, timezone = "UTC"),
    envir = ns)
  unlockBinding("calculate_planet_position", ns)
  assign("calculate_planet_position",
    function(...) list(planetary_position = data.frame(dummy = 1)),
    envir = ns)
  unlockBinding("calculate_aspect", ns)
  assign("calculate_aspect", function(data) data.frame(), envir = ns)
  unlockBinding("draw_natal_chart", ns)
  assign("draw_natal_chart", function(...) ggplot2::ggplot(), envir = ns)

  with_mocked_bindings(
    {
      r6 <- suppressMessages(DataManager$new())
      on.exit({ r6$pool <- NULL }, add = TRUE)
      expect_equal(r6$draw_status, "idle")
      expect_null(r6$llm_interpretation)
    },
    poolWithTransaction = function(pool, code) code(NULL),
    .env = asNamespace("pool")
  )
})

test_that("shuffle_and_prepare draws a card and returns a future", {
  ns <- asNamespace("astrocalculation")
  mock_names <- c("connect_postgres_db", "Logger", "lookup_city_data",
                  "calculate_planet_position", "calculate_aspect", "draw_natal_chart",
                  "connect_tarot_db", "shuffle_deck", "draw_cards", "get_tarot_interpretation")
  originals <- setNames(lapply(mock_names, function(nm) get(nm, envir = ns)), mock_names)
  on.exit({
    for (nm in mock_names) {
      if (bindingIsLocked(nm, ns)) unlockBinding(nm, ns)
      assign(nm, originals[[nm]], envir = ns)
    }
  }, add = TRUE)

  unlockBinding("connect_postgres_db", ns)
  assign("connect_postgres_db", function() list(conn = TRUE), envir = ns)
  unlockBinding("Logger", ns)
  assign("Logger",
    list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
    envir = ns)
  unlockBinding("lookup_city_data", ns)
  assign("lookup_city_data",
    function(country, city) data.frame(lat = 0, lng = 0, timezone = "UTC"),
    envir = ns)
  unlockBinding("calculate_planet_position", ns)
  assign("calculate_planet_position",
    function(...) list(planetary_position = data.frame(dummy = 1)),
    envir = ns)
  unlockBinding("calculate_aspect", ns)
  assign("calculate_aspect", function(data) data.frame(), envir = ns)
  unlockBinding("draw_natal_chart", ns)
  assign("draw_natal_chart", function(...) ggplot2::ggplot(), envir = ns)
  unlockBinding("connect_tarot_db", ns)
  assign("connect_tarot_db", function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbExecute(con, "CREATE TABLE tarot_cards (id INTEGER, name_zh TEXT, file TEXT)")
    DBI::dbExecute(con, paste0("INSERT INTO tarot_cards VALUES (1, '", fix$db_mock$fool_name, "', 'c01')"))
    DBI::dbExecute(con, "CREATE TABLE tarot_card_meanings (id INTEGER, is_reversed INTEGER, meaning_zh TEXT)")
    DBI::dbExecute(con, paste0("INSERT INTO tarot_card_meanings VALUES (1, 0, '", fix$db_mock$fool_meaning, "')"))
    con
  }, envir = ns)
  unlockBinding("shuffle_deck", ns)
  assign("shuffle_deck", function(...) data.frame(id = 2L, is_reversed = FALSE), envir = ns)
  unlockBinding("draw_cards", ns)
  assign("draw_cards", function(n, deck) deck[1, , drop = FALSE], envir = ns)
  unlockBinding("get_tarot_interpretation", ns)
  assign("get_tarot_interpretation",
    function(...) list(title = "T", body = "B", general = "G", work = "W",
                       health = "H", relationships = "R"),
    envir = ns)

  with_mocked_bindings(
    {
      r6 <- suppressMessages(DataManager$new())
      on.exit({ r6$pool <- NULL }, add = TRUE)

      fut <- r6$shuffle_and_prepare(skip_llm = FALSE)

      # Card fields must be populated synchronously
      expect_false(is.null(r6$current_cards))
      expect_false(is.null(r6$card_files))

      # Return value must be a future (has class "Future")
      expect_true(inherits(fut, "Future"))
    },
    poolWithTransaction = function(pool, code) code(NULL),
    .env = asNamespace("pool")
  )
})

test_that("shuffle_and_prepare skip_llm=TRUE skips get_tarot_interpretation call", {
  ns <- asNamespace("astrocalculation")
  mock_names <- c("connect_postgres_db", "Logger", "lookup_city_data",
                  "calculate_planet_position", "calculate_aspect", "draw_natal_chart",
                  "connect_tarot_db", "shuffle_deck", "draw_cards", "get_tarot_interpretation")
  originals <- setNames(lapply(mock_names, function(nm) get(nm, envir = ns)), mock_names)
  on.exit({
    for (nm in mock_names) {
      if (bindingIsLocked(nm, ns)) unlockBinding(nm, ns)
      assign(nm, originals[[nm]], envir = ns)
    }
  }, add = TRUE)

  unlockBinding("connect_postgres_db", ns)
  assign("connect_postgres_db", function() list(conn = TRUE), envir = ns)
  unlockBinding("Logger", ns)
  assign("Logger",
    list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
    envir = ns)
  unlockBinding("lookup_city_data", ns)
  assign("lookup_city_data",
    function(country, city) data.frame(lat = 0, lng = 0, timezone = "UTC"),
    envir = ns)
  unlockBinding("calculate_planet_position", ns)
  assign("calculate_planet_position",
    function(...) list(planetary_position = data.frame(dummy = 1)),
    envir = ns)
  unlockBinding("calculate_aspect", ns)
  assign("calculate_aspect", function(data) data.frame(), envir = ns)
  unlockBinding("draw_natal_chart", ns)
  assign("draw_natal_chart", function(...) ggplot2::ggplot(), envir = ns)
  unlockBinding("connect_tarot_db", ns)
  assign("connect_tarot_db", function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbExecute(con, "CREATE TABLE tarot_cards (id INTEGER, name_zh TEXT, file TEXT)")
    DBI::dbExecute(con, paste0("INSERT INTO tarot_cards VALUES (1, '", fix$db_mock$fool_name, "', 'c01')"))
    DBI::dbExecute(con, "CREATE TABLE tarot_card_meanings (id INTEGER, is_reversed INTEGER, meaning_zh TEXT)")
    DBI::dbExecute(con, paste0("INSERT INTO tarot_card_meanings VALUES (1, 0, '", fix$db_mock$fool_meaning_skip_llm, "')"))
    con
  }, envir = ns)
  unlockBinding("shuffle_deck", ns)
  assign("shuffle_deck", function(...) data.frame(id = 2L, is_reversed = FALSE), envir = ns)
  unlockBinding("draw_cards", ns)
  assign("draw_cards", function(n, deck) deck[1, , drop = FALSE], envir = ns)

  # get_tarot_interpretation should NOT be called when skip_llm = TRUE
  llm_call_count <- 0L
  unlockBinding("get_tarot_interpretation", ns)
  assign("get_tarot_interpretation", function(...) {
    llm_call_count <<- llm_call_count + 1L
    list(title = "T", body = "B", general = "G", work = "W",
         health = "H", relationships = "R")
  }, envir = ns)

  with_mocked_bindings(
    {
      r6 <- suppressMessages(DataManager$new())
      on.exit({ r6$pool <- NULL }, add = TRUE)

      fut <- r6$shuffle_and_prepare(skip_llm = TRUE)

      expect_true(inherits(fut, "Future"))
      # LLM must NOT have been called for skip_llm = TRUE
      expect_equal(llm_call_count, 0L)
    },
    poolWithTransaction = function(pool, code) code(NULL),
    .env = asNamespace("pool")
  )
})
