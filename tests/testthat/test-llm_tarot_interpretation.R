library(testthat)

# ── validate_interpretation ───────────────────────────────────────────────────

test_that("validate_interpretation returns valid=TRUE for well-formed JSON", {
  json <- '{"title":"The Fool","body":"A fresh start awaits.","wisdom_tag":"Begin now"}'
  result <- validate_interpretation(json)
  expect_true(result$valid)
  expect_equal(result$data$title, "The Fool")
  expect_equal(result$data$body,  "A fresh start awaits.")
  expect_equal(result$data$wisdom_tag, "Begin now")
})

test_that("validate_interpretation strips markdown code fences", {
  json <- "```json\n{\"title\":\"T\",\"body\":\"B\",\"wisdom_tag\":\"W\"}\n```"
  result <- validate_interpretation(json)
  expect_true(result$valid)
  expect_equal(result$data$title, "T")
})

test_that("validate_interpretation returns valid=FALSE for non-JSON input", {
  result <- validate_interpretation("not json at all")
  expect_false(result$valid)
  expect_match(result$error, "schema", ignore.case = TRUE)
})

test_that("validate_interpretation returns valid=FALSE when a field is missing", {
  json <- '{"title":"T","body":"B"}'   # wisdom_tag missing
  result <- validate_interpretation(json)
  expect_false(result$valid)
  expect_match(result$error, "schema", ignore.case = TRUE)
})

test_that("validate_interpretation returns valid=FALSE for empty string", {
  result <- validate_interpretation("")
  expect_false(result$valid)
})

test_that("validate_interpretation returns valid=FALSE for extra fields", {
  json <- '{"title":"T","body":"B","wisdom_tag":"W","extra":"nope"}'
  result <- validate_interpretation(json)
  expect_false(result$valid)
  expect_match(result$error, "schema", ignore.case = TRUE)
})

test_that("validate_interpretation returns valid=FALSE for empty required values", {
  json <- '{"title":"","body":"B","wisdom_tag":"W"}'
  result <- validate_interpretation(json)
  expect_false(result$valid)
})

test_that("validate_interpretation rejects numeric field values", {
  json <- '{"title":42,"body":"B","wisdom_tag":"W"}'
  result <- validate_interpretation(json)
  expect_false(result$valid)
  expect_match(result$error, "schema", ignore.case = TRUE)
})


# ── get_tarot_interpretation: fallback paths ──────────────────────────────────

test_that("get_tarot_interpretation returns fallback when api_key is NULL", {
  result <- get_tarot_interpretation(
    card_name    = "愚者",
    card_meanings = c("自由", "冒險"),
    api_key      = NULL
  )
  expect_type(result, "list")
  expect_named(result, c("title", "body", "wisdom_tag"))
  expect_equal(result$title, "愚者")
  expect_match(result$body, "自由")
})

test_that("get_tarot_interpretation returns fallback when api_key is empty string", {
  result <- get_tarot_interpretation(
    card_name    = "星星",
    card_meanings = c("希望"),
    api_key      = ""
  )
  expect_equal(result$title, "星星")
})

test_that("get_tarot_interpretation returns fallback when api_key is whitespace", {
  result <- get_tarot_interpretation(
    card_name    = "月亮",
    card_meanings = c("直覺"),
    api_key      = "   "
  )
  expect_equal(result$title, "月亮")
})

test_that("get_tarot_interpretation empty meanings fallback is safe", {
  result <- get_tarot_interpretation(
    card_name = "",
    card_meanings = character(0),
    api_key = NULL
  )

  expect_equal(result$title, "Daily Tarot")
  expect_match(result$body, "No interpretation available")
  expect_match(result$wisdom_tag, "grounded", ignore.case = TRUE)
})


# ── get_tarot_interpretation: mocked HTTP success ────────────────────────────

test_that("get_tarot_interpretation parses valid LLM response", {
  # Build a minimal OpenAI-compatible response body
  llm_content <- '{"title":"The Star","body":"Hope endures.","wisdom_tag":"Trust the process"}'
  api_body <- jsonlite::toJSON(list(
    choices = list(list(message = list(content = llm_content)))
  ), auto_unbox = TRUE)

  # Mock req_perform to return a fake 200 response
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
    card_name     = "The Star",
    card_meanings  = c("Hope"),
    api_key       = "test-key-abc"
  )

  expect_true(result$valid %||% TRUE)  # may be plain list, not wrapped
  expect_equal(result$title, "The Star")
  expect_equal(result$body,  "Hope endures.")
  expect_equal(result$wisdom_tag, "Trust the process")
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
  with_mocked_bindings(
    {
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
    },
    connect_postgres_db     = function() list(conn = TRUE),
    Logger                  = list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
    lookup_city_data        = function(country, city) data.frame(lat = 0, lng = 0, timezone = "UTC"),
    calculate_planet_position = function(...) list(planetary_position = data.frame(dummy = 1)),
    calculate_aspect        = function(data) data.frame(),
    draw_whole_sign_chart   = function(...) list(),
    .env = asNamespace("astrocalculation")
  )
})

test_that("shuffle_and_prepare draws a card and returns a future", {
  with_mocked_bindings(
    {
      with_mocked_bindings(
        {
          r6 <- suppressMessages(DataManager$new())
          on.exit({ r6$pool <- NULL }, add = TRUE)

          fut <- r6$shuffle_and_prepare()

          # Card fields must be populated synchronously
          expect_false(is.null(r6$current_cards))
          expect_false(is.null(r6$card_files))

          # Return value must be a future (has class "Future")
          expect_true(inherits(fut, "Future"))
        },
        poolWithTransaction = function(pool, code) code(NULL),
        .env = asNamespace("pool")
      )
    },
    connect_postgres_db   = function() list(conn = TRUE),
    Logger                = list(new = function(pool) list(log_info = function(...) NULL, log_error = function(...) NULL)),
    lookup_city_data      = function(country, city) data.frame(lat = 0, lng = 0, timezone = "UTC"),
    calculate_planet_position = function(...) list(planetary_position = data.frame(dummy = 1)),
    calculate_aspect      = function(data) data.frame(),
    draw_whole_sign_chart = function(...) list(),
    # Use "c01" so system.file finds the real inst/tarot_cards/c01.jpg
    connect_tarot_db      = function() {
      con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      DBI::dbExecute(con, "CREATE TABLE tarot_cards (id INTEGER, name_zh TEXT, file TEXT)")
      DBI::dbExecute(con, "INSERT INTO tarot_cards VALUES (1, '愚者', 'c01')")
      DBI::dbExecute(con, "CREATE TABLE tarot_card_meanings (id INTEGER, is_reversed INTEGER, meaning_zh TEXT)")
      DBI::dbExecute(con, "INSERT INTO tarot_card_meanings VALUES (1, 0, '自由冒險')")
      con
    },
    shuffle_deck  = function(...) data.frame(id = 2L, is_reversed = FALSE),
    draw_cards    = function(n, deck) deck[1, , drop = FALSE],
    get_tarot_interpretation = function(...) list(title = "T", body = "B", wisdom_tag = "W"),
    .env = asNamespace("astrocalculation")
  )
})
