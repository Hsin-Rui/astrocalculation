library(testthat)

# ---------------------------------------------------------------------------
# Unit tests for db_user_data.R — Story 1.2: Tarot Journal persistence
# ---------------------------------------------------------------------------

# Minimal mock connection (S4) compatible with the pattern in test-auth-logic-unit.R
make_ud_mock_conn <- function(exec_side = NULL) {
  if (!methods::isClass("MockConnUD")) {
    methods::setClass("MockConnUD", contains = "DBIConnection")
  }

  methods::setMethod(
    "sqlInterpolate",
    signature(conn = "MockConnUD", sql = "character"),
    function(conn, sql, ...) sql
  )

  methods::setMethod(
    "dbExecute",
    signature(conn = "MockConnUD", statement = "character"),
    function(conn, statement, ...) {
      if (!is.null(exec_side)) exec_side(statement)
      1L
    }
  )

  new("MockConnUD")
}

# ---------------------------------------------------------------------------

test_that("save_tarot_draw stops when pool is NULL", {
  expect_error(
    save_tarot_draw(NULL, "uid-1", "The Fool"),
    "Database connection is required"
  )
})

test_that("save_tarot_draw stops when user_id is missing", {
  pool <- make_ud_mock_conn()
  expect_error(
    save_tarot_draw(pool, NULL, "The Fool"),
    "user_id is required"
  )
  expect_error(
    save_tarot_draw(pool, "", "The Fool"),
    "user_id is required"
  )
})

test_that("save_tarot_draw stops when card_id is missing", {
  pool <- make_ud_mock_conn()
  expect_error(
    save_tarot_draw(pool, "uid-1", NULL),
    "card_id is required"
  )
  expect_error(
    save_tarot_draw(pool, "uid-1", ""),
    "card_id is required"
  )
})

test_that("save_tarot_draw executes a parameterized INSERT and returns 1", {
  calls <- list(count = 0L, statements = character())

  pool <- make_ud_mock_conn(exec_side = function(sql) {
    calls$count <<- calls$count + 1L
    calls$statements <<- c(calls$statements, sql)
  })

  rows <- save_tarot_draw(
    pool,
    user_id             = "uid-1",
    card_id             = "The Fool",
    interpretation_text = "A journey begins.",
    is_free_tier        = TRUE
  )

  expect_true(any(grepl("CREATE TABLE IF NOT EXISTS tarot_draws", calls$statements, fixed = TRUE)))
  expect_true(any(grepl("INSERT INTO tarot_draws", calls$statements, fixed = TRUE)))
  expect_false(any(grepl("gen_random_uuid", calls$statements, fixed = TRUE)))
  expect_false(any(grepl("uuid_generate_v4", calls$statements, fixed = TRUE)))
  expect_equal(rows, 1L)
})

test_that("save_tarot_draw handles NULL interpretation_text without error", {
  pool <- make_ud_mock_conn()
  expect_no_error(
    save_tarot_draw(pool, "uid-1", "The Moon", interpretation_text = NULL)
  )
})

# ---------------------------------------------------------------------------
# record_llm_credit_used — degrades gracefully when column is absent
# ---------------------------------------------------------------------------

test_that("record_llm_credit_used returns FALSE when pool is NULL", {
  res <- record_llm_credit_used(NULL, "uid-1")
  expect_false(res)
})

test_that("record_llm_credit_used returns TRUE and calls dbExecute on success", {
  calls <- list(count = 0L)

  pool <- make_ud_mock_conn(exec_side = function(sql) {
    calls$count <<- calls$count + 1L
  })

  res <- record_llm_credit_used(pool, "uid-1")
  expect_true(res)
  expect_equal(calls$count, 1L)
})

test_that("record_llm_credit_used warns (not errors) on DB failure", {
  methods::setMethod(
    "dbExecute",
    signature(conn = "MockConnUD", statement = "character"),
    function(conn, statement, ...) stop("column does not exist")
  )

  pool <- new("MockConnUD")
  expect_warning(
    record_llm_credit_used(pool, "uid-1"),
    "record_llm_credit_used"
  )
})
