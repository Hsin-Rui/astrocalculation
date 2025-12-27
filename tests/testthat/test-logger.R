test_that("Logger writes correctly to app_logs", {

  # 1. Setup
  # --- Connection Logic with Skip ---
  # Attempt to connect and capture potential errors
  pool <- tryCatch({
    connect_postgres_db()
  }, error = function(e) {
    return(NULL) # Return NULL if connection fails
  })

  # Skip the entire test if the pool is NULL or the connection is invalid
  skip_if(is.null(pool), "Postgres connection could not be established; skipping test.")

  on.exit(pool::poolClose(pool))

  # Initialize Logger
  logger <- Logger$new(pool)

  # --- TEST 1: Standard INFO Log ---
  test_evt_info <- "TEST_UNIT_INFO"
  test_msg_info <- "Verifying logger info write"
  test_user <- "test_user_id"

  # Action
  logger$log_info(test_evt_info, test_msg_info, user_id = test_user)

  # Verify
  # We query the DB to prove it actually wrote the row
  res <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
                                                   "SELECT * FROM app_logs WHERE event = ?evt ORDER BY timestamp DESC LIMIT 1",
                                                   evt = test_evt_info
  ))

  expect_equal(nrow(res), 1, label = "Row should exist in DB")
  expect_equal(res$level, "INFO")
  expect_equal(res$message, test_msg_info)
  expect_equal(res$user_entity_id, test_user)

  # --- TEST 2: ERROR Log with JSON Context ---
  test_evt_err <- "TEST_UNIT_ERROR"
  test_ctx <- list(
    error_code = 404,
    details = "Resource not found",
    attempt = 1
  )

  # Action
  logger$log_error(test_evt_err, "Something crashed", context = test_ctx)

  # Verify
  res_err <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
                                                       "SELECT * FROM app_logs WHERE event = ?evt ORDER BY timestamp DESC LIMIT 1",
                                                       evt = test_evt_err
  ))

  expect_equal(nrow(res_err), 1)
  expect_equal(res_err$level, "ERROR")

  # Verify JSON Serialization
  # We read the JSON string back from DB and parse it
  json_out <- jsonlite::fromJSON(res_err$context_json)

  expect_equal(json_out$error_code, 404)
  expect_equal(json_out$details, "Resource not found")

  # --- CLEANUP ---
  # Remove test rows to keep DB clean
  DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
                                           "DELETE FROM app_logs WHERE event IN (?e1, ?e2)",
                                           e1 = test_evt_info, e2 = test_evt_err
  ))
})
