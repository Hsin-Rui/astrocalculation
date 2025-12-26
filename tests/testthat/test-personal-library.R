test_that("Story 3: Personal Library (Save, Load, Delete)", {

  # 1. Setup: Create a User
  # --- Connection Logic with Skip ---
  # Attempt to connect and capture potential errors
  pool <- tryCatch({
    connect_postgres_db()
  }, error = function(e) {
    return(NULL) # Return NULL if connection fails
  })

  # Skip the entire test if the pool is NULL or the connection is invalid
  skip_if(is.null(pool), "Postgres connection could not be established; skipping test.")


  test_email <- "lib@test.com"

  # Cleanup (Fix: Use sqlInterpolate + Delete by Email)
  # Cleanup before test, in case of previous failed run
  if(DBI::dbExistsTable(pool, "auth_credentials")) {
    sql <- DBI::sqlInterpolate(pool, "DELETE FROM auth_credentials WHERE email = ?email", email = test_email)
    DBI::dbExecute(pool, sql)
  }

  # Ensure cleanup happens after test execution, even on failure
  on.exit({
    try({
    if(DBI::dbExistsTable(pool, "auth_credentials")) {
      sql <- DBI::sqlInterpolate(pool, "DELETE FROM auth_credentials WHERE email = ?email", email = test_email)
      DBI::dbExecute(pool, sql)
    }}, silent = TRUE)
    pool::poolClose(pool)
  }, add = TRUE)

  user_oid <- auth_register_user(pool, "test_lib_user", test_email, "password", "LibUser")

  # 2. Save a Chart
  chart_data <- list(
    name = "Test Chart A",
    birth_timestamp = as.POSIXct("1990-01-01 12:00:00", tz="UTC"),
    timezone = "UTC",
    city_name = "London",
    country = "UK",
    lat = 51.5,
    lng = -0.12,
    notes = "My first test chart"
  )

  # Save new
  expect_silent(db_save_library_entry(pool, user_oid, chart_data))

  # 3. Retrieve Library
  lib <- db_get_library(pool, user_oid)
  expect_equal(nrow(lib), 1)
  expect_equal(lib$name[1], "Test Chart A")
  expect_equal(lib$notes[1], "My first test chart")

  # 4. Update existing Chart (SCD Type 2 check)
  entity_id <- lib$entity_id[1]
  chart_data$notes <- "Updated notes"

  db_save_library_entry(pool, user_oid, chart_data, entity_id)

  lib_updated <- db_get_library(pool, user_oid)
  expect_equal(nrow(lib_updated), 1)
  expect_equal(lib_updated$notes[1], "Updated notes")

  # 5. Delete Chart
  db_delete_library_entry(pool, entity_id)

  lib_final <- db_get_library(pool, user_oid)
  expect_equal(nrow(lib_final), 0)
})
