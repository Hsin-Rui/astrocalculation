test_that("Story 3: Personal Library (Save, Load, Delete)", {

  # 1. Setup: Create a User
  pool <- tryCatch({
    connect_postgres_db()
  }, error = function(e) {
    return(NULL)
  })

  skip_if(is.null(pool), "Postgres connection could not be established; skipping test.")

  test_email <- "lib@test.com"

  # Use a Strong Password
  test_pass <- "SecureLibPass1!"

  # Cleanup Logic
  cleanup_logic <- function() {
    try({
      if(DBI::dbExistsTable(pool, "auth_credentials")) {
        # Delete by email to be safe
        DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
                                                 "DELETE FROM auth_credentials WHERE email = ?email", email = test_email))
      }
    }, silent = TRUE)
  }

  on.exit({
    cleanup_logic()
    pool::poolClose(pool)
  }, add = TRUE)

  cleanup_logic() # Clean Start

  # --- FIX: Use Strong Password & Extract ID ---
  # auth_register_user now returns a list(user_id, verification_token)
  reg_res <- auth_register_user(pool, "test_lib_user", test_email, test_pass, "LibUser")
  user_oid <- reg_res$user_id

  # 2. Save a Chart
  chart_data <- list(
    name = "Test Chart A",
    birth_timestamp = as.POSIXct("1990-01-01 12:00:00", tz="UTC"),
    timezone = "UTC",
    city_name = "London",
    country = "United Kingdom",
    lat = 51.5,
    lng = -0.12,
    notes = "My first test chart"
  )

  # Save new - check if uuid_generate_v4() is available
  save_result <- tryCatch({
    expect_silent(db_save_library_entry(pool, user_oid, chart_data))
    TRUE
  }, error = function(e) {
    # Skip if uuid-ossp extension is not installed
    if (grepl("uuid_generate_v4", e$message)) {
      skip("PostgreSQL uuid-ossp extension not installed; skipping library entry tests.")
    }
    stop(e)
  })

  if (isFALSE(save_result)) return()

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
