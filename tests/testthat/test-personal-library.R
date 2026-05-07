test_that("required save fields reject blank values", {
  expect_error(require_non_empty("", "Chart name"), "Chart name is required")
  expect_error(require_non_empty("   ", "City"), "City is required")
  expect_equal(require_non_empty(" Taipei ", "City"), "Taipei")
})

test_that("normalize_local_datetime preserves selected city wall time", {
  entered <- as.POSIXct("1986-02-13 20:30:00", tz = "UTC")
  stored <- normalize_local_datetime(entered, "Asia/Taipei")

  expect_equal(format(stored, "%Y-%m-%d %H:%M:%S", tz = "Asia/Taipei"), "1986-02-13 20:30:00")
  expect_equal(format(stored, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "1986-02-13 12:30:00")
})

test_that("Story 3: Personal Library (Save, Load, Delete)", {

  # 1. Setup: Create a User
  pool <- tryCatch({
    connect_postgres_db()
  }, error = function(e) {
    return(NULL)
  })

  skip_if(is.null(pool), "Postgres connection could not be established; skipping test.")

  # Story 1.2: Apply migration idempotently so the test works against databases
  # that may not have been upgraded yet.
  tryCatch(
    {
      migration_script <- system.file("migrations/001_tarot_draws_and_consent.R",
                                      package = "astrocalculation")
      if (nzchar(migration_script)) {
        local({
          source(migration_script, local = TRUE)
          run_migration_001(pool)
        })
      }
    },
    error = function(e) warning("Migration 001 could not be applied: ", e$message)
  )

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
  reg_res <- auth_register_user(pool, "test_lib_user", test_email, test_pass, "LibUser",
                                terms_accepted = TRUE)
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

  # Save new - UUIDs are generated in R, so this does not depend on DB extensions.
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
