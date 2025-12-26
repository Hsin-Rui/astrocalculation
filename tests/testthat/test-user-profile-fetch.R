test_that("Story 2: Profile Fetching (Join Logic)", {
  # --- Connection Logic with Skip ---
  # Attempt to connect and capture potential errors
  pool <- tryCatch({
    connect_postgres_db()
  }, error = function(e) {
    return(NULL) # Return NULL if connection fails
  })

  # Skip the entire test if the pool is NULL or the connection is invalid
  skip_if(is.null(pool), "Postgres connection could not be established; skipping test.")


  # 1. Setup
  test_email <- "profile_test@example.com"

  # Cleanup (Fix: Use sqlInterpolate)
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
      }
    }, silent = TRUE)

    # 2. Finally, close the pool
    pool::poolClose(pool)
  }, add = TRUE)

  user_oid <- auth_register_user(pool, "prof_test", test_email, "password", "ProfileTester")

  # 2. Get Profile (Verify Email JOIN)
  prof <- db_get_profile(pool, user_oid)
  expect_false(is.null(prof))
  expect_equal(prof$email, test_email)

  # 3. Update Profile (Verify Persistence)
  new_data <- list(
    display_name = "New Name",
    birth_timestamp = Sys.time(),
    city_name = "Paris",
    country = "France"
  )

  db_save_profile(pool, user_oid, new_data)

  # 4. Fetch Again
  prof_updated <- db_get_profile(pool, user_oid)
  expect_equal(prof_updated$display_name, "New Name")
  expect_equal(prof_updated$city_name, "Paris")
  expect_equal(prof_updated$email, test_email)
})
