test_that("Story 2: Profile Fetching (Join Logic)", {
  # --- Connection Logic with Skip ---
  pool <- tryCatch(
    {
      connect_postgres_db()
    },
    error = function(e) {
      return(NULL)
    }
  )

  skip_if(is.null(pool), "Postgres connection could not be established; skipping test.")
  skip_if(!inherits(pool, "Pool"), "Postgres pool not available; skipping test.")

  # 1. Setup
  test_email <- "profile_test@example.com"

  # Use a Strong Password
  test_pass <- "SecureProfPass1!"

  # Cleanup
  cleanup_logic <- function() {
    try(
      {
        if (DBI::dbExistsTable(pool, "auth_credentials")) {
          DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
            "DELETE FROM auth_credentials WHERE email = ?email",
            email = test_email
          ))
        }
      },
      silent = TRUE
    )
  }

  on.exit(
    {
      cleanup_logic()
      if (inherits(pool, "Pool")) pool::poolClose(pool)
    },
    add = TRUE
  )

  cleanup_logic() # Clean Start

  # --- FIX: Use Strong Password & Extract ID ---
  reg_res <- auth_register_user(pool, "prof_test", test_email, test_pass, "ProfileTester")
  user_oid <- reg_res$user_id

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
