# tests/testthat/test-story-1-auth.R

test_that("Story 1: Database Schema & Authentication Logic", {

  # --- Connection Logic with Skip ---
  pool <- tryCatch({
    connect_postgres_db()
  }, error = function(e) {
    return(NULL)
  })

  skip_if(is.null(pool), "Postgres connection could not be established; skipping test.")

  # Test Data
  test_id <- "test_user_007"
  test_email <- "bond@mi6.gov.uk"
  test_pass <- "SecretAgentMan!123"
  test_name <- "James Bond"

  # 2. Helper: cleanup_logic
  # We define this function to use it in both Clean Start and Teardown
  cleanup_logic <- function() {
    try({
      # A. Delete dependent data first (Manual Cascade)
      # This prevents "Foreign Key Constraint" errors if CASCADE is missing in DB
      DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
                                               "DELETE FROM user_profiles WHERE user_entity_id = ?id", id = test_id))

      DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
                                               "DELETE FROM personal_library WHERE user_entity_id = ?id", id = test_id))

      DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
                                               "DELETE FROM app_logs WHERE user_entity_id = ?id", id = test_id))

      # B. Delete the User Account
      DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
                                               "DELETE FROM auth_credentials WHERE user_entity_id = ?id OR email = ?email",
                                               id = test_id, email = test_email))
    }, silent = TRUE)
  }

  # 3. ROBUST TEARDOWN (Run at end of test)
  on.exit({
    cleanup_logic()
    pool::poolClose(pool)
  }, add = TRUE)

  # 4. CLEAN START (Run immediately)
  # Run cleanup explicitly to clear any data from previous failed runs
  cleanup_logic()

  # --- AC 1 & 2: Table Definitions Exist ---
  tables <- DBI::dbListTables(pool)
  expect_true("auth_credentials" %in% tables, "auth_credentials table must exist")
  expect_true("app_sessions" %in% tables, "app_sessions table must exist")

  fields_auth <- DBI::dbListFields(pool, "auth_credentials")
  expect_true(all(c("user_entity_id", "password_hash", "salt") %in% fields_auth),
              "auth_credentials must have required columns")

  # --- AC 3: Registration creates rows in BOTH tables ---
  new_uid <- auth_register_user(pool, test_id, test_email, test_pass, test_name)

  expect_equal(new_uid, test_id)

  # Check auth_credentials
  res_auth <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
                                                        "SELECT * FROM auth_credentials WHERE user_entity_id = ?id", id = test_id))
  expect_equal(nrow(res_auth), 1)

  # Check user_profiles
  res_prof <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
                                                        "SELECT * FROM user_profiles WHERE user_entity_id = ?id", id = test_id))
  expect_equal(nrow(res_prof), 1)
  expect_equal(res_prof$display_name, test_name)

  # --- AC 4: Password is NOT plain text ---
  stored_hash <- res_auth$password_hash
  expect_false(stored_hash == test_pass, "Password MUST be hashed")
  expect_true(nchar(stored_hash) > 20, "Hash should be long (bcrypt/sodium string)")

  # --- AC 5: Registration fails if email exists ---
  expect_error(
    auth_register_user(pool, "another_id", test_email, "pass123", "Fake Bond"),
    info = "Should not allow duplicate emails"
  )

  # --- AC 5b: Registration fails if User ID exists ---
  expect_error(
    auth_register_user(pool, test_id, "different@email.com", "pass123", "Fake Bond"),
    info = "Should not allow duplicate User IDs"
  )

  # --- AC 6: Flexible Login (Email OR ID) ---

  # 1. Login with Email
  login_1 <- auth_verify_user(pool, test_email, test_pass)
  expect_equal(login_1, test_id, info = "Should login via Email")

  # 2. Login with User ID
  login_2 <- auth_verify_user(pool, test_id, test_pass)
  expect_equal(login_2, test_id, info = "Should login via User ID")

  # 3. Fail with wrong password
  login_3 <- auth_verify_user(pool, test_id, "WrongPass")
  expect_null(login_3, info = "Should fail with wrong password")

})
