# tests/testthat/test-story-1-auth.R
test_that("Epic 3.1: Secure Registration & Email Verification", {
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

  # 1. Define Test Data (Constants)
  test_id <- "test_user_007"
  test_email <- "bond@mi6.gov.uk"
  # Note: Must now be strong (>8 chars, number, special char)
  test_pass_strong <- "SecretAgentMan!123"
  test_pass_weak <- "weak"
  test_name <- "James Bond"

  # 2. Helper: cleanup_logic (Robust Cascade)
  cleanup_logic <- function() {
    try(
      {
        # Clean dependent tables first
        DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
          "DELETE FROM user_profiles WHERE user_entity_id = ?id",
          id = test_id
        ))

        DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
          "DELETE FROM personal_library WHERE user_entity_id = ?id",
          id = test_id
        ))

        DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
          "DELETE FROM app_logs WHERE user_entity_id = ?id",
          id = test_id
        ))

        # Clean master table
        DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
          "DELETE FROM auth_credentials WHERE user_entity_id = ?id OR email = ?email",
          id = test_id, email = test_email
        ))
      },
      silent = TRUE
    )
  }

  # 3. Teardown & Setup
  on.exit(
    {
      cleanup_logic()
      if (inherits(pool, "Pool")) pool::poolClose(pool)
    },
    add = TRUE
  )

  cleanup_logic() # Clean start

  # --- TEST A: Validation Failures ---

  # A1. Invalid Email
  expect_error(
    auth_register_user(pool, test_id, "not-an-email", test_pass_strong, test_name,
                       terms_accepted = TRUE),
    "Invalid email format",
    info = "Should reject bad email regex"
  )

  # A2. Weak Password (Too short / missing chars)
  expect_error(
    auth_register_user(pool, test_id, test_email, "weakpass", test_name,
                       terms_accepted = TRUE),
    "Password must be at least 8 characters",
    info = "Should reject weak passwords"
  )

  # --- TEST B: Successful Registration ---

  # Register with valid credentials
  # Result is now a LIST: list(user_id = ..., verification_token = ...)
  res <- auth_register_user(pool, test_id, test_email, test_pass_strong, test_name,
                            terms_accepted = TRUE)

  expect_true(is.list(res))
  expect_equal(res$user_id, test_id)
  expect_true(nchar(res$verification_token) > 10) # Ensure token generated

  # --- TEST C: Verification Status (Pending) ---

  # Check DB status immediately after registration
  user_row <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
    "SELECT is_verified, verification_token FROM auth_credentials WHERE user_entity_id = ?id",
    id = test_id
  ))

  expect_false(user_row$is_verified) # Should be FALSE by default
  expect_equal(user_row$verification_token, res$verification_token)

  # --- TEST D: Email Verification Flow ---

  # 1. Try with wrong token
  success_fail <- auth_verify_email(pool, "wrong-token-123")
  expect_false(success_fail)

  # 2. Try with correct token
  success_ok <- auth_verify_email(pool, res$verification_token)
  expect_true(success_ok)

  # 3. Verify DB Update
  user_row_ver <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
    "SELECT is_verified, verification_token FROM auth_credentials WHERE user_entity_id = ?id",
    id = test_id
  ))

  expect_true(user_row_ver$is_verified) # Should now be TRUE
  expect_true(is.na(user_row_ver$verification_token)) # Token should be cleared/NULL

  # --- TEST E: Login Behavior ---

  # Login should work with the strong password
  # (Note: In future Story 3.3, login might check is_verified, but for now we check creds)
  login_id <- auth_verify_user(pool, test_email, test_pass_strong)
  expect_equal(login_id$id, test_id)
})
