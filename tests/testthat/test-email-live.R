test_that("Manual: Live Email Sending (Requires .Renviron Credentials)", {

  # 1. Skip this test automatically if credentials are missing
  # This prevents CI/CD failures on GitHub Actions where secrets might not be set
  skip_if(Sys.getenv("SMTP_USERNAME") == "" || Sys.getenv("SMTP_PASSWORD") == "",
          "Skipping live email test: SMTP credentials not found in environment.")

  # 2. Force the Email Service to actually send
  Sys.setenv(LIVE_EMAIL_TEST = "true")

  # Ensure we unset this flag even if the test crashes
  on.exit(Sys.unsetenv("LIVE_EMAIL_TEST"))

  # 3. Attempt to send an email to the sender (Self-Test)
  # We use a dummy token "LIVE_TEST_TOKEN"
  target_email <- Sys.getenv("SMTP_USERNAME")

  message(sprintf("📧 Attempting to send live test email to: %s ...", target_email))

  success <- send_verification_email(
    to_email = target_email,
    token = "LIVE_TEST_TOKEN"
  )

  # 4. Assert Success
  expect_true(success, "Email should be sent successfully without SMTP errors.")
})
