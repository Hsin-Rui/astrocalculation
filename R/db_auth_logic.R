#' @title Authentication Service Functions
#' @description Handles Registration, Login verification, and Session management.
#'
#' @param pool The DB connection pool
#' @param user_id Custom User ID (specified by user)
#' @param email User email
#' @param password Cleartext password
#' @param display_name User's public name
#'
#' @importFrom sodium password_store password_verify
#' @importFrom DBI dbGetQuery dbExecute sqlInterpolate
#' @importFrom uuid UUIDgenerate
#' @return list(user_id, verification_token) if successful
#'

auth_register_user <- function(pool, user_id, email, password, display_name) {
  # 1. Validation
  if (is.null(user_id) || user_id == "") stop("User ID is required")

  if (!validate_email(email)) {
    stop("Invalid email format.")
  }

  if (!validate_password(password)) {
    stop("Password must be at least 8 characters long, contain a number and a special character.")
  }

  # 2. Check Uniqueness (ID and Email)
  id_check <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
                                                        "SELECT 1 FROM auth_credentials WHERE user_entity_id = ?id", id = user_id))
  if (nrow(id_check) > 0) stop("This User ID is already taken.")

  email_check <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
                                                           "SELECT 1 FROM auth_credentials WHERE email = ?email", email = email))
  if (nrow(email_check) > 0) stop("This Email is already registered.")

  # C. Preparation
  hashed_pw <- sodium::password_store(password)
  # Generate a random verification token
  verif_token <- uuid::UUIDgenerate()

  # 4. Transaction: Insert Account -> Then Profile
  pool::poolWithTransaction(pool, function(con) {

    # Insert Credential (is_verified = FALSE)
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      INSERT INTO auth_credentials (
        user_entity_id, email, password_hash, salt,
        is_verified, verification_token, verification_token_expires_at, created_at
      ) VALUES (
        ?id, ?email, ?hash, '-',
        FALSE, ?token, NOW() + INTERVAL '24 hours', NOW()
      )
    ", id = user_id, email = email, hash = hashed_pw, token = verif_token))

    # Insert Profile
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      INSERT INTO user_profiles (
        user_entity_id, display_name, valid_from
      ) VALUES (
        ?id, ?name, NOW()
      )
    ", id = user_id, name = display_name))

  })

  app_url <- Sys.getenv("APP_BASE_URL", "http://127.0.0.1:3000")

  tryCatch({
    send_verification_email(email, verif_token, app_url)
  }, error = function(e) warning("Email send failed: ", e$message))

  return(list(user_id = user_id, verification_token = verif_token))
}

#' Verify Email Address
#' @param pool db connection object
#' @param token The verification string from the email link
#' @return TRUE if successful, FALSE otherwise
#'
auth_verify_email <- function(pool, token) {
  if (is.null(token) || token == "") return(FALSE)

  # Find user with this token
  res <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool, "
    SELECT user_entity_id FROM auth_credentials WHERE verification_token = ?token
    AND (verification_token_expires_at IS NULL OR verification_token_expires_at > NOW())
  ", token = token))

  if (nrow(res) == 0) return(FALSE)

  user_id <- res$user_entity_id[1]

  # Activate Account
  DBI::dbExecute(pool, DBI::sqlInterpolate(pool, "
    UPDATE auth_credentials
    SET is_verified = TRUE, verification_token = NULL
    WHERE user_entity_id = ?id
  ", id = user_id))

  return(TRUE)
}

#'
auth_verify_user <- function(pool, login_id, password) {
  # 1. Find User & Hash by Email OR User ID
  # We use the same input for both checks (OR condition)
  res <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
    "SELECT user_entity_id, password_hash, is_verified FROM auth_credentials
     WHERE user_entity_id = ?input OR email = ?input",
     input = login_id
  ))

  if (nrow(res) == 0) return(NULL)

  # 2. Verify Hash
  is_valid <- sodium::password_verify(res$password_hash, password)

  if (is_valid) {

    # Update Last Login
    DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
        "UPDATE auth_credentials SET last_login = NOW() WHERE user_entity_id = ?id",
         id = res$user_entity_id
    ))

    return(list(id = res$user_entity_id, verified = res$is_verified))
  } else {
    return(NULL)
  }
}

#' Send Verification Email via SMTP
#'
#' @param to_email The recipient's email address
#' @param token The unique verification token
#' @param base_url The base URL of your Shiny app (for the link)
#' @importFrom emayili envelope from to subject server render
#'
send_verification_email <- function(to_email, token, base_url = "http://127.0.0.1:3000") {

  # 1. BYPASS FOR TESTS (Unless specifically forced)
  # We skip ONLY if we are in a test AND the "LIVE_EMAIL_TEST" flag is NOT set.
  if (Sys.getenv("TESTTHAT") == "true" && Sys.getenv("LIVE_EMAIL_TEST") != "true") {
    message(sprintf(" [TEST MODE] Email suppressed. Link: %s/?verify=%s", base_url, token))
    return(TRUE)
  }

  # 1. Check for SMTP Config
  smtp_user <- Sys.getenv("SMTP_USERNAME")
  smtp_pass <- Sys.getenv("SMTP_PASSWORD")
  smtp_host <- Sys.getenv("SMTP_HOST", "smtp.gmail.com")
  smtp_port <- as.numeric(Sys.getenv("SMTP_PORT", "465"))

  if (smtp_user == "" || smtp_pass == "") {
    warning("SMTP credentials missing. Email not sent. Printing link to console.")
    print(paste("VERIFICATION LINK:", paste0(base_url, "/?verify=", token)))
    return(FALSE)
  }

  # 2. Construct Verification Link
  verify_link <- paste0(base_url, "/?verify=", token)

  # 3. Create Email Object
  email <- emayili::envelope() |>
    emayili::from(smtp_user) |>
    emayili::to(to_email) |>
    emayili::subject("Activate your AstroCalculation Account") |>
    emayili::text(paste0(
      "Welcome!\n\n",
      "Please click the link below to verify your account:\n",
      verify_link, "\n\n",
      "This link will expire in 24 hours."
    ))

  # 4. Send
  tryCatch({
    smtp <- emayili::server(
      host = smtp_host,
      port = smtp_port,
      username = smtp_user,
      password = smtp_pass,
      max_times = 1
    )
    smtp(email, verbose = FALSE)
    return(TRUE)
  }, error = function(e) {
    warning("Failed to send email: ", e$message)
    return(FALSE)
  })
}


#'
auth_create_session <- function(pool, user_id, duration_days = 7) {
  token <- uuid::UUIDgenerate()
  DBI::dbExecute(pool, DBI::sqlInterpolate(pool, "
    INSERT INTO app_sessions (session_token, user_entity_id, expires_at)
    VALUES (?token, ?uid, NOW() + interval ?days)
  ", token = token, uid = user_id, days = paste(duration_days, "days")))
  return(token)
}

#'
auth_validate_session <- function(pool, token) {
  if (is.null(token) || token == "") return(NULL)
  res <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool, "
    SELECT user_entity_id FROM app_sessions
    WHERE session_token = ?token AND expires_at > NOW()
  ", token = token))
  if (nrow(res) == 1) return(res$user_entity_id)
  return(NULL)
}

#' Validate email using regex
#' @param email character string. An email address
#' @return logical
#'
validate_email <- function(email) {
  # Standard regex for email validation
  pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
  return(grepl(pattern, email))
}

#' Validate password
#' @param password character string. Password should have more at least 8 characters, have a number and have a special character
#' @return logical
#'
validate_password <- function(password) {
  if (nchar(password) < 8) return(FALSE)
  if (!grepl("[0-9]", password)) return(FALSE) # Must have number
  if (!grepl("[^A-Za-z0-9]", password)) return(FALSE) # Must have special char
  return(TRUE)
}

#' Handle Google OAuth Login
#'
#' Checks if a Google user exists. If not, registers them.
#' @param pool The DB pool
#' @param email User's email from Google
#' @param google_id The unique 'sub' ID from Google
#' @param name User's display name
#' @return The user_entity_id to log in with
#'
auth_handle_oauth_user <- function(pool, email, google_id, name) {

  # 1. Check if user exists by Email
  # (We link by email because it's unique)
  existing <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
                                                        "SELECT user_entity_id, oauth_subject_id FROM auth_credentials WHERE email = ?email",
                                                        email = email))

  if (nrow(existing) > 0) {
    user_id <- existing$user_entity_id[1]

    # If this existing user hasn't been linked to Google yet, link them now
    if (is.na(existing$oauth_subject_id[1]) || existing$oauth_subject_id[1] == "") {
      DBI::dbExecute(pool, DBI::sqlInterpolate(pool, "
        UPDATE auth_credentials
        SET oauth_provider = 'google', oauth_subject_id = ?gid, is_verified = TRUE
        WHERE user_entity_id = ?uid
      ", gid = google_id, uid = user_id))
    }

    return(user_id)

  } else {
    # 2. Register New User (Auto-Verified)
    new_id <- paste0("user_", uuid::UUIDgenerate())

    pool::poolWithTransaction(pool, function(con) {
      # Create Credentials (No password hash needed for OAuth-only, but we put a placeholder)
      DBI::dbExecute(con, DBI::sqlInterpolate(con, "
        INSERT INTO auth_credentials (
          user_entity_id, email, password_hash, salt,
          is_verified, oauth_provider, oauth_subject_id, created_at
        ) VALUES (
          ?uid, ?email, 'OAUTH_USER', '-',
          TRUE, 'google', ?gid, NOW()
        )
      ", uid = new_id, email = email, gid = google_id))

      # Create Profile
      DBI::dbExecute(con, DBI::sqlInterpolate(con, "
        INSERT INTO user_profiles (user_entity_id, display_name, valid_from)
        VALUES (?uid, ?name, NOW())
      ", uid = new_id, name = name))
    })

    return(new_id)
  }
}
