#' @title Authentication Service Functions
#' @description Handles Registration, Login verification, and Session management.
#'
#' @param pool The DB connection pool
#' @param user_id Custom User ID (specified by user)
#' @param email User email
#' @param password Cleartext password
#' @param display_name User's public name
#' @param terms_accepted Logical. Must be TRUE; registration is hard-blocked otherwise.
#' @param oracle_voice_preference Character. "Living Spark" (AI) or "Ancient Echo" (Static).
#'   Defaults to "Living Spark".
#'
#' @importFrom sodium password_store password_verify
#' @importFrom DBI dbGetQuery dbExecute sqlInterpolate
#' @importFrom uuid UUIDgenerate
#' @return list(user_id, verification_token) if successful
#'

auth_register_user <- function(pool, user_id, email, password, display_name,
                               terms_accepted = FALSE,
                               oracle_voice_preference = "Living Spark") {
  # 1. Consent gate — hard fail before any DB work
  if (!isTRUE(terms_accepted)) {
    stop("Registration requires acceptance of the Terms of Use.")
  }

  # 2. Field validation
  if (is.null(user_id) || user_id == "") stop("User ID is required")

  if (!validate_email(email)) {
    stop("Invalid email format.")
  }

  validate_password(password)

  # Validate voice preference value
  valid_voices <- c("Living Spark", "Ancient Echo")
  if (!oracle_voice_preference %in% valid_voices) {
    stop(paste("oracle_voice_preference must be one of:", paste(valid_voices, collapse = ", ")))
  }

  # 3. Check Uniqueness (ID and Email)
  id_check <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
    "SELECT 1 FROM auth_credentials WHERE user_entity_id = ?id",
    id = user_id
  ))
  if (nrow(id_check) > 0) stop("This User ID is already taken.")

  email_check <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
    "SELECT 1 FROM auth_credentials WHERE email = ?email",
    email = email
  ))
  if (nrow(email_check) > 0) stop("This Email is already registered.")

  # 4. Preparation
  hashed_pw <- sodium::password_store(password)
  # Generate a random verification token
  verif_token <- uuid::UUIDgenerate()

  # 5. Transaction: Insert Account -> Then Profile
  pool::poolWithTransaction(pool, function(con) {
    # Insert Credential with terms_accepted_at (is_verified = FALSE)
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      INSERT INTO auth_credentials (
        user_entity_id, email, password_hash,
        is_verified, verification_token, verification_token_expires_at,
        terms_accepted_at, created_at
      ) VALUES (
        ?id, ?email, ?hash,
        FALSE, ?token, NOW() + INTERVAL '24 hours',
        NOW(), NOW()
      )
    ", id = user_id, email = email, hash = hashed_pw, token = verif_token))

    # Insert Profile with oracle_voice_preference
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      INSERT INTO user_profiles (
        user_entity_id, display_name, oracle_voice_preference, valid_from
      ) VALUES (
        ?id, ?name, ?voice, NOW()
      )
    ", id = user_id, name = display_name, voice = oracle_voice_preference))
  })

  app_url <- normalize_app_base_url(Sys.getenv("APP_BASE_URL", "http://127.0.0.1:3000"))

  tryCatch(
    {
      send_verification_email(email, verif_token, app_url)
    },
    error = function(e) warning("Email send failed: ", e$message)
  )

  return(list(user_id = user_id, verification_token = verif_token))
}

#' Verify Email Address
#' @param pool db connection object
#' @param token The verification string from the email link
#' @return TRUE if successful, FALSE otherwise
#'
auth_verify_email <- function(pool, token) {
  if (is.null(token) || token == "") {
    return(FALSE)
  }

  # Find user with this token
  res <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool, "
    SELECT user_entity_id FROM auth_credentials WHERE verification_token = ?token
    AND (verification_token_expires_at IS NULL OR verification_token_expires_at > NOW())
  ", token = token))

  if (nrow(res) == 0) {
    return(FALSE)
  }

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
  # 1. Find User & Hash by Email OR User ID (including lockout fields)
  res <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
    "SELECT user_entity_id, password_hash, is_verified, failed_attempts, locked_until
     FROM auth_credentials
     WHERE user_entity_id = ?input OR email = ?input",
    input = login_id
  ))

  if (nrow(res) == 0) {
    return(NULL)
  }

  # 2. Check if account is locked
  if (!is.na(res$locked_until) && res$locked_until > Sys.time()) {
    return(list(locked = TRUE, locked_until = res$locked_until))
  }

  # 3. Verify Hash
  is_valid <- sodium::password_verify(res$password_hash, password)

  if (is_valid) {
    # Reset failed attempts and update last login
    DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
      "UPDATE auth_credentials
       SET last_login = NOW(), failed_attempts = 0, locked_until = NULL
       WHERE user_entity_id = ?id",
      id = res$user_entity_id
    ))

    return(list(id = res$user_entity_id, verified = res$is_verified))
  } else {
    # Increment failed attempts
    new_attempts <- res$failed_attempts + 1

    if (new_attempts >= 5) {
      # Lock account for 15 minutes
      DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
        "UPDATE auth_credentials
         SET failed_attempts = ?attempts, locked_until = NOW() + INTERVAL '15 minutes'
         WHERE user_entity_id = ?id",
        attempts = new_attempts, id = res$user_entity_id
      ))
      # Log the lockout event for security tracking
      tryCatch(DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
        "INSERT INTO auth_security_log
           (user_entity_id, event_type, failed_attempts_at_event, was_locked)
         VALUES (?uid, 'account_locked', ?attempts, FALSE)",
        uid = res$user_entity_id, attempts = new_attempts
      )), error = function(e) warning("Security log insert failed: ", e$message))
    } else {
      # Just increment the counter
      DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
        "UPDATE auth_credentials
         SET failed_attempts = ?attempts
         WHERE user_entity_id = ?id",
        attempts = new_attempts, id = res$user_entity_id
      ))
    }

    return(NULL)
  }
}

#' Trigger Password Reset
#' @param pool db connection
#' @param email user email
#' @param ttl_minutes time-to-live in minutes for the reset token
#' @return TRUE if token issued, FALSE if user not found
#'
auth_trigger_password_reset <- function(pool, email, ttl_minutes = 30) {
  if (!validate_email(email)) stop("Invalid email format.")

  user_row <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
    "SELECT user_entity_id FROM auth_credentials WHERE email = ?email",
    email = email
  ))

  if (nrow(user_row) == 0) {
    return(FALSE)
  }

  token <- uuid::UUIDgenerate()
  expires_at <- Sys.time() + ttl_minutes * 60

  DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
    "UPDATE auth_credentials SET reset_token = ?token, reset_token_expires_at = ?exp WHERE user_entity_id = ?id",
    token = token, exp = expires_at, id = user_row$user_entity_id[1]
  ))

  app_url <- normalize_app_base_url(Sys.getenv("APP_BASE_URL", "http://127.0.0.1:3000"))

  tryCatch(
    {
      send_reset_email(email, token, app_url)
    },
    error = function(e) warning("Reset email send failed: ", e$message)
  )

  return(TRUE)
}

#' Reset Password Using Token
#' @param pool db connection
#' @param token reset token from email link
#' @param new_password new password string
#' @return TRUE if reset succeeded, FALSE if token invalid/expired
#'
auth_reset_password <- function(pool, token, new_password) {
  if (is.null(token) || token == "") {
    return(FALSE)
  }

  validate_password(new_password)

  # Validate token & expiry
  res <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool, "
    SELECT user_entity_id FROM auth_credentials
    WHERE reset_token = ?token
      AND reset_token_expires_at IS NOT NULL
      AND reset_token_expires_at > NOW()
  ", token = token))

  if (nrow(res) == 0) {
    return(FALSE)
  }

  hashed_pw <- sodium::password_store(new_password)

  # Fetch current lockout state before resetting (for the security log)
  current_state <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
    "SELECT failed_attempts, locked_until FROM auth_credentials WHERE user_entity_id = ?id",
    id = res$user_entity_id[1]
  ))
  was_locked <- !is.na(current_state$locked_until[1]) && current_state$locked_until[1] > Sys.time()

  # Log the password reset event (immutable record) before making any changes
  tryCatch(DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
    "INSERT INTO auth_security_log
       (user_entity_id, event_type, failed_attempts_at_event, was_locked)
     VALUES (?uid, 'password_reset', ?attempts, ?locked)",
    uid = res$user_entity_id[1],
    attempts = current_state$failed_attempts[1],
    locked = was_locked
  )), error = function(e) warning("Security log insert failed: ", e$message))

  DBI::dbExecute(pool, DBI::sqlInterpolate(pool, "
    UPDATE auth_credentials
    SET password_hash = ?hash,
        reset_token = NULL,
        reset_token_expires_at = NULL,
        is_verified = TRUE,
        failed_attempts = 0,
        locked_until = NULL
    WHERE user_entity_id = ?id
  ", hash = hashed_pw, id = res$user_entity_id[1]))

  return(TRUE)
}

#' Create SMTP Server Object
#' @return emayili server object or NULL if config is missing
create_smtp_server <- function() {
  smtp_user <- Sys.getenv("SMTP_USERNAME")
  smtp_pass <- Sys.getenv("SMTP_PASSWORD")
  smtp_host <- Sys.getenv("SMTP_HOST", "smtp.gmail.com")
  smtp_port <- as.numeric(Sys.getenv("SMTP_PORT", "465"))

  if (smtp_user == "" || smtp_pass == "") {
    warning("SMTP credentials missing.")
    return(NULL)
  }

  emayili::server(
    host = smtp_host,
    port = smtp_port,
    username = smtp_user,
    password = smtp_pass,
    max_times = 1
  )
}

#' Normalize application base URL from environment/config
#'
#' @param base_url Character. Raw base URL, often sourced from `APP_BASE_URL`.
#' @return Character scalar with surrounding quotes and trailing slashes removed.
normalize_app_base_url <- function(base_url = "http://127.0.0.1:3000") {
  if (is.null(base_url) || length(base_url) == 0) {
    return("http://127.0.0.1:3000")
  }

  normalized <- trimws(as.character(base_url[1]))
  normalized <- gsub("^[\"']+|[\"']+$", "", normalized)
  normalized <- sub("/+$", "", normalized)

  if (!nzchar(normalized)) {
    return("http://127.0.0.1:3000")
  }

  normalized
}

#' Build a query-token link for application emails
#'
#' @param base_url Character. Application base URL.
#' @param query_key Character. Query parameter name, e.g. `reset`.
#' @param token Character. Verification or reset token.
#' @return Character URL.
build_app_token_link <- function(base_url, query_key, token) {
  paste0(
    normalize_app_base_url(base_url),
    "/?",
    query_key,
    "=",
    utils::URLencode(as.character(token), reserved = TRUE)
  )
}

#' Send Verification Email via SMTP
#'
#' @param to_email The recipient's email address
#' @param token The unique verification token
#' @param base_url The base URL of your Shiny app (for the link)
#' @return TRUE if email sent successfully, FALSE otherwise
#' @importFrom emayili envelope from to subject server render
#'
send_verification_email <- function(to_email, token, base_url = "http://127.0.0.1:3000") {
  verify_link <- build_app_token_link(base_url, "verify", token)

  if (Sys.getenv("TESTTHAT") == "true" && Sys.getenv("LIVE_EMAIL_TEST") != "true") {
    message(sprintf(" [TEST MODE] Email suppressed. Link: %s", verify_link))
    return(TRUE)
  }

  smtp <- create_smtp_server()
  if (is.null(smtp)) {
    warning("Email not sent. Printing link to console.")
    print(paste("VERIFICATION LINK:", verify_link))
    return(FALSE)
  }

  email <- emayili::envelope() |>
    emayili::from(Sys.getenv("SMTP_USERNAME")) |>
    emayili::to(to_email) |>
    emayili::subject("Activate your AstroCalculation Account") |>
    emayili::text(paste0(
      "Welcome!\n\n",
      "Please click the link below to verify your account:\n",
      verify_link, "\n\n",
      "This link will expire in 24 hours."
    ))

  tryCatch(
    {
      smtp(email, verbose = FALSE)
      return(TRUE)
    },
    error = function(e) {
      warning("Failed to send email: ", e$message)
      return(FALSE)
    }
  )
}

#' Send Password Reset Email via SMTP
#'
#' @param to_email The recipient's email address
#' @param token The reset token string
#' @param base_url The base URL of the application
#' @return TRUE if email sent successfully, FALSE otherwise
#' @importFrom emayili envelope from to subject server render
#'
send_reset_email <- function(to_email, token, base_url = "http://127.0.0.1:3000") {
  reset_link <- build_app_token_link(base_url, "reset", token)

  if (Sys.getenv("TESTTHAT") == "true" && Sys.getenv("LIVE_EMAIL_TEST") != "true") {
    message(sprintf(" [TEST MODE] Reset email suppressed. Link: %s", reset_link))
    return(TRUE)
  }

  smtp <- create_smtp_server()
  if (is.null(smtp)) {
    warning("Reset email not sent.")
    return(FALSE)
  }

  email <- emayili::envelope() |>
    emayili::from(Sys.getenv("SMTP_USERNAME")) |>
    emayili::to(to_email) |>
    emayili::subject("Reset your AstroCalculation password") |>
    emayili::text(paste0(
      "\u60a8\u8981\u6c42\u91cd\u8a2d\u5bc6\u78bc\u3002\n\n",
      "\u8acb\u9ede\u64ca\u4e0b\u5217\u9023\u7d50\u5b8c\u6210\u91cd\u8a2d\uff0830 \u5206\u9418\u5167\u6709\u6548\uff09\uff1a\n",
      reset_link, "\n\n",
      "\u5982\u679c\u9019\u4e0d\u662f\u60a8\u672c\u4eba\u64cd\u4f5c\uff0c\u8acb\u5ffd\u7565\u6b64\u90f5\u4ef6\u3002"
    ))

  tryCatch(
    {
      smtp(email, verbose = FALSE)
      return(TRUE)
    },
    error = function(e) {
      warning("Failed to send reset email: ", e$message)
      return(FALSE)
    }
  )
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
  if (is.null(token) || token == "") {
    return(NULL)
  }
  res <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool, "
    SELECT user_entity_id FROM app_sessions
    WHERE session_token = ?token AND expires_at > NOW()
  ", token = token))
  if (nrow(res) == 1) {
    return(res$user_entity_id)
  }
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
  if (nchar(password) < 8) {
    stop("Password must be at least 8 characters long.")
  }
  if (!grepl("[0-9]", password)) {
    stop("Password must contain at least one number.")
  }
  if (!grepl("[^A-Za-z0-9]", password)) {
    stop("Password must contain at least one special character.")
  }
  return(TRUE)
}

#' Handle Google OAuth Login/Registration
#'
#' Checks if a Google user exists. If not, registers them.
#' @param pool The DB pool
#' @param email User's email from Google
#' @param google_id The unique 'sub' ID from Google
#' @param name User's display name
#' @return The user_entity_id to log in with
#' @export
auth_handle_oauth_user <- function(pool, email, google_id, name) {
  # 1. Check if user exists by Email (Link accounts)
  existing <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
    "SELECT user_entity_id, oauth_subject_id FROM auth_credentials WHERE email = ?email",
    email = email
  ))

  if (nrow(existing) > 0) {
    user_id <- existing$user_entity_id[1]

    # If not yet linked, link now & auto-verify
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
    # We use the email prefix as a default username (user can change later if we build that feature)
    # or generate a UUID. Let's use a UUID for safety to avoid collisions.
    new_uid <- uuid::UUIDgenerate()

    pool::poolWithTransaction(pool, function(con) {
      # Create Credentials (No password hash needed for OAuth-only)
      DBI::dbExecute(con, DBI::sqlInterpolate(con, "
        INSERT INTO auth_credentials (
          user_entity_id, email, password_hash,
          is_verified, oauth_provider, oauth_subject_id, created_at
        ) VALUES (
          ?uid, ?email, NULL,
          TRUE, 'google', ?gid, NOW()
        )
      ", uid = new_uid, email = email, gid = google_id))

      # Create Profile
      DBI::dbExecute(con, DBI::sqlInterpolate(con, "
        INSERT INTO user_profiles (user_entity_id, display_name, valid_from)
        VALUES (?uid, ?name, NOW())
      ", uid = new_uid, name = name))
    })

    return(new_uid)
  }
}
