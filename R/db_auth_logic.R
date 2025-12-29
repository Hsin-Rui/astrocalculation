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
#' @import uuid UUIDgenerate
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
        is_verified, verification_token, created_at
      ) VALUES (
        ?id, ?email, ?hash, '-',
        FALSE, ?token, NOW()
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
  creds <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
                                                     "SELECT user_entity_id, password_hash FROM auth_credentials
     WHERE email = ?input OR user_entity_id = ?input",
                                                     input = login_id
  ))

  if (nrow(creds) == 0) return(NULL)

  # 2. Verify Hash
  is_valid <- sodium::password_verify(creds$password_hash, password)

  if (is_valid) {
    user_id <- creds$user_entity_id
    # Update Last Login
    DBI::dbExecute(pool, DBI::sqlInterpolate(pool,
                                             "UPDATE auth_credentials SET last_login = NOW() WHERE user_entity_id = ?id",
                                             id = user_id
    ))
    return(user_id)
  } else {
    return(NULL)
  }
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
