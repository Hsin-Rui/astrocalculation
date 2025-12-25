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
#'

auth_register_user <- function(pool, user_id, email, password, display_name) {
  # 1. Validation
  if (is.null(user_id) || user_id == "") stop("User ID is required")
  if (is.null(email) || email == "") stop("Email is required")
  if (is.null(password) || nchar(password) < 6) stop("Password must be at least 6 chars")

  # 2. Check Uniqueness (ID and Email)
  # Check ID
  id_check <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
                                                        "SELECT 1 FROM auth_credentials WHERE user_entity_id = ?id",
                                                        id = user_id
  ))
  if (nrow(id_check) > 0) stop("This User ID is already taken.")

  # Check Email
  email_check <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool,
                                                           "SELECT 1 FROM auth_credentials WHERE email = ?email",
                                                           email = email
  ))
  if (nrow(email_check) > 0) stop("This Email is already registered.")

  # 3. Hash Password
  hashed_pw <- sodium::password_store(password)

  # 4. Transaction: Insert Account -> Then Profile
  pool::poolWithTransaction(pool, function(con) {

    # A. Insert Master Account (Using custom user_id)
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      INSERT INTO auth_credentials (
        user_entity_id, email, password_hash, salt
      ) VALUES (
        ?id, ?email, ?hash, '-'
      )
    ", id = user_id, email = email, hash = hashed_pw))

    # B. Insert Initial Profile
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      INSERT INTO user_profiles (
        user_entity_id, display_name, valid_from
      ) VALUES (
        ?id, ?name, NOW()
      )
    ", id = user_id, name = display_name))

  })

  return(user_id)
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
