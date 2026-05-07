library(testthat)

mock_conn_class <- function() {
    if (!methods::isClass("MockConn")) methods::setClass("MockConn", contains = "DBIConnection")
}

make_mock_conn <- function(query_res = data.frame(), exec_side = NULL) {
    mock_conn_class()

    # Allow sequential query results (list) or single data.frame
    res_list <- if (is.list(query_res) && !is.data.frame(query_res)) query_res else list(query_res)
    res_idx <- 1L

    methods::setMethod(
        "sqlInterpolate",
        signature(conn = "MockConn", sql = "character"),
        function(conn, sql, ...) sql
    )

    methods::setMethod(
        "dbGetQuery",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            res <- res_list[[min(res_idx, length(res_list))]]
            res_idx <<- res_idx + 1L
            res
        }
    )

    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            if (!is.null(exec_side)) exec_side()
            1
        }
    )

    new("MockConn")
}

# Unit-level tests for db_auth_logic without live DB

test_that("validate_email enforces basic format", {
    expect_true(validate_email("user@example.com"))
    expect_false(validate_email("bad-email"))
    expect_false(validate_email("@example.com"))
})

test_that("validate_password enforces length, number, special", {
    expect_true(validate_password("Abcd123!"))
    expect_error(validate_password("short1!"), "at least 8 characters")
    expect_error(validate_password("NoNumber!"), "at least one number")
    expect_error(validate_password("NoSpecial123"), "at least one special character")
})

test_that("auth_verify_user returns NULL when user not found", {
    pool <- make_mock_conn(query_res = data.frame())
    res <- auth_verify_user(pool, "id", "pw")
    expect_null(res)
})

test_that("auth_verify_user returns id and updates last_login on success", {
    exec_calls <- list(count = 0)

    pool <- make_mock_conn(
        query_res = data.frame(
            user_entity_id = "u-1",
            password_hash = sodium::password_store("pw"),
            is_verified = TRUE,
            failed_attempts = 0,
            locked_until = as.POSIXct(NA)
        ),
        exec_side = function() exec_calls$count <<- exec_calls$count + 1
    )

    res <- with_mocked_bindings(
        {
            auth_verify_user(pool, "login", "pw")
        },
        password_verify = function(hash, pwd) {
            expect_equal(hash, "hash")
            expect_equal(pwd, "pw")
            TRUE
        },
        .env = asNamespace("sodium")
    )

    expect_equal(res$id, "u-1")
    expect_true(res$verified)
    expect_equal(exec_calls$count, 1)
})

test_that("auth_handle_oauth_user links existing user", {
    exec_calls <- list(count = 0)

    pool <- make_mock_conn(
        query_res = data.frame(user_entity_id = "u-existing", oauth_subject_id = NA_character_),
        exec_side = function() exec_calls$count <<- exec_calls$count + 1
    )

    res <- with_mocked_bindings(
        {
            auth_handle_oauth_user(pool, "a@b.com", "gid-1", "Alice")
        },
        UUIDgenerate = function() "uuid-unused",
        .env = asNamespace("uuid")
    )

    expect_equal(res, "u-existing")
    expect_equal(exec_calls$count, 1) # update executed
})

test_that("auth_handle_oauth_user creates new user when none exists", {
    exec_calls <- list(count = 0, statements = character(0))

    pool <- make_mock_conn(
        query_res = data.frame()
    )

    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            exec_calls$count <<- exec_calls$count + 1
            exec_calls$statements <<- c(exec_calls$statements, statement)
            1
        }
    )

    res <- {
        ns_pool <- asNamespace("pool")
        old_tx <- get("poolWithTransaction", envir = ns_pool)
        unlockBinding("poolWithTransaction", ns_pool)
        assign("poolWithTransaction", function(pool_obj, code) code(pool_obj), envir = ns_pool)
        on.exit(
            {
                assign("poolWithTransaction", old_tx, envir = ns_pool)
                lockBinding("poolWithTransaction", ns_pool)
            },
            add = TRUE
        )

        ns_uuid <- asNamespace("uuid")
        old_uuid <- get("UUIDgenerate", envir = ns_uuid)
        unlockBinding("UUIDgenerate", ns_uuid)
        assign("UUIDgenerate", function(...) "new-uid", envir = ns_uuid)
        on.exit(
            {
                assign("UUIDgenerate", old_uuid, envir = ns_uuid)
                lockBinding("UUIDgenerate", ns_uuid)
            },
            add = TRUE
        )

        auth_handle_oauth_user(pool, "a@b.com", "gid-1", "Alice")
    }

    expect_match(res, "new-uid")
    expect_equal(exec_calls$count, 2) # credentials + profile inserts

    profile_stmt <- exec_calls$statements[
        grepl("INSERT INTO user_profiles", exec_calls$statements, fixed = TRUE)
    ]
    expect_length(profile_stmt, 1L)
    expect_true(grepl("entry_id", profile_stmt, fixed = TRUE))
    expect_true(grepl("display_name", profile_stmt, fixed = TRUE))
})

test_that("auth_trigger_password_reset returns FALSE when email missing", {
    pool <- make_mock_conn(query_res = data.frame())
    res <- auth_trigger_password_reset(pool, "missing@example.com", ttl_minutes = 30)
    expect_false(res)
})

test_that("auth_trigger_password_reset writes token and expiry", {
    exec_calls <- list(count = 0)
    pool <- make_mock_conn(
        query_res = data.frame(user_entity_id = "uid-1"),
        exec_side = function() exec_calls$count <<- exec_calls$count + 1
    )

    res <- with_mocked_bindings(
        auth_trigger_password_reset(pool, "user@example.com", ttl_minutes = 30),
        UUIDgenerate = function(...) "token-123",
        .env = asNamespace("uuid")
    )

    expect_true(res)
    expect_equal(exec_calls$count, 1)
})

test_that("email token links strip deployment quotes from APP_BASE_URL", {
    expect_equal(
        build_app_token_link('"https://uat.astro-roots.com"', "reset", "token-123"),
        "https://uat.astro-roots.com/?reset=token-123"
    )
    expect_equal(
        build_app_token_link("https://uat.astro-roots.com/", "verify", "token-123"),
        "https://uat.astro-roots.com/?verify=token-123"
    )
})

test_that("auth_reset_password fails on invalid token", {
    pool <- make_mock_conn(query_res = data.frame())
    res <- auth_reset_password(pool, "bad-token", "Abcd123!")
    expect_false(res)
})

test_that("auth_reset_password updates password and clears token", {
    exec_calls <- list(count = 0)
    # Now needs 2 query results: token validation + current state fetch
    pool <- make_mock_conn(
        query_res = list(
            data.frame(user_entity_id = "uid-1", stringsAsFactors = FALSE),
            data.frame(failed_attempts = 0L, locked_until = as.POSIXct(NA),
                       stringsAsFactors = FALSE)
        ),
        exec_side = function() exec_calls$count <<- exec_calls$count + 1
    )

    res <- auth_reset_password(pool, "token-ok", "Abcd123!")
    expect_true(res)
    # 2 dbExecute calls: security log INSERT + credentials UPDATE
    expect_equal(exec_calls$count, 2)
})

# Story 1.3: Account Lockout Tests

test_that("auth_verify_user increments failed_attempts on wrong password", {
    exec_calls <- list(count = 0, statement = "")

    pool <- make_mock_conn(
        query_res = data.frame(
            user_entity_id = "u-1",
            password_hash = sodium::password_store("correct_pw"),
            is_verified = TRUE,
            failed_attempts = 2,
            locked_until = as.POSIXct(NA)
        )
    )

    # Override dbExecute to capture the statement
    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            exec_calls$statement <<- statement
            exec_calls$count <<- exec_calls$count + 1
            1
        }
    )

    res <- auth_verify_user(pool, "u-1", "wrong_pw")

    expect_null(res)
    expect_equal(exec_calls$count, 1)
    # Check that statement updates failed_attempts (value will be interpolated)
    expect_true(grepl("SET failed_attempts", exec_calls$statement, fixed = TRUE))
    expect_false(grepl("locked_until", exec_calls$statement, fixed = TRUE))
})

test_that("auth_verify_user locks account after 5 failed attempts", {
    exec_calls <- list(count = 0, statements = character(0))

    pool <- make_mock_conn(
        query_res = data.frame(
            user_entity_id = "u-1",
            password_hash = sodium::password_store("correct_pw"),
            is_verified = TRUE,
            failed_attempts = 4,
            locked_until = as.POSIXct(NA)
        )
    )

    # Override dbExecute to capture all statements (list, not single)
    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            exec_calls$statements <<- c(exec_calls$statements, statement)
            exec_calls$count <<- exec_calls$count + 1
            1
        }
    )

    res <- auth_verify_user(pool, "u-1", "wrong_pw")

    expect_null(res)
    # 2 dbExecute calls: UPDATE locked_until + INSERT security log
    expect_equal(exec_calls$count, 2)
    # First statement is the lockout UPDATE
    expect_true(grepl("SET failed_attempts", exec_calls$statements[1], fixed = TRUE))
    expect_true(grepl("locked_until", exec_calls$statements[1], fixed = TRUE))
    expect_true(grepl("15 minutes", exec_calls$statements[1], fixed = TRUE))
    # Second statement is the security log INSERT
    expect_true(grepl("auth_security_log", exec_calls$statements[2], fixed = TRUE))
})

test_that("auth_verify_user resets failed_attempts on successful login", {
    exec_calls <- list(count = 0, statement = "")

    pool <- make_mock_conn(
        query_res = data.frame(
            user_entity_id = "u-1",
            password_hash = sodium::password_store("correct_pw"),
            is_verified = TRUE,
            failed_attempts = 3,
            locked_until = as.POSIXct(NA)
        )
    )

    # Override dbExecute to capture the statement
    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            exec_calls$statement <<- statement
            exec_calls$count <<- exec_calls$count + 1
            1
        }
    )

    res <- auth_verify_user(pool, "u-1", "correct_pw")

    expect_equal(res$id, "u-1")
    expect_true(res$verified)
    expect_equal(exec_calls$count, 1)
    expect_true(grepl("failed_attempts = 0", exec_calls$statement, fixed = TRUE))
})

test_that("auth_verify_user returns locked status for locked account", {
    future_time <- Sys.time() + 600  # 10 minutes from now

    pool <- make_mock_conn(
        query_res = data.frame(
            user_entity_id = "u-1",
            password_hash = sodium::password_store("correct_pw"),
            is_verified = TRUE,
            failed_attempts = 5,
            locked_until = future_time
        )
    )

    res <- auth_verify_user(pool, "u-1", "correct_pw")

    expect_true(!is.null(res$locked))
    expect_true(res$locked)
    expect_equal(res$locked_until, future_time)
})

test_that("auth_verify_user allows login after lockout expires", {
    exec_calls <- list(count = 0)
    past_time <- Sys.time() - 600  # 10 minutes ago

    pool <- make_mock_conn(
        query_res = data.frame(
            user_entity_id = "u-1",
            password_hash = sodium::password_store("correct_pw"),
            is_verified = TRUE,
            failed_attempts = 5,
            locked_until = past_time
        ),
        exec_side = function() exec_calls$count <<- exec_calls$count + 1
    )

    res <- auth_verify_user(pool, "u-1", "correct_pw")

    expect_equal(res$id, "u-1")
    expect_true(res$verified)
    expect_equal(exec_calls$count, 1)
})

# Security Logging Tests -------------------------------------------------------

test_that("auth_verify_user inserts security log when account is locked", {
    exec_calls <- list(count = 0, statements = character(0))

    pool <- make_mock_conn(
        query_res = data.frame(
            user_entity_id = "u-1",
            password_hash = sodium::password_store("correct_pw"),
            is_verified = TRUE,
            failed_attempts = 4L,
            locked_until = as.POSIXct(NA)
        )
    )

    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            exec_calls$statements <<- c(exec_calls$statements, statement)
            exec_calls$count <<- exec_calls$count + 1L
            1
        }
    )

    res <- auth_verify_user(pool, "u-1", "wrong_pw")

    expect_null(res)
    # Expected: 1st dbExecute = UPDATE locked_until, 2nd = INSERT security log
    expect_equal(exec_calls$count, 2L)
    expect_true(any(grepl("auth_security_log", exec_calls$statements, fixed = TRUE)))
    expect_true(any(grepl("account_locked", exec_calls$statements, fixed = TRUE)))
})

test_that("auth_verify_user does NOT insert security log for non-locking failure", {
    exec_calls <- list(count = 0, statements = character(0))

    pool <- make_mock_conn(
        query_res = data.frame(
            user_entity_id = "u-1",
            password_hash = sodium::password_store("correct_pw"),
            is_verified = TRUE,
            failed_attempts = 2L,
            locked_until = as.POSIXct(NA)
        )
    )

    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            exec_calls$statements <<- c(exec_calls$statements, statement)
            exec_calls$count <<- exec_calls$count + 1L
            1
        }
    )

    res <- auth_verify_user(pool, "u-1", "wrong_pw")

    expect_null(res)
    # Only 1 dbExecute: UPDATE failed_attempts (no security log)
    expect_equal(exec_calls$count, 1L)
    expect_false(any(grepl("auth_security_log", exec_calls$statements, fixed = TRUE)))
})

test_that("auth_reset_password inserts security log before unlocking", {
    exec_calls <- list(count = 0, statements = character(0))

    # make_mock_conn with sequential query results:
    # 1st dbGetQuery = token validation (user found)
    # 2nd dbGetQuery = fetch current lockout state
    pool <- make_mock_conn(
        query_res = list(
            data.frame(user_entity_id = "uid-1", stringsAsFactors = FALSE),
            data.frame(failed_attempts = 5L, locked_until = Sys.time() + 300,
                       stringsAsFactors = FALSE)
        )
    )

    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            exec_calls$statements <<- c(exec_calls$statements, statement)
            exec_calls$count <<- exec_calls$count + 1L
            1
        }
    )

    res <- auth_reset_password(pool, "token-ok", "Abcd123!")

    expect_true(res)
    # Expected: 1st = INSERT security log, 2nd = UPDATE credentials (unlock)
    expect_equal(exec_calls$count, 2L)

    # Security log INSERT comes first
    expect_true(grepl("auth_security_log", exec_calls$statements[1], fixed = TRUE))
    expect_true(grepl("password_reset", exec_calls$statements[1], fixed = TRUE))

    # Credential UPDATE comes second and contains unlock fields
    expect_true(grepl("failed_attempts = 0", exec_calls$statements[2], fixed = TRUE))
    expect_true(grepl("locked_until = NULL", exec_calls$statements[2], fixed = TRUE))
})

test_that("auth_reset_password security log records was_locked = TRUE when account locked", {
    exec_calls <- list(statements = character(0))
    future_lock <- Sys.time() + 600

    pool <- make_mock_conn(
        query_res = list(
            data.frame(user_entity_id = "uid-1", stringsAsFactors = FALSE),
            data.frame(failed_attempts = 5L, locked_until = future_lock,
                       stringsAsFactors = FALSE)
        )
    )

    # Override sqlInterpolate to do real named-param substitution so we can
    # check actual values (TRUE/FALSE) in the resulting SQL string
    methods::setMethod(
        "sqlInterpolate",
        signature(conn = "MockConn", sql = "character"),
        function(conn, sql, ...) {
            args <- list(...)
            for (nm in names(args)) {
                val <- args[[nm]]
                repl <- if (is.logical(val)) toupper(as.character(val)) else as.character(val)
                sql <- gsub(paste0("?", nm), repl, sql, fixed = TRUE)
            }
            sql
        }
    )

    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            exec_calls$statements <<- c(exec_calls$statements, statement)
            1
        }
    )

    res <- auth_reset_password(pool, "token-ok", "Abcd123!")

    expect_true(res)
    log_stmt <- exec_calls$statements[grepl("auth_security_log", exec_calls$statements, fixed = TRUE)]
    expect_length(log_stmt, 1L)
    expect_true(grepl("TRUE", log_stmt, fixed = TRUE))   # was_locked = TRUE
})

test_that("auth_reset_password security log records was_locked = FALSE when not locked", {
    exec_calls <- list(statements = character(0))

    pool <- make_mock_conn(
        query_res = list(
            data.frame(user_entity_id = "uid-1", stringsAsFactors = FALSE),
            data.frame(failed_attempts = 0L, locked_until = as.POSIXct(NA),
                       stringsAsFactors = FALSE)
        )
    )

    # Override sqlInterpolate to do real named-param substitution
    methods::setMethod(
        "sqlInterpolate",
        signature(conn = "MockConn", sql = "character"),
        function(conn, sql, ...) {
            args <- list(...)
            for (nm in names(args)) {
                val <- args[[nm]]
                repl <- if (is.logical(val)) toupper(as.character(val)) else as.character(val)
                sql <- gsub(paste0("?", nm), repl, sql, fixed = TRUE)
            }
            sql
        }
    )

    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            exec_calls$statements <<- c(exec_calls$statements, statement)
            1
        }
    )

    res <- auth_reset_password(pool, "token-ok", "Abcd123!")

    expect_true(res)
    log_stmt <- exec_calls$statements[grepl("auth_security_log", exec_calls$statements, fixed = TRUE)]
    expect_length(log_stmt, 1L)
    expect_true(grepl("FALSE", log_stmt, fixed = TRUE))  # was_locked = FALSE
})

test_that("auth_reset_password still succeeds if security log insert fails", {
    exec_calls <- list(count = 0L)

    pool <- make_mock_conn(
        query_res = list(
            data.frame(user_entity_id = "uid-1", stringsAsFactors = FALSE),
            data.frame(failed_attempts = 3L, locked_until = as.POSIXct(NA),
                       stringsAsFactors = FALSE)
        )
    )

    call_num <- 0L
    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            call_num <<- call_num + 1L
            if (call_num == 1L) stop("auth_security_log does not exist") # simulate missing table
            exec_calls$count <<- exec_calls$count + 1L
            1
        }
    )

    # Should NOT throw — security log failure is wrapped in tryCatch(warning(...))
    expect_warning(
        res <- auth_reset_password(pool, "token-ok", "Abcd123!"),
        "Security log insert failed"
    )
    expect_true(res)
    expect_equal(exec_calls$count, 1L) # UPDATE still executed
})

test_that("auth_register_user hard-fails when terms_accepted is FALSE", {
    pool <- make_mock_conn()
    expect_error(
        auth_register_user(pool, "uid-1", "a@b.com", "Abcd123!", "Alice",
                           terms_accepted = FALSE),
        "Terms of Use"
    )
})

test_that("auth_register_user hard-fails when terms_accepted is missing (default)", {
    pool <- make_mock_conn()
    expect_error(
        auth_register_user(pool, "uid-1", "a@b.com", "Abcd123!", "Alice"),
        "Terms of Use"
    )
})

test_that("auth_register_user inserts credentials and profile when consented", {
    exec_calls <- list(count = 0L, statements = character(0))

    pool <- make_mock_conn(
        query_res = list(data.frame(), data.frame())
    )

    methods::setMethod(
        "dbExecute",
        signature(conn = "MockConn", statement = "character"),
        function(conn, statement, ...) {
            exec_calls$count <<- exec_calls$count + 1L
            exec_calls$statements <<- c(exec_calls$statements, statement)
            1
        }
    )

    ns_pool <- asNamespace("pool")
    old_tx <- get("poolWithTransaction", envir = ns_pool)
    unlockBinding("poolWithTransaction", ns_pool)
    assign("poolWithTransaction", function(pool_obj, code) code(pool_obj), envir = ns_pool)
    on.exit({
        assign("poolWithTransaction", old_tx, envir = ns_pool)
        lockBinding("poolWithTransaction", ns_pool)
    }, add = TRUE)

    ns_uuid <- asNamespace("uuid")
    old_uuid <- get("UUIDgenerate", envir = ns_uuid)
    uuid_values <- c("verif-tok", "profile-entry")
    unlockBinding("UUIDgenerate", ns_uuid)
    assign("UUIDgenerate", function(...) {
        value <- uuid_values[1]
        uuid_values <<- uuid_values[-1]
        value
    }, envir = ns_uuid)
    on.exit({
        assign("UUIDgenerate", old_uuid, envir = ns_uuid)
        lockBinding("UUIDgenerate", ns_uuid)
    }, add = TRUE)

    res <- with_mocked_bindings(
        {
            with_mocked_bindings(
                {
                    auth_register_user(
                        pool, "uid-ok", "ok@b.com", "Abcd123!", "Bob",
                        terms_accepted = TRUE
                    )
                },
                password_store = function(pwd) "hashed",
                .env = asNamespace("sodium")
            )
        },
        send_verification_email = function(...) TRUE,
        .env = asNamespace("astrocalculation")
    )

    expect_equal(res$user_id, "uid-ok")
    expect_equal(res$verification_token, "verif-tok")
    expect_equal(exec_calls$count, 2L)

    credential_stmt <- exec_calls$statements[
        grepl("INSERT INTO auth_credentials", exec_calls$statements, fixed = TRUE)
    ]
    profile_stmt <- exec_calls$statements[
        grepl("INSERT INTO user_profiles", exec_calls$statements, fixed = TRUE)
    ]

    expect_length(credential_stmt, 1L)
    expect_true(grepl("terms_accepted_at", credential_stmt, fixed = TRUE))
    expect_true(grepl("verification_token", credential_stmt, fixed = TRUE))

    expect_length(profile_stmt, 1L)
    expect_true(grepl("entry_id", profile_stmt, fixed = TRUE))
    expect_true(grepl("display_name", profile_stmt, fixed = TRUE))
})
