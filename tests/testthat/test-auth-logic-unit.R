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
    expect_error(validate_password("short1!")) # too short
    expect_error(validate_password("NoNumber!"))
    expect_error(validate_password("NoSpecial123"))
})

test_that("auth_verify_user returns NULL when user not found", {
    pool <- make_mock_conn(query_res = data.frame())
    res <- auth_verify_user(pool, "id", "pw")
    expect_null(res)
})

test_that("auth_verify_user returns id and updates last_login on success", {
    exec_calls <- list(count = 0)

    pool <- make_mock_conn(
        query_res = data.frame(user_entity_id = "u-1", password_hash = sodium::password_store("pw"), is_verified = TRUE),
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
    exec_calls <- list(count = 0)

    pool <- make_mock_conn(
        query_res = data.frame(),
        exec_side = function() exec_calls$count <<- exec_calls$count + 1
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

test_that("auth_reset_password fails on invalid token", {
    pool <- make_mock_conn(query_res = data.frame())
    res <- auth_reset_password(pool, "bad-token", "Abcd123!")
    expect_false(res)
})

test_that("auth_reset_password updates password and clears token", {
    exec_calls <- list(count = 0)
    pool <- make_mock_conn(
        query_res = list(
            data.frame(user_entity_id = "uid-1", stringsAsFactors = FALSE)
        ),
        exec_side = function() exec_calls$count <<- exec_calls$count + 1
    )

    res <- auth_reset_password(pool, "token-ok", "Abcd123!")
    expect_true(res)
    expect_equal(exec_calls$count, 1)
})
