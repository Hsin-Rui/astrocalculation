test_that("Postgres pools validate on checkout instead of a background timer", {
  old_env <- Sys.getenv(
    c("PGHOST", "DB_PORT", "PGDATABASE", "PGUSER", "PGPASSWORD", "PGSSLMODE", "R_CONFIG_ACTIVE"),
    unset = NA_character_
  )
  on.exit({
    for (nm in names(old_env)) {
      if (is.na(old_env[[nm]])) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, as.list(stats::setNames(old_env[[nm]], nm)))
      }
    }
  }, add = TRUE)

  Sys.setenv(
    PGHOST = "localhost",
    DB_PORT = "5432",
    PGDATABASE = "astro_test",
    PGUSER = "astro_user",
    PGPASSWORD = "secret",
    PGSSLMODE = "disable",
    R_CONFIG_ACTIVE = "dev"
  )

  captured <- list()

  with_mocked_bindings(
    {
      expect_equal(connect_postgres_db(), "mock_pool")
    },
    postgres_driver = function() "postgres_drv",
    postgres_db_pool = function(...) {
      captured$args <<- list(...)
      "mock_pool"
    },
    .package = "astrocalculation"
  )

  expect_equal(captured$args$validateQuery, "SELECT 1")
  expect_equal(captured$args$validationInterval, 0)
  expect_equal(captured$args$options, "-c search_path=dev")
})

test_that("IP geolocation pool uses the same foreground validation policy", {
  old_env <- Sys.getenv(
    c("PGHOST", "DB_PORT", "PGDATABASE", "PGUSER", "PGPASSWORD", "PGSSLMODE"),
    unset = NA_character_
  )
  on.exit({
    for (nm in names(old_env)) {
      if (is.na(old_env[[nm]])) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, as.list(stats::setNames(old_env[[nm]], nm)))
      }
    }
  }, add = TRUE)

  Sys.setenv(
    PGHOST = "localhost",
    DB_PORT = "5432",
    PGDATABASE = "astro_test",
    PGUSER = "astro_user",
    PGPASSWORD = "secret",
    PGSSLMODE = "disable"
  )

  captured <- list()

  with_mocked_bindings(
    {
      expect_equal(connect_postgres_ipgeo_db(), "mock_pool")
    },
    postgres_driver = function() "postgres_drv",
    postgres_db_pool = function(...) {
      captured$args <<- list(...)
      "mock_pool"
    },
    .package = "astrocalculation"
  )

  expect_equal(captured$args$validateQuery, "SELECT 1")
  expect_equal(captured$args$validationInterval, 0)
  expect_equal(captured$args$options, "-c search_path=ipgeo")
})
