#' Connect to the Database
#'
#' Establishes a connection pool to the PostgreSQL database using environment
#' variables. It automatically handles SSL settings for Azure vs Local.
#'
#' @return A `pool` object
#' @export
connect_postgres_db <- function() {

  # 1. Read Credentials
  db_host <- Sys.getenv("DB_HOST")
  db_port <- Sys.getenv("DB_PORT")
  db_name <- Sys.getenv("DB_NAME")
  db_user <- Sys.getenv("DB_USER")
  db_pass <- Sys.getenv("DB_PASSWORD")
  ssl_mode <- Sys.getenv("DB_SSL_MODE", "require")

  # 2. Check for missing config
  if (db_host == "") {
    stop("DB_HOST is missing. Did you load your .Renviron?")
  }

  # 3. Create Pool
  # We use 'pool' because Shiny apps are multi-threaded/async.
  # A single connection would bottleneck.
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = db_name,
    host = db_host,
    port = db_port,
    user = db_user,
    password = db_pass,
    sslmode = ssl_mode
  )

  return(pool)
}

#' Close Database Connection
#'
#' @param pool The connection pool to close
#' @export
close_postgres_db <- function(pool) {
  if (!is.null(pool) && pool::dbIsValid(pool)) {
    pool::poolClose(pool)
  }
}
