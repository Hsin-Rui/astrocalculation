#' Get a connection to the cities SQLite database
#'
#' This function locates the 'cities.sqlite' database included with the package
#' and returns a DBI connection to it.
#'
#' @return A DBI database connection object.
#' @export
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect
#' @examples
#' \dontrun{
#' con <- get_cities_connection()
#' DBI::dbListTables(con)
#' DBI::dbDisconnect(con)
#' }
connect_cities_db <- function() {
  # Locate the database file within the installed package
  path <- system.file("extdata", "cities.sqlite", package = "astrocalculation", mustWork = TRUE)

  # Establish and return the connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

  return(con)
}

#' Connect to the Database
#'
#' Establishes a connection pool to the PostgreSQL database using environment
#' variables. It automatically handles SSL settings for Azure vs Local.
#'
#' @return A `pool` object
#'
connect_postgres_db <- function() {

  # 1. Read Credentials
  db_host <- Sys.getenv("PGHOST")
  db_port <- Sys.getenv("DB_PORT")
  db_name <- Sys.getenv("PGDATABASE")
  #db_user <- Sys.getenv("DB_USER")
  db_user <- Sys.getenv("PGUSER")
  #db_pass <- Sys.getenv("DB_PASSWORD")
  db_pass <- Sys.getenv("PGPASSWORD")
  # ssl_mode <- Sys.getenv("DB_SSL_MODE", "require")
  ssl_mode <- Sys.getenv("PGSSLMODE", "require")

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
    sslmode = ssl_mode,
    options = paste0("-c search_path=", Sys.getenv("R_CONFIG_ACTIVE"))
  )

  return(pool)
}

#' Close Database Connection
#'
#' @param pool The connection pool to close
#'
close_postgres_db <- function(pool) {
  if (!is.null(pool) && pool::dbIsValid(pool)) {
    pool::poolClose(pool)
  }
}

#' Initialize Database Schema from YAML
#'
#' Reads inst/extdata/table_defs.yml, translates it to SQL, and executes it.
#'
#' @param pool database connection (connect_postgres_db)
#' @param overwrite Logical. If TRUE, drops existing tables before creating them.
#'   Use with caution!
#'
#' @importFrom yaml read_yaml
#' @importFrom pool poolWithTransaction
#'
db_initialize_schema <- function(pool = NULL, overwrite = FALSE) {

  # 1. Connect using your new function name
  if (is.null(pool)) pool <- connect_postgres_db()
  on.exit(close_postgres_db(pool))

  # 2. Locate the YAML file
  schema_path <- system.file("extdata", "table_defs.yml", package = "astrocalculation")
  if (schema_path == "") schema_path <- "inst/extdata/table_defs.yml"

  if (!file.exists(schema_path)) {
    schema_path <- sub(".yml$", ".yaml", schema_path)
    if (!file.exists(schema_path)) stop("Could not find table_defs.yml or .yaml")
  }

  defs <- yaml::read_yaml(schema_path)
  message("Reading schema config from: ", schema_path)

  # 3. Execute Transaction
  # B. Enable Extensions (outside the transaction — DDL in some managed PG
  #    environments requires superuser and cannot run inside a transaction block)
  if (!is.null(defs$config$extensions)) {
    for (ext in defs$config$extensions) {
      tryCatch(
        DBI::dbExecute(pool, sprintf('CREATE EXTENSION IF NOT EXISTS "%s"', ext)),
        error = function(e) message(sprintf(
          "Note: Could not install extension '%s': %s (may require superuser)", ext, e$message
        ))
      )
    }
  }

  pool::poolWithTransaction(pool, function(con) {

    # A. OPTIONAL: Drop Tables if overwrite requested
    if (overwrite) {
      message("Overwrite enabled: Dropping existing tables...")
      for (table_name in names(defs$tables)) {
        # CASCADE ensures dependent tables (like personal_library) are dropped too
        DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s CASCADE", table_name))
        message(sprintf("Dropped table: %s", table_name))
      }
    }

    # B. Enable Extensions
    # (moved outside the transaction — handled before poolWithTransaction)

    # C. Create Tables
    for (table_name in names(defs$tables)) {
      tbl <- defs$tables[[table_name]]

      # Build Column Definitions
      col_defs <- vapply(names(tbl$columns), function(col_name) {
        col_info <- tbl$columns[[col_name]]

        # Handle both object (new) and string (legacy) formats
        sql_type <- if (is.list(col_info)) col_info$type else col_info

        sprintf("%s %s", col_name, sql_type)
      }, character(1))

      # Build Foreign Keys
      fk_defs <- c()
      if (!is.null(tbl$foreign_keys)) {
        for (fk in tbl$foreign_keys) {
          fk_str <- sprintf(
            "CONSTRAINT fk_%s_%s FOREIGN KEY (%s) REFERENCES %s ON DELETE %s",
            table_name, fk$column, fk$column, fk$references, fk$on_delete
          )
          fk_defs <- c(fk_defs, fk_str)
        }
      }

      # Combine into CREATE TABLE statement
      all_defs <- paste(c(col_defs, fk_defs), collapse = ",\n  ")
      query <- sprintf("CREATE TABLE IF NOT EXISTS %s (\n  %s\n);", table_name, all_defs)

      DBI::dbExecute(con, query)
      message(sprintf("able verified: %s", table_name))

      # D. Create Indices
      if (!is.null(tbl$indices)) {
        for (idx_col in tbl$indices) {
          idx_name <- sprintf("idx_%s_%s", table_name, idx_col)
          DBI::dbExecute(con, sprintf(
            "CREATE INDEX IF NOT EXISTS %s ON %s(%s)", idx_name, table_name, idx_col
          ))
        }
      }
    }
  })

  message("Schema migration complete.")
}

#' Get a connection to the tarot SQLite database
#'
#' This function locates the 'tarot.sqlite' database included with the package
#' and returns a DBI connection to it.
#'
#' @return A DBI database connection object.
#' @export
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect
#' @examples
#' \dontrun{
#' con <- get_tarot_connection()
#' DBI::dbListTables(con)
#' DBI::dbGetQuery(con, "select * from tarot_cards")
#' DBI::dbDisconnect(con)
#' }
connect_tarot_db <- function() {
  # Locate the database file within the installed package
  path <- system.file("extdata", "tarot.sqlite", package = "astrocalculation", mustWork = TRUE)

  # Establish and return the connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

  return(con)
}
