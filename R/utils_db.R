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

#' Initialize Database Schema from YAML
#'
#' Reads inst/extdata/table_defs.yml, translates it to SQL, and executes it.
#'
#' @param overwrite Logical. If TRUE, drops existing tables before creating them.
#'   Use with caution!
#'
#' @importFrom yaml read_yaml
#' @importFrom pool poolWithTransaction
#' @export
db_initialize_schema <- function(overwrite = FALSE) {

  # 1. Connect using your new function name
  pool <- connect_postgres_db()
  on.exit(close_postgres_db(pool))

  # 2. Locate the YAML file
  schema_path <- system.file("extdata", "table_defs.yml", package = "astrocalculations")
  if (schema_path == "") schema_path <- "inst/extdata/table_defs.yml"

  if (!file.exists(schema_path)) {
    schema_path <- sub(".yml$", ".yaml", schema_path)
    if (!file.exists(schema_path)) stop("Could not find table_defs.yml or .yaml")
  }

  defs <- yaml::read_yaml(schema_path)
  message("📖 Reading schema config from: ", schema_path)

  # 3. Execute Transaction
  pool::poolWithTransaction(pool, function(con) {

    # A. OPTIONAL: Drop Tables if overwrite requested
    if (overwrite) {
      message("⚠️ Overwrite enabled: Dropping existing tables...")
      for (table_name in names(defs$tables)) {
        # CASCADE ensures dependent tables (like personal_library) are dropped too
        DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s CASCADE", table_name))
        message(sprintf("🗑️ Dropped table: %s", table_name))
      }
    }

    # B. Enable Extensions
    if (!is.null(defs$config$extensions)) {
      for (ext in defs$config$extensions) {
        DBI::dbExecute(con, sprintf('CREATE EXTENSION IF NOT EXISTS "%s"', ext))
      }
    }

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
      message(sprintf("✅ Table verified: %s", table_name))

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

  message("🚀 Schema migration complete.")
}
