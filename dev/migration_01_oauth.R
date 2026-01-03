library(DBI)
library(pool)

# Source the db_connection file to get access to connect_postgres_db
source("R/db_connection.R")

message("Connecting to database...")
con <- connect_postgres_db()

# Helper function to add a column if it doesn't exist
add_column_if_missing <- function(conn_pool, table, col_name, col_type) {
  # We need to get a connection from the pool to use dbListFields
  conn <- pool::poolCheckout(conn_pool)
  tryCatch({
    fields <- DBI::dbListFields(conn, table)
    if (!col_name %in% fields) {
      message(sprintf("Adding '%s' column to '%s' table...", col_name, table))
      # Use dbExecute on the connection for DDL
      DBI::dbExecute(conn, sprintf("ALTER TABLE %s ADD COLUMN %s %s", table, col_name, col_type))
    } else {
      message(sprintf("Column '%s' already exists in '%s'.", col_name, table))
    }
  }, finally = {
    pool::poolReturn(conn)
  })
}

# Add OAuth columns to the auth_credentials table
message("Running OAuth schema migration...")
add_column_if_missing(con, "auth_credentials", "oauth_provider", "VARCHAR(50)")
add_column_if_missing(con, "auth_credentials", "oauth_subject_id", "VARCHAR(255)")

message("Schema migration complete.")
pool::poolClose(con)
