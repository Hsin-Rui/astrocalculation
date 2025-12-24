#' Connect to the Database
#'
#' Establishes a connection pool to the PostgreSQL database using environment
#' variables. It automatically handles SSL settings for Azure vs Local.
#'
#' @return A `pool` object
#'
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
#' @param overwrite Logical. If TRUE, drops existing tables before creating them.
#'   Use with caution!
#'
#' @importFrom yaml read_yaml
#' @importFrom pool poolWithTransaction
#'
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
  message("Reading schema config from: ", schema_path)

  # 3. Execute Transaction
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

#' @title Database Service Functions
#' @description Standalone functions to handle specific DB interactions.
#'
#' @param pool The database connection pool
#' @param user_id The unique Azure Object ID
#'

db_get_profile <- function(pool, user_id) {
  if (is.null(pool) || is.null(user_id)) return(NULL)

  query <- "SELECT * FROM user_profiles WHERE user_entity_id = ?id AND valid_to IS NULL LIMIT 1"
  res <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool, query, id = user_id))

  if (nrow(res) == 0) return(NULL)
  return(res)
}

#'
db_save_profile <- function(pool, user_id, data) {

  # 1. PERFORM LOOKUP
  loc_data <- lookup_city_data(data$country, data$city_name)

  # 2. RESOLVE TIMEZONE (Defensive)
  final_tz <- loc_data$timezone
  if (is.null(final_tz) || is.na(final_tz) || final_tz == "") {
    final_tz <- "UTC"
  }

  # 3. PREPARE TIMESTAMP (The Fix from Reprex)
  birth_ts <- data$birth_timestamp
  if (is.null(birth_ts)) {
    birth_ts <- Sys.time()
  }

  # Apply the timezone strictly to the timestamp object
  # This ensures 'birth_ts' is a valid POSIXct with the correct TZ attribute
  birth_ts <- lubridate::force_tz(birth_ts, tzone = final_tz)

  message(sprintf("Saving Profile for %s in %s (TZ: %s)",
                  user_id, data$city_name, final_tz))

  pool::poolWithTransaction(pool, function(con) {
    # Close Old
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      UPDATE user_profiles SET valid_to = NOW()
      WHERE user_entity_id = ?id AND valid_to IS NULL
    ", id = user_id))

    # Insert New with LOOKUP values
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      INSERT INTO user_profiles (
        user_entity_id, display_name, email,
        birth_timestamp, timezone,
        city_name, country, lat, lng, profile_photo
      ) VALUES (
        ?id, ?name, ?email,
        ?ts, ?tz,
        ?city, ?country, ?lat, ?lng, ?photo
      )
    ",
    id = user_id,
    name = data$display_name,
    email = data$email,
    ts = birth_ts,
    tz = loc_data$timezone,      # <--- FROM LOOKUP
    city = data$city_name,
    country = data$country,
    lat = loc_data$lat,          # <--- FROM LOOKUP
    lng = loc_data$lng,          # <--- FROM LOOKUP
    photo = if (is.null(data$profile_photo)) NA_character_ else data$profile_photo
    ))
  })
}

db_get_library <- function(pool, user_id) {
  if (is.null(pool) || is.null(user_id)) return(data.frame())

  query <- "SELECT * FROM personal_library WHERE owner_oid = ?id AND valid_to IS NULL ORDER BY name ASC"
  DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool, query, id = user_id))
}

db_save_library_entry <- function(pool, user_id, data, entity_id = NULL) {

  # Lookup Location
  loc_data <- lookup_city_data(data$country, data$city_name)

  # Resolve Timezone
  final_tz <- loc_data$timezone
  if (is.null(final_tz) || is.na(final_tz) || final_tz == "") {
    final_tz <- "UTC"
  }

  # Prepare Timestamp
  birth_ts <- data$birth_timestamp
  if (is.null(birth_ts)) birth_ts <- Sys.time()

  # Force Timezone
  birth_ts <- lubridate::force_tz(birth_ts, tzone = final_tz)
  pool::poolWithTransaction(pool, function(con) {
    # Generate or Reuse ID
    final_entity_id <- entity_id
    if (is.null(final_entity_id)) {
      final_entity_id <- DBI::dbGetQuery(con, "SELECT uuid_generate_v4() as id")$id
    } else {
      DBI::dbExecute(con, DBI::sqlInterpolate(con, "
        UPDATE personal_library SET valid_to = NOW()
        WHERE entity_id = ?eid AND valid_to IS NULL
      ", eid = final_entity_id))
    }

    # Insert
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      INSERT INTO personal_library (
        entity_id, owner_oid, name, birth_timestamp, timezone, city_name, lat, lng, notes
      ) VALUES (?eid, ?owner, ?name, ?ts, ?tz, ?city, ?lat, ?lng, ?notes)
    ",
     eid = final_entity_id, owner = user_id, name = data$name,
     ts = birth_ts, tz = final_tz, city = data$city_name,
     lat = loc_data$lat, lng = loc_data$lng, notes = data$notes %||% ""))
  })
}

# Helper for NULL checks
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Mock City Lookup
#' Replace this with your actual SQLite call later.
#' @noRd
lookup_city_data <- function(country, city) {
  # TODO: Connect to astrocalculation internal SQLite DB
  # Handle NULLs or Empty strings safely
  if (is.null(country) || is.null(city) || length(country) == 0 || length(city) == 0) {
    return(list(lat = 0, lng = 0, timezone = "UTC"))
  }

  # Mock Logic - Extend this or connect to your SQLite DB
  if (country == "Taiwan" && city == "Taipei") {
    return(list(
      lat = 25.0330,
      lng = 121.5654,
      timezone = "Asia/Taipei"
    ))
  }

  # Default Fallback
  return(list(lat = 0, lng = 0, timezone = "UTC"))
}
