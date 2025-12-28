#' @title Database Service Functions
#' @description Standalone functions to handle specific DB interactions.
#'
#' @param pool The database connection pool
#' @param user_id The unique Azure Object ID
#'

db_get_profile <- function(pool, user_id) {
  if (is.null(pool) || is.null(user_id)) return(NULL)

  query <- "
    SELECT p.*, a.email
    FROM user_profiles p
    LEFT JOIN auth_credentials a ON p.user_entity_id = a.user_entity_id
    WHERE p.user_entity_id = ?id AND p.valid_to IS NULL
    LIMIT 1
  "
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

    # Insert New Profile (WITHOUT EMAIL)
    # Note: We do NOT update email here. Email is immutable in auth_credentials.
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      INSERT INTO user_profiles (
        user_entity_id, display_name,
        birth_timestamp, timezone,
        city_name, country, lat, lng, profile_photo,
        valid_from
      ) VALUES (
        ?id, ?name,
        ?ts, ?tz,
        ?city, ?country, ?lat, ?lng, ?photo,
        NOW()
      )
    ",
     id = user_id,
     name = data$display_name,
     ts = birth_ts,
     tz = final_tz,
     city = data$city_name,
     country = data$country,
     lat = loc_data$lat,
     lng = loc_data$lng,
     photo = if (is.null(data$profile_photo)) NA_character_ else data$profile_photo
    ))
  })
}

db_get_library <- function(pool, user_id) {
  if (is.null(pool) || is.null(user_id)) return(data.frame())

  query <- "SELECT * FROM personal_library WHERE user_entity_id = ?id AND valid_to IS NULL ORDER BY name ASC"
  DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool, query, id = user_id))
}

#' Handles "SCD Type 2" updates (if editing) or creates new entries.
#'
#' @param pool database connection (postgres pool)
#' @param user_id username
#' @param data list of data to save chart (timestamp, country, city, chart name)
#' @param entity_id uuid of the chart
#'
db_save_library_entry <- function(pool, user_id, data, entity_id = NULL) {

  if (is.null(data$birth_timestamp)) stop("Birth timestamp is required")
  if (is.null(data$country)) stop("Country is required")
  if (is.null(data$city_name)) stop("City is required")
  if (is.null(data$name)) stop("Chart name is required")

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
        entity_id,
        user_entity_id,
        name,
        birth_timestamp,
        timezone,
        city_name,
        country,
        lat,
        lng,
        notes,
        valid_from
      ) VALUES (?eid, ?owner, ?name, ?ts, ?tz, ?city, ?country, ?lat, ?lng, ?notes, NOW())
    ",
     eid = final_entity_id,
     owner = user_id,
     name = data$name,
     ts = birth_ts,
     tz = final_tz,
     city = data$city_name,
     country = data$country,
     lat = loc_data$lat,
     lng = loc_data$lng,
     notes = data$notes %||% ""))
  })
}

db_delete_library_entry <- function(pool, entry_id) {
  # Soft delete by setting valid_to
  DBI::dbExecute(pool, DBI::sqlInterpolate(pool, "
        UPDATE personal_library SET valid_to = NOW()
        WHERE entity_id = ?eid AND valid_to IS NULL
   ", eid = entry_id))
}

# Helper for NULL checks
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Lookup City Coordinates and Timezone
#'
#' Fetches latitude, longitude, and timezone string from the local SQLite database.
#' Joins cities, countries, and timezones tables.
#'
#' @param country Character string. Full country name (e.g., "Taiwan")
#' @param city Character string. City name (e.g., "Taipei")
#' @return A list containing lat, lng, and timezone.
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect sqlInterpolate
#'
lookup_city_data <- function(country, city) {

  # 1. Connect to internal SQLite DB
  con <- connect_cities_db()
  on.exit(DBI::dbDisconnect(con))

  # 2. Prepare SQL with JOINs
  # cities table links to timezones via timezone_id
  # cities table links to countries via country_code
  sql <- "
    SELECT
      ci.latitude,
      ci.longitude,
      tz.timezone
    FROM cities ci
    JOIN countries co ON ci.country_code = co.country_code
    LEFT JOIN timezones tz ON ci.timezone_id = tz.timezone_id
    WHERE ci.name = ?city
      AND co.country_name = ?country
      LIMIT 1
  "

  # 3. Execute Query
  query <- DBI::sqlInterpolate(con, sql, city = city, country = country)
  res <- DBI::dbGetQuery(con, query)

  # 4. Handle Results & Fallback
  if (nrow(res) == 0) {
    warning(sprintf("Location lookup failed for %s, %s. Defaulting to UTC/0,0.", city, country))
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

  return(list(
    lat = res$latitude[1],
    lng = res$longitude[1],
    timezone = res$timezone[1]
  ))
}
