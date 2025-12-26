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
        entity_id, user_entity_id, name, birth_timestamp, timezone, city_name, lat, lng, notes
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
