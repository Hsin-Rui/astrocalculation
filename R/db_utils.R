#' @title Database Service Functions
#' @description Standalone functions to handle specific DB interactions.
#'
#' @param pool The database connection pool
#' @param user_id The unique Azure Object ID
#' @importFrom stats setNames
#'

db_get_profile <- function(pool, user_id) {
  if (is.null(pool) || is.null(user_id)) {
    return(NULL)
  }

  query <- "
    SELECT p.*, a.email
    FROM user_profiles p
    LEFT JOIN auth_credentials a ON p.user_entity_id = a.user_entity_id
    WHERE p.user_entity_id = ?id AND p.valid_to IS NULL
    LIMIT 1
  "
  res <- DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool, query, id = user_id))

  if (nrow(res) == 0) {
    return(NULL)
  }
  return(res)
}

#'
db_save_profile <- function(pool, user_id, data) {
  display_name <- require_non_empty(data$display_name, "Display name")
  country <- require_non_empty(data$country, "Country")
  city_name <- require_non_empty(data$city_name, "City")

  # 1. PERFORM LOOKUP
  loc_data <- lookup_city_data(country, city_name)

  # 2. RESOLVE TIMEZONE (Defensive)
  final_tz <- loc_data$timezone
  if (is.null(final_tz) || is.na(final_tz) || final_tz == "") {
    final_tz <- "UTC"
  }

  # 3. PREPARE TIMESTAMP
  birth_ts <- normalize_local_datetime(data$birth_timestamp, final_tz)

  message(sprintf(
    "Saving Profile for %s in %s (TZ: %s)",
    user_id, city_name, final_tz
  ))

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
        entry_id,
        user_entity_id, display_name,
        birth_timestamp, timezone,
        city_name, country, lat, lng, profile_photo,
        valid_from
      ) VALUES (
        ?entry_id,
        ?id, ?name,
        ?ts, ?tz,
        ?city, ?country, ?lat, ?lng, ?photo,
        NOW()
      )
    ",
      entry_id = uuid::UUIDgenerate(),
      id = user_id,
      name = display_name,
      ts = birth_ts,
      tz = final_tz,
      city = city_name,
      country = country,
      lat = loc_data$lat,
      lng = loc_data$lng,
      photo = if (is.null(data$profile_photo)) NA_character_ else data$profile_photo
    ))
  })
}

db_get_library <- function(pool, user_id) {
  if (is.null(pool) || is.null(user_id)) {
    return(data.frame())
  }

  query <- "SELECT * FROM personal_library WHERE user_entity_id = ?id AND valid_to IS NULL ORDER BY name ASC"
  DBI::dbGetQuery(pool, DBI::sqlInterpolate(pool, query, id = user_id))
}

#' Handles "SCD Type 2" updates (if editing) or creates new entries
#'
#' @param pool database connection (postgres pool)
#' @param user_id username
#' @param data list of data to save chart (timestamp, country, city, chart name)
#' @param entity_id uuid of the chart
#'
db_save_library_entry <- function(pool, user_id, data, entity_id = NULL) {
  chart_name <- require_non_empty(data$name, "Chart name")
  country <- require_non_empty(data$country, "Country")
  city_name <- require_non_empty(data$city_name, "City")

  # Lookup Location
  loc_data <- lookup_city_data(country, city_name)

  # Resolve Timezone
  final_tz <- loc_data$timezone
  if (is.null(final_tz) || is.na(final_tz) || final_tz == "") {
    final_tz <- "UTC"
  }

  birth_ts <- normalize_local_datetime(data$birth_timestamp, final_tz)
  pool::poolWithTransaction(pool, function(con) {
    entry_id <- uuid::UUIDgenerate()

    # Generate or Reuse ID
    final_entity_id <- entity_id
    if (is.null(final_entity_id)) {
      final_entity_id <- uuid::UUIDgenerate()
    } else {
      DBI::dbExecute(con, DBI::sqlInterpolate(con, "
        UPDATE personal_library SET valid_to = NOW()
        WHERE entity_id = ?eid AND valid_to IS NULL
      ", eid = final_entity_id))
    }

    # Insert
    DBI::dbExecute(con, DBI::sqlInterpolate(con, "
      INSERT INTO personal_library (
        entry_id,
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
      ) VALUES (?entry_id, ?eid, ?owner, ?name, ?ts, ?tz, ?city, ?country, ?lat, ?lng, ?notes, NOW())
    ",
      entry_id = entry_id,
      eid = final_entity_id,
      owner = user_id,
      name = chart_name,
      ts = birth_ts,
      tz = final_tz,
      city = city_name,
      country = country,
      lat = loc_data$lat,
      lng = loc_data$lng,
      notes = data$notes %||% ""
    ))
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

#' Require a non-empty scalar value
#'
#' @param value Value to validate.
#' @param label Human-readable field name for error messages.
#' @return Trimmed character scalar.
require_non_empty <- function(value, label) {
  if (is.null(value) || length(value) == 0 || is.na(value[1]) ||
      !nzchar(trimws(as.character(value[1])))) {
    stop(paste(label, "is required"), call. = FALSE)
  }

  trimws(as.character(value[1]))
}

#' Interpret a UI datetime as local wall time for a location timezone
#'
#' @param value POSIXt or parseable datetime from Shiny.
#' @param timezone IANA timezone for the selected city.
#' @return POSIXct timestamp whose wall-clock components are in `timezone`.
normalize_local_datetime <- function(value, timezone) {
  if (is.null(value) || length(value) == 0 || is.na(value[1])) {
    stop("Birth timestamp is required", call. = FALSE)
  }

  if (inherits(value, "POSIXt")) {
    return(lubridate::force_tz(value[1], tzone = timezone))
  }

  parsed <- lubridate::as_datetime(as.character(value[1]), tz = timezone)
  if (is.na(parsed)) {
    stop("Birth timestamp is invalid", call. = FALSE)
  }

  parsed
}

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

  return(list(
    lat = res$latitude[1],
    lng = res$longitude[1],
    timezone = res$timezone[1]
  ))
}

#' Get Bilingual Country Options
#' Returns a named vector for selectizeInput: "Name ZH (Name EN)" = "Name EN"
#' @return A named character vector
#'
get_country_options <- function() {
  con <- connect_cities_db()
  on.exit(DBI::dbDisconnect(con))

  # Fetch all countries
  res <- DBI::dbGetQuery(con, "SELECT country_name, name_zh FROM countries ORDER BY country_name")

  # Format Label: "台灣 (Taiwan)" or just "Taiwan" if no translation
  labels <- ifelse(!is.na(res$name_zh) & res$name_zh != "",
    paste0(res$name_zh, " (", res$country_name, ")"),
    res$country_name
  )

  # Value: "Taiwan" (English name required by backend)
  return(setNames(res$country_name, labels))
}

#' Get Bilingual City Options for a Country
#' Returns a named vector for selectizeInput
#' @param country_name The English name of the country
#' @return A named character vector
#'
get_city_options <- function(country_name) {
  con <- connect_cities_db()
  on.exit(DBI::dbDisconnect(con))

  # Join to filter by Country Name
  sql <- "
    SELECT ci.name, ci.name_zh
    FROM cities ci
    JOIN countries co ON ci.country_code = co.country_code
    WHERE co.country_name = ?name
    ORDER BY ci.name
  "
  query <- DBI::sqlInterpolate(con, sql, name = country_name)
  res <- DBI::dbGetQuery(con, query)

  if (nrow(res) == 0) {
    return(character(0))
  }

  # Format Label: "台北市 (Taipei)"
  labels <- ifelse(!is.na(res$name_zh) & res$name_zh != "",
    paste0(res$name_zh, " (", res$name, ")"),
    res$name
  )

  return(setNames(res$name, labels))
}
