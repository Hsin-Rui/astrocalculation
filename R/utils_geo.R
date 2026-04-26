#' Extract Client IP from a Shiny Session
#'
#' Prioritises the `X-Forwarded-For` header injected by the Caddy reverse proxy.
#' Falls back to the direct Shiny socket address when the header is absent.
#'
#' @param session A Shiny session object.
#' @return A single IP address string, or `"127.0.0.1"` if none is detectable.
#' @export
get_client_ip <- function(session) {
  req <- tryCatch(session$request, error = function(...) NULL)
  if (is.null(req)) {
    return("127.0.0.1")
  }

  forwarded <- req$HTTP_X_FORWARDED_FOR

  if (!is.null(forwarded) && nzchar(trimws(forwarded))) {
    # X-Forwarded-For may be comma-separated; use the first valid token.
    ips <- strsplit(forwarded, ",", fixed = TRUE)[[1]]
    ips <- trimws(ips)
    valid_ips <- ips[nzchar(ips) & vapply(ips, .is_valid_ip, logical(1))]
    if (length(valid_ips) > 0) {
      return(valid_ips[[1]])
    }
  }

  remote <- req$REMOTE_ADDR
  if (!is.null(remote) && nzchar(trimws(remote)) && .is_valid_ip(trimws(remote))) {
    return(trimws(remote))
  }

  "127.0.0.1"
}

#' Look Up IP Location via PostgreSQL GeoLite2 Database
#'
#' Resolves a client IP address to a timezone, latitude, and longitude by
#' querying the `ipgeo_city_blocks` and `ipgeo_city_locations` tables in the
#' dedicated IP-geo PostgreSQL database. Falls back to `Asia/Taipei` if the
#' lookup fails or exceeds the timeout (AC 13).
#'
#' The database connection is provided by `connect_postgres_ipgeo_db()`.
#' Data population and updates are handled by separate data-engineering tasks.
#'
#' @param ip Character. IPv4 or IPv6 address string.
#' @param timeout_secs Numeric. Maximum seconds allowed for the lookup.
#'   Defaults to `2` (strict requirement from AC 13).
#' @return A list with fields:
#'   \describe{
#'     \item{timezone}{Character. IANA timezone string.}
#'     \item{latitude}{Numeric. Approximate latitude, or `NA` on fallback.}
#'     \item{longitude}{Numeric. Approximate longitude, or `NA` on fallback.}
#'     \item{status}{Character. `"ok"` or `"fallback"`.}
#'   }
#' @importFrom DBI dbGetQuery
#' @export
get_ip_location <- function(ip, timeout_secs = 2) {
  fallback <- list(
    timezone  = "Asia/Taipei",
    latitude  = NA_real_,
    longitude = NA_real_,
    status    = "fallback"
  )

  if (is.null(ip) || length(ip) == 0) {
    return(fallback)
  }

  ip <- trimws(as.character(ip[[1]]))

  # Guard: reject obviously invalid / private / loopback addresses early
  if (is.null(ip) || !nzchar(ip) || !.is_valid_ip(ip) || .is_private_ip(ip)) {
    return(fallback)
  }

  result <- tryCatch({
    # setTimeLimit enforces AC 13 with a hard cap at 2 seconds.
    timeout <- suppressWarnings(as.numeric(timeout_secs))
    if (is.na(timeout) || timeout <= 0) {
      timeout <- 2
    }
    timeout <- min(timeout, 2)
    setTimeLimit(elapsed = timeout, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)

    pool <- connect_postgres_ipgeo_db()
    on.exit(pool::poolClose(pool), add = TRUE)

    # CIDR containment query: find the most specific network block for this IP.
    # $1::inet casts the parameter to PostgreSQL inet type safely (no injection risk).
    sql <- paste(
      "SELECT l.time_zone, b.latitude, b.longitude",
      "FROM ipgeo_city_blocks b",
      "JOIN ipgeo_city_locations l ON b.geoname_id = l.geoname_id",
      "WHERE b.network >>= $1::inet",
      "ORDER BY masklen(b.network) DESC",
      "LIMIT 1"
    )
    raw <- DBI::dbGetQuery(pool, sql, params = list(ip))

    if (nrow(raw) == 0) {
      return(fallback)
    }

    tz  <- raw$time_zone[1]
    lat <- raw$latitude[1]
    lng <- raw$longitude[1]

    # Validate the returned value is a real IANA timezone string
    if (is.null(tz) || is.na(tz) || !nzchar(tz) || !(tz %in% OlsonNames())) {
      return(fallback)
    }

    list(
      timezone  = tz,
      latitude  = if (is.na(lat)) NA_real_ else as.numeric(lat),
      longitude = if (is.na(lng)) NA_real_ else as.numeric(lng),
      status    = "ok"
    )
  }, error = function(e) {
    warning(sprintf("IP geolocation failed for '%s': %s", ip, e$message))
    fallback
  })

  return(result)
}

# --------------------------------------------------------------------------- #
# Internal helpers (not exported)                                              #
# --------------------------------------------------------------------------- #

#' Detect private / reserved IP ranges
#'
#' @param ip Character.
#' @return Logical.
#' @noRd
.is_private_ip <- function(ip) {
  if (is.null(ip) || length(ip) == 0) {
    return(FALSE)
  }

  ip <- tolower(trimws(as.character(ip[[1]])))

  private_patterns <- c(
    "^127\\.",          # loopback
    "^10\\.",           # RFC 1918
    "^172\\.(1[6-9]|2[0-9]|3[01])\\.",  # RFC 1918
    "^192\\.168\\.",    # RFC 1918
    "^::1$",            # IPv6 loopback
    "^fc", "^fd",      # IPv6 unique local
    "^fe80:"            # IPv6 link-local
  )
  any(vapply(private_patterns, function(p) grepl(p, ip), logical(1)))
}

#' Detect syntactically valid IP addresses
#'
#' @param ip Character.
#' @return Logical.
#' @noRd
.is_valid_ip <- function(ip) {
  if (is.null(ip) || length(ip) == 0) {
    return(FALSE)
  }

  ip <- trimws(as.character(ip[[1]]))

  ipv4 <- grepl(
    "^((25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})\\.){3}(25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})$",
    ip
  )

  ipv6 <- grepl("^[0-9a-fA-F:]+$", ip) && grepl(":", ip, fixed = TRUE)

  ipv4 || ipv6
}
