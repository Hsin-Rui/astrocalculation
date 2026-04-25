#' Extract Client IP from a Shiny Session
#'
#' Prioritises the `X-Forwarded-For` header injected by the Caddy reverse proxy.
#' Falls back to the direct Shiny socket address when the header is absent.
#'
#' @param session A Shiny session object.
#' @return A single IP address string, or `"127.0.0.1"` if none is detectable.
#' @export
get_client_ip <- function(session) {
  forwarded <- session$request$HTTP_X_FORWARDED_FOR

  if (!is.null(forwarded) && nzchar(trimws(forwarded))) {
    # X-Forwarded-For may be comma-separated; take the leftmost (original client)
    ips <- strsplit(forwarded, ",", fixed = TRUE)[[1]]
    return(trimws(ips[[1]]))
  }

  remote <- session$request$REMOTE_ADDR
  if (!is.null(remote) && nzchar(trimws(remote))) {
    return(trimws(remote))
  }

  "127.0.0.1"
}

#' Look Up IP Location via Local MaxMind GeoLite2 Database
#'
#' Resolves a client IP address to a timezone string using the bundled
#' `GeoLite2-City.mmdb` file in `inst/extdata/`. Falls back to
#' `Asia/Taipei` if the database is missing or if the lookup fails.
#'
#' @section MaxMind database setup:
#' Download `GeoLite2-City.mmdb` from
#' \url{https://dev.maxmind.com/geoip/geolite2-free-geolocation-data} and
#' place it at `astrocalculation/inst/extdata/GeoLite2-City.mmdb`.
#' The file is excluded from version control via `.gitignore`.
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
#' @export
#' @importFrom rgeolocate maxmind
#'
get_ip_location <- function(ip, timeout_secs = 2) {
  fallback <- list(
    timezone  = "Asia/Taipei",
    latitude  = NA_real_,
    longitude = NA_real_,
    status    = "fallback"
  )

  # Guard: reject obviously invalid / private / loopback addresses early
  if (is.null(ip) || !nzchar(ip) || .is_private_ip(ip)) {
    return(fallback)
  }

  mmdb_path <- system.file("extdata", "GeoLite2-City.mmdb",
                            package = "astrocalculation")
  if (!nzchar(mmdb_path) || !file.exists(mmdb_path)) {
    warning("GeoLite2-City.mmdb not found in inst/extdata. Falling back to Asia/Taipei.")
    return(fallback)
  }

  result <- tryCatch({
    # setTimeLimit enforces the strict 2-second ceiling (AC 13)
    setTimeLimit(elapsed = timeout_secs, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)

    # The GeoLite2-City MMDB stores timezone under the key "time_zone"
    fields <- c("time_zone", "latitude", "longitude")
    raw <- rgeolocate::maxmind(ip, mmdb_path, fields)

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

#' Detect private / reserved IP ranges (IPv4 only)
#'
#' @param ip Character.
#' @return Logical.
#' @noRd
.is_private_ip <- function(ip) {
  private_patterns <- c(
    "^127\\.",          # loopback
    "^10\\.",           # RFC 1918
    "^172\\.(1[6-9]|2[0-9]|3[01])\\.",  # RFC 1918
    "^192\\.168\\.",    # RFC 1918
    "^::1$",            # IPv6 loopback
    "^fc", "^fd"        # IPv6 unique local
  )
  any(vapply(private_patterns, function(p) grepl(p, ip), logical(1)))
}
