#' R6 Class for localized description lookup
#'
#' `DescriptionEngine` loads semantic description keys from YAML into an internal
#' environment to provide fast locale-aware lookups.
#'
#' @import R6
#' @importFrom yaml read_yaml
#' @export
DescriptionEngine <- R6::R6Class(
  "DescriptionEngine",
  private = list(
    dictionary = NULL,
    # Canonical set of planet keys supported by get_summary()
    supported_planet_keys = c("sun", "moon", "asc", "mercury", "venus", "mars", "jupiter", "saturn"),
    # Canonical set of major aspect labels supported by get_aspect_summary()
    supported_aspect_labels = c("conjunction", "square", "trine", "opposition"),

    # Recursively reject non-serializable references before JSON handoff.
    assert_serializable_metadata = function(x, path = "metadata") {
      if (inherits(x, "R6")) {
        stop(sprintf("%s contains a non-serializable R6 object.", path), call. = FALSE)
      }
      if (is.environment(x)) {
        stop(sprintf("%s contains a non-serializable environment.", path), call. = FALSE)
      }
      if (is.function(x)) {
        stop(sprintf("%s contains a non-serializable function.", path), call. = FALSE)
      }
      if (typeof(x) %in% c("externalptr", "weakref", "symbol", "language", "bytecode")) {
        stop(sprintf("%s contains a non-serializable reference.", path), call. = FALSE)
      }

      if (is.list(x)) {
        if (length(x) == 0L) {
          return(invisible(TRUE))
        }

        item_names <- names(x)
        for (i in seq_along(x)) {
          item_label <- if (!is.null(item_names) && nzchar(item_names[[i]])) item_names[[i]] else as.character(i)
          private$assert_serializable_metadata(x[[i]], sprintf("%s$%s", path, item_label))
        }
      }

      invisible(TRUE)
    }
  ),
  public = list(
    #' @description
    #' Initialize the description engine by loading translations from YAML once.
    #' @param yaml_path Optional override for the YAML file path.
    initialize = function(yaml_path = NULL) {
      if (is.null(yaml_path)) {
        yaml_path <- system.file("i18n", "descriptions.yaml", package = "astrocalculation")
      }

      if (!nzchar(yaml_path) || !file.exists(yaml_path) || dir.exists(yaml_path)) {
        stop("Description YAML file not found.")
      }

      parsed <- tryCatch(
        read_yaml(yaml_path),
        error = function(e) {
          stop(sprintf("Failed to parse description YAML: %s", conditionMessage(e)), call. = FALSE)
        }
      )

      parsed_names <- names(parsed)
      if (!is.list(parsed) || length(parsed) == 0L || is.null(parsed_names) || anyNA(parsed_names) || any(!nzchar(parsed_names))) {
        stop("Description YAML must be a named list of keys.")
      }

      private$dictionary <- new.env(parent = emptyenv(), hash = TRUE)
      list2env(parsed, envir = private$dictionary)

    },

    #' @description
    #' Get a localized string from the loaded semantic key dictionary.
    #' @param key Semantic key string.
    #' @param lang Locale code (`zh`, `en`, `de`).
    #' @return A localized string or `NA_character_` when missing.
    localize = function(key, lang) {
      if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
        return(NA_character_)
      }
      if (!is.character(lang) || length(lang) != 1L || is.na(lang) || !nzchar(lang)) {
        return(NA_character_)
      }

      if (!exists(key, envir = private$dictionary, inherits = FALSE)) {
        return(NA_character_)
      }

      localized_values <- private$dictionary[[key]]
      if (!is.list(localized_values) || is.null(localized_values[[lang]])) {
        return(NA_character_)
      }

      localized_value <- localized_values[[lang]]
      if (length(localized_value) != 1L) {
        return(NA_character_)
      }

      as.character(localized_value)
    },

    #' @description
    #' Build a structured, JSON-serializable semantic summary for a planet or chart point.
    #'
    #' Returns a plain R list with no R6 objects or environment references so the
    #' payload is safe to pass across a `future_promise()` process boundary via
    #' `jsonlite::toJSON()`.
    #'
    #' @param planet_key A single character string identifying the planet or chart
    #'   point. Supported values: `"sun"`, `"moon"`, `"asc"`, `"mercury"`,
    #'   `"venus"`, `"mars"`, `"jupiter"`, `"saturn"`.
    #' @param lang Locale code (`"zh"`, `"en"`, `"de"`). Falls back to `"en"` when
    #'   the requested locale is absent for a given key.
    #' @param metadata An optional named list of caller-supplied context to embed
    #'   in the payload (e.g. degree, sign).  Must be a list.
    #' @return A named list:
    #'   \describe{
    #'     \item{`summary_id`}{Stable key string of the form `"summary.<planet_key>"`.}
    #'     \item{`summary`}{Localized sentence string, or `NA_character_` when
    #'       neither the requested locale nor the `"en"` fallback is available.}
    #'     \item{`lang`}{The locale code passed in.}
    #'     \item{`metadata`}{The metadata list passed in (may be empty).}
    #'   }
    get_summary = function(planet_key, lang, metadata = list()) {
      if (!is.character(planet_key) || length(planet_key) != 1L || is.na(planet_key) || !nzchar(planet_key)) {
        stop("planet_key must be a non-empty single character string.", call. = FALSE)
      }
      if (!planet_key %in% private$supported_planet_keys) {
        stop(
          sprintf(
            "Unsupported planet_key: '%s'. Supported keys are: %s.",
            planet_key,
            paste(private$supported_planet_keys, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      if (!is.character(lang) || length(lang) != 1L || is.na(lang) || !nzchar(lang)) {
        stop("lang must be a non-empty single character string.", call. = FALSE)
      }
      if (!is.list(metadata)) {
        stop("metadata must be a list.", call. = FALSE)
      }
      private$assert_serializable_metadata(metadata)

      summary_id <- paste0("summary.", planet_key)

      # Primary locale lookup; deterministic EN fallback when primary is absent.
      summary_text <- self$localize(summary_id, lang)
      if (is.na(summary_text) && !identical(lang, "en")) {
        summary_text <- self$localize(summary_id, "en")
      }

      list(
        summary_id = summary_id,
        summary    = summary_text,
        lang       = lang,
        metadata   = metadata
      )
    },

    #' @description
    #' Build a structured, JSON-serializable semantic summary for a major aspect.
    #'
    #' Only the four major aspects are supported: conjunction, square, trine,
    #' opposition.  Returns a plain R list safe for `jsonlite::toJSON()`.
    #'
    #' @param aspect_label A single character string identifying the aspect.
    #'   Supported values: `"conjunction"`, `"square"`, `"trine"`, `"opposition"`.
    #' @param lang Locale code (`"zh"`, `"en"`, `"de"`). Falls back to `"en"` when
    #'   the requested locale is absent.
    #' @return A named list:
    #'   \describe{
    #'     \item{`aspect_id`}{Stable key string of the form `"aspect.<aspect_label>"`.}
    #'     \item{`summary`}{Localized sentence string, or `NA_character_` on fallback miss.}
    #'     \item{`lang`}{The locale code passed in.}
    #'   }
    get_aspect_summary = function(aspect_label, lang) {
      if (!is.character(aspect_label) || length(aspect_label) != 1L || is.na(aspect_label) || !nzchar(aspect_label)) {
        stop("aspect_label must be a non-empty single character string.", call. = FALSE)
      }
      if (!aspect_label %in% private$supported_aspect_labels) {
        stop(
          sprintf(
            "Unsupported aspect: '%s'. Supported aspects are: %s.",
            aspect_label,
            paste(private$supported_aspect_labels, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      if (!is.character(lang) || length(lang) != 1L || is.na(lang) || !nzchar(lang)) {
        stop("lang must be a non-empty single character string.", call. = FALSE)
      }

      aspect_id <- paste0("aspect.", aspect_label)

      aspect_text <- self$localize(aspect_id, lang)
      if (is.na(aspect_text) && !identical(lang, "en")) {
        aspect_text <- self$localize(aspect_id, "en")
      }

      list(
        aspect_id = aspect_id,
        summary   = aspect_text,
        lang      = lang
      )
    }
  )
)
