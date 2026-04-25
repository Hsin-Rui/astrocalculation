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
    dictionary = NULL
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

      as.character(localized_values[[lang]])
    }
  )
)
