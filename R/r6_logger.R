#' Database Logger Class
#'
#' @description
#' Handles writing logs to the 'app_logs' table.
#' Designed to be robust: if logging fails, it fails silently (prints to console)
#' so it doesn't crash the main app.
#'
#' @import R6
#' @import DBI
#' @import jsonlite
#'
Logger <- R6::R6Class(
  "DbLogger",
  #' @field pool database connection
  public = list(
    pool = NULL,

    #' @description Initialize with a DB pool
    #' @param pool postgres sql connection
    initialize = function(pool) {
      self$pool <- pool
    },

    #' @description Log an INFO event
    #' @param event function call
    #' @param message log message
    #' @param user_id user name. can be NULL (guest)
    #' @param context a json list
    log_info = function(event, message, user_id = NULL, context = list()) {
      self$write_log("INFO", event, message, user_id, context)
    },

    #' @description Log an ERROR event
    #' @param event function call
    #' @param message log message
    #' @param user_id user name. can be NULL (guest)
    #' @param context a json list
    log_error = function(event, message, user_id = NULL, context = list()) {
      self$write_log("ERROR", event, message, user_id, context)
    },

    #' @description Internal writer
    #' @param level "INFO", "ERROR" etc.
    #' @param event function call
    #' @param message log message
    #' @param user_id user name. can be NULL (guest)
    #' @param context a json list
    write_log = function(level, event, message, user_id, context) {
      if (is.null(self$pool)) {
        message(sprintf("[%s] %s: %s", level, event, message))
        return()
      }

      tryCatch({
        # 1. Prepare JSON Context
        context_json <- jsonlite::toJSON(context, auto_unbox = TRUE)

        # 2. SQL Insert (Clean & Separated)
        sql <- "INSERT INTO app_logs (level, event, message, context_json, user_entity_id)
                VALUES (?level, ?evt, ?msg, ?ctx, ?uid)"

        query <- DBI::sqlInterpolate(self$pool, sql,
                                     level = level,
                                     evt = event,  # <--- Clean separate column
                                     msg = message,
                                     ctx = context_json,
                                     uid = if(is.null(user_id)) NA_character_ else user_id)

        DBI::dbExecute(self$pool, query)

      }, error = function(e) {
        # Fallback to console
        message(sprintf("LOGGER FAILED: %s", e$message))
      })
    }
  )
)
