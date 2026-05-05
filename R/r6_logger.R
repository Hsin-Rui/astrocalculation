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
#' @importFrom uuid UUIDgenerate
#'
Logger <- R6::R6Class(
  "Logger",
  public = list(
    #' @field pool database connection
    pool = NULL,
    #' @field app_logs_table_available boolean
    app_logs_table_available = NULL,

    #' @description Initialize with a DB pool
    #' @param pool postgres sql connection
    initialize = function(pool) {
      self$pool <- pool
      self$app_logs_table_available <- NULL
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
      if (length(context) == 0) context <- list(sys_time = as.character(Sys.time()))
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
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        message(sprintf("[%s] %s | %s: %s", timestamp, level, event, message))
        return()
      }

      # If log table is unavailable, gracefully fall back to console logging.
      if (is.null(self$app_logs_table_available)) {
        self$app_logs_table_available <- tryCatch(
          DBI::dbExistsTable(self$pool, "app_logs"),
          error = function(e) FALSE
        )
      }
      if (!isTRUE(self$app_logs_table_available)) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        message(sprintf("[%s] %s | %s: %s", timestamp, level, event, message))
        return()
      }

      tryCatch({
        # 1. Prepare JSON Context
        context_json <- jsonlite::toJSON(context, auto_unbox = TRUE)

        # 2. SQL Insert (Clean & Separated)
        sql <- "INSERT INTO app_logs (log_id, level, event, message, context_json, user_entity_id)
                VALUES (?log_id, ?level, ?evt, ?msg, ?ctx, ?uid)"

        query <- DBI::sqlInterpolate(self$pool, sql,
                                     log_id = uuid::UUIDgenerate(),
                                     level = level,
                                     evt = event,  # <--- Clean separate column
                                     msg = message,
                                     ctx = context_json,
                                     uid = if(is.null(user_id)) NA_character_ else user_id)

        DBI::dbExecute(self$pool, query)

      }, error = function(e) {
        # If the table disappears mid-session, stop retrying DB writes.
        if (grepl("relation \"app_logs\" does not exist", e$message, fixed = TRUE)) {
          self$app_logs_table_available <- FALSE
        }
        # Fallback to console
        message(sprintf("LOGGER FAILED: %s", e$message))
      })
    }
  )
)
