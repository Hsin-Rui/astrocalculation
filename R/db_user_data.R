#' @title Tarot Draw Data Service Functions
#' @description Functions for persisting and retrieving tarot draw Journal entries.
#'
#' @importFrom DBI dbExecute sqlInterpolate
#'

#' Save a Tarot Draw to the Journal
#'
#' Inserts a new row into `tarot_draws`, linking the draw to the authenticated
#' user and recording the LLM (or static) interpretation.  Uses a parameterized
#' query to prevent SQL injection (project standard).
#'
#' @param pool  A DBI/pool database connection.
#' @param user_id  The `user_entity_id` of the authenticated user.
#' @param card_id  Character identifier of the drawn card (e.g. card name).
#' @param interpretation_text  Full interpretation text to persist (may be NULL).
#' @param is_free_tier  Logical; TRUE = LLM credit consumed from the free 1/day quota.
#' @return Invisibly, the number of rows inserted (always 1 on success).
#' @export
#'
save_tarot_draw <- function(pool, user_id, card_id, interpretation_text = NULL,
                            is_free_tier = TRUE) {
  if (is.null(pool))    stop("Database connection is required to save a tarot draw.")
  if (is.null(user_id) || user_id == "") stop("user_id is required to save a tarot draw.")
  if (is.null(card_id) || card_id == "") stop("card_id is required to save a tarot draw.")

  rows <- DBI::dbExecute(
    pool,
    DBI::sqlInterpolate(
      pool,
      "INSERT INTO tarot_draws
         (user_entity_id, card_id, interpretation_text, is_free_tier)
       VALUES
         (?uid, ?card, ?interp, ?free_tier)",
      uid       = user_id,
      card      = card_id,
      interp    = if (is.null(interpretation_text)) NA_character_ else interpretation_text,
      free_tier = isTRUE(is_free_tier)
    )
  )

  invisible(rows)
}

#' Record LLM Credit Consumption
#'
#' Updates `auth_credentials` to record that the user consumed their
#' 1/day LLM credit by storing the current UTC timestamp in `llm_credit_used_at`.
#' If the column does not yet exist (pre-migration environment), this is a no-op
#' with a warning so it never brings down the app.
#'
#' NOTE: The `llm_credit_used_at` column is expected to be added via a future
#' migration if per-day credit tracking becomes a separate DB field.  For Story 1.2
#' we use the `is_free_tier` flag on the `tarot_draws` row as the source of truth.
#'
#' @param pool   A DBI/pool database connection.
#' @param user_id  The `user_entity_id` of the authenticated user.
#' @return Invisibly TRUE.
#' @export
#'
record_llm_credit_used <- function(pool, user_id) {
  if (is.null(pool) || is.null(user_id)) return(invisible(FALSE))

  tryCatch(
    {
      DBI::dbExecute(
        pool,
        DBI::sqlInterpolate(
          pool,
          "UPDATE auth_credentials
             SET llm_credit_used_at = NOW()
           WHERE user_entity_id = ?uid",
          uid = user_id
        )
      )
    },
    error = function(e) {
      # Column may not exist in older schemas — degrade gracefully
      warning("record_llm_credit_used: ", e$message)
    }
  )

  invisible(TRUE)
}
