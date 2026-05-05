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
#' @param interpretation_payload JSON/string payload returned by the interpretation layer.
#' @param is_local_fallback Logical; TRUE when the local fallback was used instead of an LLM.
#' @param llm_provider Character provider label, e.g. `groq`, `openrouter`, `local_fallback`.
#' @param llm_model Character model identifier.
#' @param card_file Character path/name for the drawn card image.
#' @param is_reversed Logical; whether the card was drawn reversed.
#' @return Invisibly, the number of rows inserted (always 1 on success).
#' @export
#'
save_tarot_draw <- function(pool, user_id, card_id, interpretation_text = NULL,
                            is_free_tier = TRUE,
                            interpretation_payload = NULL,
                            is_local_fallback = NULL,
                            llm_provider = NULL,
                            llm_model = NULL,
                            card_file = NULL,
                            is_reversed = NULL) {
  if (is.null(pool))    stop("Database connection is required to save a tarot draw.")
  if (is.null(user_id) || user_id == "") stop("user_id is required to save a tarot draw.")
  if (is.null(card_id) || card_id == "") stop("card_id is required to save a tarot draw.")

  ensure_tarot_draws_table(pool)
  entry_id <- uuid::UUIDgenerate()

  rows <- DBI::dbExecute(
    pool,
    DBI::sqlInterpolate(
      pool,
      "INSERT INTO tarot_draws
         (entry_id, user_entity_id, card_id, interpretation_text, is_free_tier,
          interpretation_payload, is_local_fallback, llm_provider, llm_model,
          card_file, is_reversed)
       VALUES
         (?entry_id, ?uid, ?card, ?interp, ?free_tier,
          ?payload, ?fallback, ?provider, ?model, ?card_file, ?reversed)",
      entry_id  = entry_id,
      uid       = user_id,
      card      = as.character(card_id),
      interp    = nullable_character(interpretation_text),
      free_tier = isTRUE(is_free_tier),
      payload   = nullable_character(interpretation_payload),
      fallback  = if (is.null(is_local_fallback)) NA else isTRUE(is_local_fallback),
      provider  = nullable_character(llm_provider),
      model     = nullable_character(llm_model),
      card_file = nullable_character(card_file),
      reversed  = if (is.null(is_reversed)) NA else isTRUE(is_reversed)
    )
  )

  invisible(rows)
}

#' Ensure Tarot Draw Audit Table Exists
#'
#' Creates the table and additive audit columns needed to inspect each draw and
#' interpretation response. This is intentionally idempotent for UAT databases
#' that may not have run every migration yet.
#'
#' @param pool A DBI/pool database connection.
#' @return Invisibly TRUE.
ensure_tarot_draws_table <- function(pool) {
  if (is.null(pool)) return(invisible(FALSE))

  DBI::dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS tarot_draws (
      entry_id UUID PRIMARY KEY,
      user_entity_id VARCHAR(255) NOT NULL REFERENCES auth_credentials(user_entity_id) ON DELETE CASCADE,
      draw_date DATE DEFAULT CURRENT_DATE,
      card_id VARCHAR(255) NOT NULL,
      interpretation_text TEXT,
      is_free_tier BOOLEAN DEFAULT TRUE,
      interpretation_payload TEXT,
      is_local_fallback BOOLEAN,
      llm_provider VARCHAR(64),
      llm_model VARCHAR(255),
      card_file VARCHAR(255),
      is_reversed BOOLEAN,
      created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
    )"
  )

  alter_statements <- c(
    "ALTER TABLE tarot_draws ADD COLUMN IF NOT EXISTS interpretation_payload TEXT",
    "ALTER TABLE tarot_draws ADD COLUMN IF NOT EXISTS is_local_fallback BOOLEAN",
    "ALTER TABLE tarot_draws ADD COLUMN IF NOT EXISTS llm_provider VARCHAR(64)",
    "ALTER TABLE tarot_draws ADD COLUMN IF NOT EXISTS llm_model VARCHAR(255)",
    "ALTER TABLE tarot_draws ADD COLUMN IF NOT EXISTS card_file VARCHAR(255)",
    "ALTER TABLE tarot_draws ADD COLUMN IF NOT EXISTS is_reversed BOOLEAN"
  )

  for (statement in alter_statements) {
    DBI::dbExecute(pool, statement)
  }

  invisible(TRUE)
}

#' Convert optional values to nullable character scalars
#'
#' @param value Any value.
#' @return Character scalar or `NA_character_`.
nullable_character <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value[1])) {
    return(NA_character_)
  }

  as.character(value[1])
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
      # Column may not exist in older schemas - degrade gracefully
      warning("record_llm_credit_used: ", e$message)
    }
  )

  invisible(TRUE)
}
