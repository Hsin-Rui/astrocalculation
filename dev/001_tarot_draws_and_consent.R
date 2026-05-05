#' Migration 001: Add Consent Fields and Tarot Draws Table
#'
#' Adds `terms_accepted_at` to `auth_credentials`,
#' `oracle_voice_preference` to `user_profiles`, and creates the
#' `tarot_draws` table for the Tarot Journal.
#'
#' @param con A live DBI/pool database connection.
#' @return Invisibly TRUE on success; stops with an error on failure.
#'
run_migration_001 <- function(con) {
  # 1. Add terms_accepted_at to auth_credentials
  DBI::dbExecute(
    con,
    "ALTER TABLE auth_credentials
     ADD COLUMN IF NOT EXISTS terms_accepted_at TIMESTAMP WITH TIME ZONE"
  )

  # 2. Add oracle_voice_preference to user_profiles
  DBI::dbExecute(
    con,
    "ALTER TABLE user_profiles
     ADD COLUMN IF NOT EXISTS oracle_voice_preference VARCHAR(50) DEFAULT 'Living Spark'"
  )

  # 3. Create tarot_draws table (idempotent via IF NOT EXISTS)
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS tarot_draws (
      entry_id         UUID PRIMARY KEY,
      user_entity_id   VARCHAR(255) NOT NULL
                         REFERENCES auth_credentials(user_entity_id) ON DELETE CASCADE,
      draw_date        DATE DEFAULT CURRENT_DATE,
      card_id          VARCHAR(255) NOT NULL,
      interpretation_text TEXT,
      is_free_tier     BOOLEAN DEFAULT TRUE,
      interpretation_payload TEXT,
      is_local_fallback BOOLEAN,
      llm_provider     VARCHAR(64),
      llm_model        VARCHAR(255),
      card_file        VARCHAR(255),
      is_reversed      BOOLEAN,
      created_at       TIMESTAMP WITH TIME ZONE DEFAULT NOW()
    )"
  )

  invisible(TRUE)
}
