#' Migration 002: Tarot Draw Audit Columns
#'
#' Ensures `tarot_draws` exists without requiring PostgreSQL UUID generator
#' extensions, then adds audit columns for inspecting card draws and LLM output.
#'
#' @param con A live DBI/pool database connection.
#' @return Invisibly TRUE on success; stops with an error on failure.
run_migration_002 <- function(con) {
  DBI::dbExecute(
    con,
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
    DBI::dbExecute(con, statement)
  }

  invisible(TRUE)
}
