#' Read and trim an environment variable
#'
#' @param name Character. Environment variable name.
#' @return Character scalar or `NULL` when unset/blank.
get_clean_env <- function(name) {
  value <- Sys.getenv(name, unset = "")
  value <- trimws(gsub("^[\"']+|[\"']+$", "", value))

  if (!nzchar(value)) {
    return(NULL)
  }

  value
}

#' Resolve tarot LLM provider configuration from environment
#'
#' @return List with `api_key`, `base_url`, and `model`.
resolve_tarot_llm_config <- function() {
  groq_key <- get_clean_env("GROQ_API_KEY")
  if (!is.null(groq_key)) {
    return(list(
      provider = "groq",
      api_key = groq_key,
      base_url = get_clean_env("GROQ_BASE_URL") %||%
        "https://api.groq.com/openai/v1/chat/completions",
      model = get_clean_env("GROQ_MODEL") %||% "llama-3.1-8b-instant"
    ))
  }

  openrouter_key <- get_clean_env("OPENROUTER_API_KEY")
  if (!is.null(openrouter_key)) {
    return(list(
      provider = "openrouter",
      api_key = openrouter_key,
      base_url = get_clean_env("OPENROUTER_BASE_URL") %||%
        "https://openrouter.ai/api/v1/chat/completions",
      model = get_clean_env("OPENROUTER_MODEL") %||% "openai/gpt-4.1-mini"
    ))
  }

  list(
    provider = "local_fallback",
    api_key = NULL,
    base_url = "https://api.groq.com/openai/v1/chat/completions",
    model = "llama-3.1-8b-instant"
  )
}
