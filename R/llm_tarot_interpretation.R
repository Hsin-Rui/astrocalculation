#' Get a Tarot card interpretation from a Free LLM API
#'
#' Calls the Groq free-tier API (llama-3.1-8b-instant model) to generate an
#' archetypal interpretation for the drawn tarot card in Traditional Chinese.
#' Falls back to the database card meaning when the API is unavailable or
#' returns invalid JSON.
#'
#' The function is intentionally structured so that the API endpoint, model,
#' and auth header can be swapped to Gemini / Vertex AI for registered /
#' paid users in Story 1.3 without changing callers.
#'
#' @param card_name   Character. Localised name of the drawn card.
#' @param card_meanings Character vector.  Keywords / description from the
#'   tarot DB, used as prompt context and as the hard fallback, so must not
#'   be \code{NULL}.
#' @param api_key Character or \code{NULL}.  Groq API key.  When \code{NULL}
#'   or empty the function returns the structured fallback immediately without
#'   attempting a network call.
#' @param base_url Character.  API base URL.  Defaults to the Groq chat
#'   completions endpoint.  Override to point at Gemini / another provider in
#'   future stories.
#' @param model Character.  Model identifier.  Defaults to
#'   \code{"llama-3.1-8b-instant"}.
#'
#' @return A named list with six character fields (all in Traditional Chinese):
#'   \describe{
#'     \item{title}{Short 3-7 character headline.}
#'     \item{body}{Full interpretation prose (2-3 sentences).}
#'     \item{general}{One-sentence general life insight.}
#'     \item{work}{One-sentence career / work insight.}
#'     \item{health}{One-sentence health / vitality insight.}
#'     \item{relationships}{One-sentence relationship insight.}
#'   }
#'   On fallback the fields are derived from \code{card_meanings}.
#'
#' @importFrom httr2 request req_headers req_body_json req_error req_retry
#'   req_timeout req_perform resp_body_string
#' @importFrom jsonlite fromJSON toJSON
#' @export

get_tarot_interpretation <- function(
    card_name,
    card_meanings,
    api_key  = NULL,
    base_url = "https://api.groq.com/openai/v1/chat/completions",
    model    = "llama-3.1-8b-instant"
) {

  # Skip API call when no key is configured
  if (is.null(api_key) || nchar(trimws(api_key)) == 0) {
    return(build_tarot_fallback(card_name, card_meanings))
  }

  # --- Build structured prompt ------------------------------------------
  prompts       <- tarot_prompts()
  system_prompt <- paste0(prompts$llm_system_prompt, collapse = "")
  user_prompt   <- paste0(
    prompts$llm_user_prompt$card_label,     card_name, "\n",
    prompts$llm_user_prompt$meanings_label, paste(card_meanings, collapse = ", "), "\n",
    prompts$llm_user_prompt$request
  )

  # --- HTTP request with retry ------------------------------------------
  response_text <- tryCatch({
    request_body <- list(
      messages    = list(
        list(role = "system", content = system_prompt),
        list(role = "user",   content = user_prompt)
      ),
      max_tokens  = 512L,
      temperature = 0.7,
      response_format = list(type = "json_object")
    )
    if (!is.null(model) && nchar(trimws(model)) > 0) {
      request_body$model <- model
    }

    req <- httr2::request(base_url) |>
      httr2::req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type"  = "application/json"
      ) |>
      httr2::req_body_json(request_body) |>
      httr2::req_timeout(seconds = 10) |>
      httr2::req_retry(
        max_tries = 3,
        is_transient = function(resp) {
          # Retry on 429 (rate limit) and 5xx server errors
          httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
        }
      ) |>
      httr2::req_error(is_error = function(resp) FALSE) # handle errors manually

    resp <- httr2::req_perform(req)

    if (httr2::resp_status(resp) != 200L) {
      return(build_tarot_fallback(card_name, card_meanings))
    }

    body <- httr2::resp_body_string(resp)
    parsed_resp <- jsonlite::fromJSON(body, simplifyVector = FALSE)
    # Extract content from OpenAI-compatible response shape
    parsed_resp$choices[[1]]$message$content
  }, error = function(e) {
    # Network error or unexpected structure — use fallback
    NULL
  })

  if (is.null(response_text)) return(build_tarot_fallback(card_name, card_meanings))

  # --- Parse and validate JSON from model output ------------------------
  validation <- validate_interpretation(response_text)
  if (!isTRUE(validation$valid)) {
    return(build_tarot_fallback(card_name, card_meanings))
  }

  validation$data
}


#' Validate a tarot interpretation JSON string
#'
#' Checks that the provided JSON text is parseable and contains the six required
#' fields: \code{title}, \code{body}, \code{general}, \code{work},
#' \code{health}, and \code{relationships}. Extra provider/model metadata is
#' ignored, and common wrappers such as \code{interpretation} or \code{data} are
#' unwrapped.
#'
#' @param json_text Character. Raw JSON string returned by the LLM.
#'
#' @return A named list:
#'   \describe{
#'     \item{valid}{Logical. \code{TRUE} if the JSON is valid and complete.}
#'     \item{data}{Named list with \code{title}, \code{body}, \code{general},
#'       \code{work}, \code{health}, \code{relationships}
#'       (only present when \code{valid = TRUE}).}
#'     \item{error}{Character error description (only present when
#'       \code{valid = FALSE}).}
#'   }
#'
#' @importFrom jsonlite fromJSON
#' @export
validate_interpretation <- function(json_text) {
  required_fields <- c("title", "body", "general", "work", "health", "relationships")

  if (is.null(json_text) || length(json_text) == 0) {
    return(list(valid = FALSE, error = "Response is empty"))
  }

  # Strip markdown code fences and tolerate short prose before/after the object.
  cleaned <- gsub("^```(?:json)?\\s*|\\s*```$", "", trimws(as.character(json_text[[1]])), perl = TRUE)
  json_start <- regexpr("\\{", cleaned, perl = TRUE)[[1]]
  json_end_matches <- gregexpr("\\}", cleaned, perl = TRUE)[[1]]
  if (json_start > 0 && !identical(json_end_matches, -1L)) {
    cleaned <- substr(cleaned, json_start, max(json_end_matches))
  }

  parsed <- tryCatch(jsonlite::fromJSON(cleaned, simplifyVector = FALSE), error = function(e) NULL)

  if (is.null(parsed)) {
    return(list(valid = FALSE, error = "Response is not valid JSON"))
  }

  candidate <- parsed
  wrapper_names <- c("interpretation", "tarot_interpretation", "response", "data", "result")
  for (wrapper in wrapper_names) {
    if (is.list(candidate[[wrapper]])) {
      candidate <- candidate[[wrapper]]
      break
    }
  }

  missing_fields <- setdiff(required_fields, names(candidate))
  if (length(missing_fields) > 0) {
    return(list(
      valid = FALSE,
      error = paste("Response is missing required fields:", paste(missing_fields, collapse = ", "))
    ))
  }

  invalid_fields <- required_fields[vapply(required_fields, function(field) {
    value <- candidate[[field]]
    !is.character(value) || length(value) != 1L || !nzchar(trimws(value))
  }, logical(1))]
  if (length(invalid_fields) > 0) {
    return(list(
      valid = FALSE,
      error = paste("Response has invalid required fields:", paste(invalid_fields, collapse = ", "))
    ))
  }

  list(
    valid = TRUE,
    data  = list(
      title         = trimws(candidate$title),
      body          = trimws(candidate$body),
      general       = trimws(candidate$general),
      work          = trimws(candidate$work),
      health        = trimws(candidate$health),
      relationships = trimws(candidate$relationships)
    )
  )
}


#' Internal helper that loads inst/extdata/tarot_prompts.yaml into a list
#' Used by get_tarot_interpretation() and r6_data_manager.R.
tarot_prompts <- function() {
  yaml::read_yaml(
    system.file("extdata", "tarot_prompts.yaml",
                package = "astrocalculation", mustWork = TRUE)
  )
}


#' Internal helper that builds a fallback list
#' @param card_name card name parsed into LLM
#' @param card_meanings keywords related to the card
#'
build_tarot_fallback <- function(card_name, card_meanings){

  meaning_vec  <- if (is.null(card_meanings)) character(0) else as.character(card_meanings)
  meaning_vec  <- meaning_vec[!is.na(meaning_vec) & nzchar(trimws(meaning_vec))]
  meaning_text <- if (length(meaning_vec) > 0) paste(meaning_vec, collapse = "; ") else "No interpretation available."
  title_text   <- if (!is.null(card_name) && nzchar(trimws(as.character(card_name)))) card_name else "Daily Tarot"
  list(
    title            = title_text,
    body             = meaning_text,
    general          = meaning_text,
    work             = meaning_text,
    health           = meaning_text,
    relationships    = meaning_text,
    is_local_fallback = TRUE
  )
}
