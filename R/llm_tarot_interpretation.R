#' Get a Tarot card interpretation from a Free LLM API
#'
#' Calls the Groq free-tier API (llama-3.1-8b-instant model) to generate an
#' archetypal interpretation for the drawn tarot card.  Falls back to the
#' database card meaning when the API is unavailable or returns invalid JSON.
#'
#' The function is intentionally structured so that the API endpoint, model,
#' and auth header can be swapped to Gemini / Vertex AI for registered /
#' paid users in Story 1.3 without changing callers.
#'
#' @param card_name   Character. Localised name of the drawn card
#'   (e.g., \code{"愚者逆位"}).
#' @param card_meanings Character vector.  Keywords / description from the
#'   tarot DB — used as prompt context and as the hard fallback, so must not
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
#' @return A named list with three character fields:
#'   \describe{
#'     \item{title}{Short title for the interpretation (≤ 20 chars).}
#'     \item{body}{Full interpretation prose.}
#'     \item{wisdom_tag}{A single short wisdom phrase.}
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
  # --- Fallback builder -------------------------------------------------
  build_fallback <- function() {
    meaning_vec <- if (is.null(card_meanings)) character(0) else as.character(card_meanings)
    meaning_vec <- meaning_vec[!is.na(meaning_vec) & nzchar(trimws(meaning_vec))]
    meaning_text <- if (length(meaning_vec) > 0) {
      paste(meaning_vec, collapse = "; ")
    } else {
      "No interpretation available."
    }
    wisdom_text <- if (length(meaning_vec) > 0) {
      meaning_vec[[1]]
    } else {
      "Take one grounded step today."
    }
    title_text <- if (!is.null(card_name) && nzchar(trimws(as.character(card_name)))) {
      as.character(card_name)
    } else {
      "Daily Tarot"
    }

    list(
      title      = title_text,
      body       = meaning_text,
      wisdom_tag = wisdom_text
    )
  }

  # Skip API call when no key is configured
  if (is.null(api_key) || nchar(trimws(api_key)) == 0) {
    return(build_fallback())
  }

  # --- Build structured prompt ------------------------------------------
  system_prompt <- paste0(
    "You are an archetypal analyst who provides insightful tarot interpretations. ",
    "Your tone is calm, academic, and avoids mystical or occult language. ",
    "Respond ONLY with a JSON object containing these exact keys: ",
    "\"title\" (a short 3-7 word headline), ",
    "\"body\" (2-3 sentences of archetypal interpretation), ",
    "\"wisdom_tag\" (one short actionable insight). ",
    "Do NOT include any text outside the JSON object."
  )

  user_prompt <- paste0(
    "Card drawn: ", card_name, "\n",
    "Core meanings: ", paste(card_meanings, collapse = ", "), "\n",
    "Provide the JSON interpretation now."
  )

  # --- HTTP request with retry ------------------------------------------
  response_text <- tryCatch({
    req <- httr2::request(base_url) |>
      httr2::req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type"  = "application/json"
      ) |>
      httr2::req_body_json(list(
        model       = model,
        messages    = list(
          list(role = "system", content = system_prompt),
          list(role = "user",   content = user_prompt)
        ),
        max_tokens  = 256L,
        temperature = 0.7
      )) |>
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
      return(build_fallback())
    }

    body <- httr2::resp_body_string(resp)
    parsed_resp <- jsonlite::fromJSON(body, simplifyVector = FALSE)
    # Extract content from OpenAI-compatible response shape
    parsed_resp$choices[[1]]$message$content
  }, error = function(e) {
    # Network error or unexpected structure — use fallback
    NULL
  })

  if (is.null(response_text)) return(build_fallback())

  # --- Parse and validate JSON from model output ------------------------
  validation <- validate_interpretation(response_text)
  if (!isTRUE(validation$valid)) {
    return(build_fallback())
  }

  validation$data
}


#' Validate a tarot interpretation JSON string
#'
#' Checks that the provided JSON text is parseable and contains the three
#' required fields: \code{title}, \code{body}, and \code{wisdom_tag}.
#' Uses \code{jsonlite} which is already a dependency of this package.
#'
#' @param json_text Character. Raw JSON string returned by the LLM.
#'
#' @return A named list:
#'   \describe{
#'     \item{valid}{Logical. \code{TRUE} if the JSON is valid and complete.}
#'     \item{data}{Named list with \code{title}, \code{body}, \code{wisdom_tag}
#'       (only present when \code{valid = TRUE}).}
#'     \item{error}{Character error description (only present when
#'       \code{valid = FALSE}).}
#'   }
#'
#' @importFrom jsonlite fromJSON
#' @export
validate_interpretation <- function(json_text) {
  # 1. Strip markdown code fences that some models emit
  cleaned <- gsub("^```(?:json)?\\s*|\\s*```$", "", trimws(json_text), perl = TRUE)

  schema <- paste0(
    '{',
    '"type":"object",',
    '"required":["title","body","wisdom_tag"],',
    '"properties":{',
    '"title":{"type":"string","minLength":1},',
    '"body":{"type":"string","minLength":1},',
    '"wisdom_tag":{"type":"string","minLength":1}',
    '},',
    '"additionalProperties":false',
    '}'
  )

  schema_ok <- tryCatch(
    jsonvalidate::json_validate(cleaned, schema, error = FALSE),
    error = function(e) FALSE
  )

  if (!isTRUE(schema_ok)) {
    return(list(valid = FALSE, error = "Response does not match required JSON schema"))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(cleaned, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(list(valid = FALSE, error = "Response is not valid JSON"))
  }

  required_fields <- c("title", "body", "wisdom_tag")
  missing_fields <- setdiff(required_fields, names(parsed))

  if (length(missing_fields) > 0) {
    return(list(
      valid = FALSE,
      error = paste("Missing required fields:", paste(missing_fields, collapse = ", "))
    ))
  }

  scalar_character_fields <- vapply(
    required_fields,
    function(field) {
      value <- parsed[[field]]
      is.character(value) && length(value) == 1 && nzchar(trimws(value))
    },
    logical(1)
  )

  if (!all(scalar_character_fields)) {
    return(list(valid = FALSE, error = "Schema fields must be non-empty scalar strings"))
  }

  # Coerce to character to guard against unexpected types
  list(
    valid = TRUE,
    data  = list(
      title      = as.character(parsed$title),
      body       = as.character(parsed$body),
      wisdom_tag = as.character(parsed$wisdom_tag)
    )
  )
}
