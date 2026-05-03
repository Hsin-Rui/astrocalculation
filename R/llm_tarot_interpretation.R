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
  # --- Fallback builder -------------------------------------------------
  build_fallback <- function() {
    meaning_vec <- if (is.null(card_meanings)) character(0) else as.character(card_meanings)
    meaning_vec <- meaning_vec[!is.na(meaning_vec) & nzchar(trimws(meaning_vec))]
    meaning_text <- if (length(meaning_vec) > 0) {
      paste(meaning_vec, collapse = "; ")
    } else {
      "No interpretation available."
    }
    title_text <- if (!is.null(card_name) && nzchar(trimws(as.character(card_name)))) {
      as.character(card_name)
    } else {
      "Daily Tarot"
    }

    list(
      title         = title_text,
      body          = meaning_text,
      general       = meaning_text,
      work          = meaning_text,
      health        = meaning_text,
      relationships = meaning_text
    )
  }

  # Skip API call when no key is configured
  if (is.null(api_key) || nchar(trimws(api_key)) == 0) {
    return(build_fallback())
  }

  # --- Build structured prompt ------------------------------------------
  system_prompt <- paste0(
    "\u4f60\u662f\u4e00\u4f4d\u5c08\u696d\u63d0\u4f9b\u5854\u7f85\u724c\u89e3\u8b80\u7684\u539f\u578b\u5206\u6790\u5e2b\uff0c\u4f7f\u7528\u7e41\u9ad4\u4e2d\u6587\u56de\u8986\u3002",
    "\u60a8\u7684\u8a9e\u6c23\u5e73\u9759\u3001\u6df1\u601d\u719f\u616e\uff0c\u907f\u514d\u795e\u79d8\u6216\u8d85\u81ea\u7136\u7684\u8a9e\u8a00\u3002",
    "\u50c5\u4ee5JSON\u683c\u5f0f\u56de\u61c9\uff0c\u5305\u542b\u4ee5\u4e0b\u6307\u5b9a\u9375\u540d\uff1a",
    "\"title\"\uff083\u81f37\u500b\u7e41\u9ad4\u4e2d\u6587\u5b57\u7684\u6a19\u984c\uff09\uff0c",
    "\"body\"\uff082\u81f33\u53e5\u7684\u539f\u578b\u89e3\u8b80\uff09\uff0c",
    "\"general\"\uff08\u4e00\u53e5\u8a71\uff1a\u7db2\u5408\u4eba\u751f\u6d1e\u898b\uff09\uff0c",
    "\"work\"\uff08\u4e00\u53e5\u8a71\uff1a\u4e8b\u696d/\u5de5\u4f5c\u6d1e\u898b\uff09\uff0c",
    "\"health\"\uff08\u4e00\u53e5\u8a71\uff1a\u5065\u5eb7/\u6d3b\u529b\u6d1e\u898b\uff09\uff0c",
    "\"relationships\"\uff08\u4e00\u53e5\u8a71\uff1a\u611f\u60c5/\u4eba\u969b\u95dc\u4fc2\u6d1e\u898b\uff09\u3002",
    "\u8acb\u52ff\u5728JSON\u7269\u4ef6\u5916\u5305\u542b\u4efb\u4f55\u6587\u5b57\u3002"
  )

  user_prompt <- paste0(
    "\u6240\u62bd\u5361\u724c\uff1a", card_name, "\n",
    "\u6838\u5fc3\u542b\u7fa9\uff1a", paste(card_meanings, collapse = ", "), "\n",
    "\u8acb\u73fe\u5728\u63d0\u4f9b JSON \u89e3\u8b80\u3002"
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
#' Checks that the provided JSON text is parseable and contains the six
#' required fields: \code{title}, \code{body}, \code{general}, \code{work},
#' \code{health}, and \code{relationships}.
#' Uses \code{jsonlite} which is already a dependency of this package.
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
  # 1. Strip markdown code fences that some models emit
  cleaned <- gsub("^```(?:json)?\\s*|\\s*```$", "", trimws(json_text), perl = TRUE)

  schema <- paste0(
    '{',
    '"type":"object",',
    '"required":["title","body","general","work","health","relationships"],',
    '"properties":{',
    '"title":{"type":"string","minLength":1},',
    '"body":{"type":"string","minLength":1},',
    '"general":{"type":"string","minLength":1},',
    '"work":{"type":"string","minLength":1},',
    '"health":{"type":"string","minLength":1},',
    '"relationships":{"type":"string","minLength":1}',
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
    jsonlite::fromJSON(cleaned, simplifyVector = TRUE),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(list(valid = FALSE, error = "Response is not valid JSON"))
  }

  # Coerce to character to guard against unexpected types
  list(
    valid = TRUE,
    data  = list(
      title         = as.character(parsed$title),
      body          = as.character(parsed$body),
      general       = as.character(parsed$general),
      work          = as.character(parsed$work),
      health        = as.character(parsed$health),
      relationships = as.character(parsed$relationships)
    )
  )
}
