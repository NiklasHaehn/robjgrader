#' Read a student text submission
#'
#' Reads a plain-text student answer from a \code{.txt}, \code{.md}, or
#' \code{.pdf} file and returns it as a single character string.
#'
#' @param path Path to the submission file.
#' @return A single character string containing the file contents.
#' @export
read_student_text <- function(path) {
  if (!file.exists(path))
    stop(sprintf("File not found: '%s'", path))

  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("txt", "md", "rmd", "qmd")) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  } else if (ext == "pdf") {
    if (!requireNamespace("pdftools", quietly = TRUE))
      stop("Package 'pdftools' is required to read PDF files. ",
           "Install with: install.packages('pdftools')")
    paste(pdftools::pdf_text(path), collapse = "\n")
  } else {
    stop(sprintf(
      "Unsupported file extension '.%s'. Supported formats: .txt, .md, .pdf.",
      ext
    ))
  }
}


#' Find and read a student text submission automatically
#'
#' Scans the current working directory for text files (\code{.txt},
#' \code{.md}, \code{.pdf}), excludes the calling autograder script, and
#' returns the content of the first matching file as a character string.
#' Mirrors the auto-discovery logic of \code{source_student_file()}.
#'
#' @param pattern       Optional regex to filter filenames (e.g.
#'   \code{"interpretation"}). When \code{NULL}, all supported text formats
#'   are considered.
#' @param autograder_name Character or \code{NULL}. Additional filename(s) to
#'   exclude beyond the auto-detected calling script.
#'
#' @return A single character string with the file contents.
#' @export
find_student_text <- function(pattern = NULL, autograder_name = NULL) {
  calling <- .calling_file()
  exclude  <- unique(c(
    if (!is.null(calling))         basename(calling),
    if (!is.null(autograder_name)) autograder_name
  ))

  file_pattern <- if (!is.null(pattern)) {
    pattern
  } else {
    "\\.(txt|md|rmd|qmd|pdf)$"
  }

  candidates <- list.files(pattern = file_pattern, full.names = TRUE,
                            ignore.case = TRUE)
  candidates <- candidates[!basename(candidates) %in% exclude]

  if (length(candidates) == 0L)
    stop("No student text file found in the current directory.")
  if (length(candidates) > 1L)
    warning(sprintf(
      "%d text files found; using the first: %s",
      length(candidates), basename(candidates[[1L]])
    ), call. = FALSE)

  path <- candidates[[1L]]
  message(sprintf("Reading student text: %s", basename(path)))
  read_student_text(path)
}


#' Validate a student text answer using an LLM
#'
#' Sends a student's written answer to an OpenAI-compatible LLM endpoint for
#' automated grading.  Supports two modes:
#'
#' \describe{
#'   \item{Mode A -- full prompt}{Pass a complete \code{prompt} that you have
#'     written yourself.  The prompt must instruct the model to return JSON
#'     matching the schema described below.}
#'   \item{Mode B -- structured prompt}{Pass \code{question} and \code{rubric};
#'     a complete grading prompt is built automatically.}
#' }
#'
#' The LLM is expected to return a JSON object with the following fields:
#' \preformatted{
#' {
#'   "pass":     <boolean>,
#'   "score":    <number 0-1>,
#'   "criteria": [
#'     { "name": "...", "pass": <boolean>, "message": "..." }
#'   ],
#'   "feedback": "..."   // only when feedback = TRUE
#' }
#' }
#'
#' If the response does not conform to this schema, the LLM is asked to retry
#' up to \code{max_retry} times before returning a result with
#' \code{overall = NA}.
#'
#' @param text      Character. The student's answer, typically from
#'   \code{read_student_text()}.
#' @param prompt    Character. Full system prompt (Mode A). Mutually exclusive
#'   with \code{question} and \code{rubric}.
#' @param question  Character. The assignment question (Mode B).
#' @param rubric    Named character vector or named list. Each element is a
#'   grading criterion; its name is the criterion label and its value is the
#'   description of what a passing answer must demonstrate.
#' @param reference Character or file path. An optional model answer used as a
#'   grading standard.  If a valid file path is supplied, the file is read via
#'   \code{read_student_text()}.  When provided, the LLM compares the student
#'   answer against the reference rather than grading against abstract criteria
#'   alone.  Only used in Mode B (\code{question}/\code{rubric}); ignored in
#'   Mode A.
#' @param name      Character. Label for this result, shown in console output
#'   and Gradescope.
#' @param feedback  Logical. If \code{TRUE}, the LLM is asked to provide
#'   written feedback (max 3 sentences / 200 words) stored in
#'   \code{result$feedback}.
#' @param model     Character. Model identifier passed to the API.
#' @param base_url  Character. Base URL of the OpenAI-compatible API endpoint.
#'   Defaults to Groq (\code{"https://api.groq.com/openai/v1"}).
#' @param api_key   Character. API key. Defaults to the \code{GROQ_API_KEY}
#'   environment variable.
#' @param max_retry Integer. Maximum regrading attempts on invalid responses.
#'   Default \code{3L}.
#'
#' @return A \code{robjgrader_result} with fields \code{score} (normalized
#'   0--1, used for partial credit in \code{run_autograder()}) and, when
#'   \code{feedback = TRUE}, \code{feedback} (character or \code{NULL}).
#' @export
validate_text <- function(
  text,
  prompt    = NULL,
  question  = NULL,
  rubric    = NULL,
  reference = NULL,
  name      = "text",
  feedback  = FALSE,
  model     = "llama-3.3-70b-versatile",
  base_url  = "https://api.groq.com/openai/v1",
  api_key   = Sys.getenv("GROQ_API_KEY"),
  max_retry = 3L
) {
  if (is.null(prompt) && (is.null(question) || is.null(rubric)))
    stop("Provide either 'prompt' (Mode A) or both 'question' and 'rubric' (Mode B).")
  if (!is.null(prompt) && (!is.null(question) || !is.null(rubric)))
    stop("'prompt' and 'question'/'rubric' are mutually exclusive.")
  if (nchar(api_key) == 0L)
    stop("No API key found. Set GROQ_API_KEY or pass 'api_key' explicitly.")

  ref_text <- if (!is.null(reference)) {
    if (is.character(reference) && length(reference) == 1L &&
        file.exists(reference)) {
      read_student_text(reference)
    } else {
      as.character(reference)
    }
  } else {
    NULL
  }

  sys_prompt <- if (!is.null(prompt)) prompt else
    .build_text_prompt(question, rubric, feedback, ref_text)

  messages <- list(
    list(role = "system", content = sys_prompt),
    list(role = "user",   content = text)
  )

  parsed     <- NULL
  error_note <- NULL

  for (attempt in seq_len(max_retry)) {
    msgs <- if (!is.null(error_note)) {
      c(messages,
        list(
          list(role = "assistant", content = error_note$raw),
          list(role = "user",
               content = paste0(
                 "Your previous response was invalid: ", error_note$reason,
                 "\nPlease return only valid JSON matching the required schema."
               ))
        ))
    } else {
      messages
    }

    raw <- tryCatch(
      .call_llm(msgs, model, base_url, api_key),
      error = function(e) stop(sprintf("LLM API call failed: %s", conditionMessage(e)))
    )

    result <- .parse_llm_response(raw)
    if (isTRUE(result$valid)) {
      parsed <- result$data
      break
    }
    error_note <- list(raw = raw, reason = result$reason)
  }

  if (is.null(parsed)) {
    return(.text_error_result(name, sprintf(
      "LLM returned an invalid response after %d attempt(s): %s",
      max_retry, error_note$reason
    )))
  }

  checks <- lapply(parsed$criteria, function(cr) {
    list(name    = cr$name %||% "criterion",
         pass    = isTRUE(cr$pass),
         message = cr$message %||% "")
  })

  overall <- isTRUE(parsed$pass)
  score   <- if (!is.null(parsed$score)) {
    max(0, min(1, as.numeric(parsed$score)))
  } else {
    if (overall) 1 else 0
  }

  fb <- if (isTRUE(feedback) && !is.null(parsed$feedback)) parsed$feedback else NULL

  structure(
    list(
      object_name  = name,
      object_type  = "text",
      object_class = "character",
      overall      = overall,
      score        = score,
      checks       = checks,
      feedback     = fb
    ),
    class = "robjgrader_result"
  )
}


# -- Internal helpers ----------------------------------------------------------

.build_text_prompt <- function(question, rubric, feedback, reference = NULL) {
  criteria_block <- paste(
    mapply(
      function(nm, desc) sprintf("- %s: %s", nm, desc),
      names(rubric), as.character(rubric)
    ),
    collapse = "\n"
  )

  reference_block <- if (!is.null(reference) && nchar(trimws(reference)) > 0L) {
    sprintf(
      "\nREFERENCE ANSWER (use this as the standard for comparison):\n%s\n",
      reference
    )
  } else {
    ""
  }

  feedback_field <- if (isTRUE(feedback)) {
    paste0(
      "\n  \"feedback\": \"<written feedback for the student.",
      " Rules: constructive and precise; plain English; max 3 sentences and 200 words;",
      " no greetings, sign-offs, or filler phrases; do not mention AI, language models,",
      " or automated grading; start directly with the substantive comment>\","
    )
  } else {
    ""
  }

  sprintf(
    paste0(
      "You are a grader for a political science methods course.\n\n",
      "QUESTION:\n%s\n\n",
      "GRADING CRITERIA (each must be assessed independently):\n%s\n",
      "%s\n",
      "Evaluate the student's answer against each criterion. ",
      "Return ONLY a JSON object with this exact structure -- no prose, ",
      "no markdown code fences:\n",
      "{\n",
      "  \"pass\": <true if the overall answer is satisfactory, false otherwise>,\n",
      "  \"score\": <number between 0 and 1 reflecting overall quality>,\n",
      "  \"criteria\": [\n",
      "    { \"name\": \"<criterion name>\", \"pass\": <true/false>,",
      " \"message\": \"<brief reason, 1 sentence>\" }\n",
      "  ],%s\n",
      "}\n\n",
      "Return ONLY the JSON object."
    ),
    question, criteria_block, reference_block, feedback_field
  )
}


.call_llm <- function(messages, model, base_url, api_key) {
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")

  resp <- httr2::request(paste0(base_url, "/chat/completions")) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(list(
      model       = model,
      messages    = messages,
      temperature = 0.1,
      max_tokens  = 600L
    )) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status != 200L) {
    body <- tryCatch(httr2::resp_body_json(resp), error = function(e) list())
    msg  <- body$error$message %||% httr2::resp_status_desc(resp)
    stop(sprintf("API error %d: %s", status, msg))
  }

  httr2::resp_body_json(resp)$choices[[1L]]$message$content
}


.parse_llm_response <- function(raw) {
  cleaned <- gsub("^```(?:json)?\\s*|\\s*```$", "", trimws(raw), perl = TRUE)

  parsed <- tryCatch(
    jsonlite::fromJSON(cleaned, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed))
    return(list(valid = FALSE, reason = "Response is not valid JSON"))

  if (is.null(parsed$pass))
    return(list(valid = FALSE, reason = "Missing required field 'pass'"))

  if (!is.list(parsed$criteria) || length(parsed$criteria) == 0L)
    return(list(valid = FALSE, reason = "Missing or empty 'criteria' array"))

  if (!is.null(parsed$score)) {
    s <- suppressWarnings(as.numeric(parsed$score))
    if (is.na(s) || s < 0 || s > 1)
      return(list(valid = FALSE,
                  reason = sprintf("'score' must be 0-1, got: %s", parsed$score)))
  }

  list(valid = TRUE, data = parsed)
}


.text_error_result <- function(name, message) {
  structure(
    list(
      object_name  = name,
      object_type  = "text",
      object_class = "character",
      overall      = NA,
      score        = NA_real_,
      checks       = list(list(name = "llm_grading", pass = NA, message = message)),
      feedback     = NULL
    ),
    class = "robjgrader_result"
  )
}
