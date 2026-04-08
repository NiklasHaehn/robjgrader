#' Read a student text submission
#'
#' Reads a plain-text student answer from a file and returns it as a single
#' character string.  Supported formats:
#'
#' \describe{
#'   \item{\code{.txt}, \code{.md}, \code{.rmd}, \code{.qmd}}{Read with
#'     \code{readLines()}; no additional dependencies required.}
#'   \item{\code{.pdf}}{Converted to plain text via
#'     \code{pdftools::pdf_text()}.  Requires the \pkg{pdftools} package.
#'     Heading styles (bold, font size) are not preserved in the plain-text
#'     output; instruct students to use numbered section labels
#'     (e.g. \code{Q1:}, \code{1.}) for reliable section detection.}
#' }
#'
#' For automatic file discovery, see \code{\link{find_student_text}}.
#' To split the returned text into sections, see
#' \code{\link{split_student_text}}.
#'
#' @param path Path to the submission file.  Must be one of the supported
#'   formats listed above.
#' @return A single character string containing the full file contents.
#' @seealso \code{\link{find_student_text}}, \code{\link{split_student_text}},
#'   \code{\link{validate_text}}
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


#' Split a student text submission into sections
#'
#' Splits a multi-question text submission into individual sections so that
#' each question can be graded independently via \code{validate_text()}.
#' Section boundaries are detected automatically based on the content format:
#'
#' \describe{
#'   \item{Markdown (\code{.md})}{Lines starting with one to three \code{#}
#'     characters (\code{# Q1}, \code{## Part 2}, etc.).}
#'   \item{Plain text / PDF (\code{.txt}, \code{.pdf})}{Lines matching common
#'     question prefixes: \code{Q1:}, \code{Question 1.}, \code{1.},
#'     \code{1)}, \code{Part 1}, \code{Section 1}.  When no prefix pattern
#'     is found, the text is split on double blank lines.}
#' }
#'
#' @param text       Character. Full text from \code{read_student_text()} or
#'   \code{find_student_text()}.
#' @param n_sections Integer or \code{NULL}. Expected number of sections.
#'   Used as a limit when falling back to blank-line splitting.
#' @param format     One of \code{"auto"} (default), \code{"markdown"}, or
#'   \code{"plain"}.  \code{"auto"} detects the format from the content.
#'
#' @return A named character vector.  Names are the detected section headings
#'   (e.g. \code{"Q1: Interpretation"}); values are the section bodies.
#'   Unnamed sections are labelled \code{"Section 1"}, \code{"Section 2"},
#'   etc.
#' @export
split_student_text <- function(text, n_sections = NULL, format = "auto") {
  fmt <- if (format == "auto") .detect_text_format(text) else format

  sections <- switch(fmt,
    markdown = .split_markdown(text),
    plain    = .split_plain(text, n_sections),
    .split_plain(text, n_sections)
  )

  if (length(sections) == 0L) {
    warning("No section boundaries detected; returning full text as one section.",
            call. = FALSE)
    return(stats::setNames(text, "Section 1"))
  }

  sections
}


# -- Section-detection helpers -------------------------------------------------

.detect_text_format <- function(text) {
  if (grepl("(?m)^#{1,3}\\s", text, perl = TRUE)) "markdown" else "plain"
}

.split_markdown <- function(text) {
  lines    <- strsplit(text, "\n")[[1L]]
  headings <- grep("^#{1,3}\\s", lines, perl = TRUE)
  if (length(headings) == 0L) return(character(0L))

  ends <- c(headings[-1L] - 1L, length(lines))

  result <- character(length(headings))
  nms    <- character(length(headings))
  for (i in seq_along(headings)) {
    h       <- headings[i]
    nms[i]  <- trimws(sub("^#{1,3}\\s*", "", lines[h]))
    body    <- lines[(h + 1L):ends[i]]
    result[i] <- trimws(paste(body, collapse = "\n"))
  }
  stats::setNames(result, nms)
}

.split_plain <- function(text, n_sections = NULL) {
  lines <- strsplit(text, "\n")[[1L]]

  # Try common numbered/labelled heading patterns
  patterns <- c(
    "^(Q|Question|Part|Section|Task|Problem)\\s*[0-9]+[.:)]",
    "^[0-9]+[.)\\s]"
  )
  heading_lines <- integer(0L)
  for (pat in patterns) {
    hits <- grep(pat, lines, perl = TRUE, ignore.case = TRUE)
    if (length(hits) > 0L) { heading_lines <- hits; break }
  }

  # Fallback: double blank lines
  if (length(heading_lines) == 0L) {
    chunks <- strsplit(text, "\n{2,}")[[1L]]
    chunks <- trimws(chunks)
    chunks <- chunks[nchar(chunks) > 0L]
    if (!is.null(n_sections) && length(chunks) > n_sections)
      chunks <- chunks[seq_len(n_sections)]
    return(stats::setNames(chunks, paste("Section", seq_along(chunks))))
  }

  ends <- c(heading_lines[-1L] - 1L, length(lines))

  result <- character(length(heading_lines))
  nms    <- character(length(heading_lines))
  for (i in seq_along(heading_lines)) {
    h         <- heading_lines[i]
    nms[i]    <- trimws(lines[h])
    body      <- if (h < ends[i]) lines[(h + 1L):ends[i]] else character(0L)
    result[i] <- trimws(paste(body, collapse = "\n"))
  }
  stats::setNames(result, nms)
}

.extract_section <- function(sections, section, name) {
  if (is.numeric(section)) {
    idx <- as.integer(section)
    if (idx < 1L || idx > length(sections))
      stop(sprintf("Section index %d out of range (%d section(s) found): %s",
                   idx, length(sections),
                   paste(names(sections), collapse = ", ")))
    return(sections[[idx]])
  }

  nms   <- names(sections)
  lower <- tolower(nms)
  query <- tolower(as.character(section))

  exact <- which(lower == query)
  if (length(exact) > 0L) return(sections[[exact[1L]]])

  partial <- which(grepl(query, lower, fixed = TRUE) |
                   vapply(lower, grepl, logical(1L), x = query, fixed = TRUE))
  if (length(partial) > 0L) return(sections[[partial[1L]]])

  stop(sprintf("Section '%s' not found. Available sections: %s",
               section, paste(nms, collapse = ", ")))
}


.extract_section_match <- function(sections, match_spec, name) {
  heading   <- match_spec$heading
  min_words <- match_spec$min_words %||% 0L

  if (is.null(heading))
    stop("'match_section' must include a 'heading' entry (regex pattern).")

  hits <- which(grepl(heading, names(sections), perl = TRUE, ignore.case = TRUE))

  if (min_words > 0L) {
    long_enough <- vapply(sections[hits], function(s) {
      length(strsplit(trimws(s), "\\s+")[[1L]]) >= min_words
    }, logical(1L))
    hits <- hits[long_enough]
  }

  if (length(hits) == 0L)
    stop(sprintf(
      "No section heading matches pattern '%s'%s. Available headings: %s",
      heading,
      if (min_words > 0L) sprintf(" (min_words = %d)", min_words) else "",
      paste(names(sections), collapse = ", ")
    ))

  sections[[hits[[1L]]]]
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
#' @param section   Character or integer. When the submission contains multiple
#'   questions, identifies which section to grade.  A character value is
#'   matched against section headings (case-insensitive, partial match
#'   allowed); an integer selects by position.  When \code{NULL} (default),
#'   the full text is graded without splitting.  Mutually exclusive with
#'   \code{match_section}.
#' @param n_sections Integer or \code{NULL}. Total number of questions in the
#'   submission.  Used as a hint by \code{split_student_text()} when no
#'   heading patterns are found (limits blank-line splitting to the first
#'   \code{n_sections} chunks).
#' @param match_section Named list for regex-based section selection.  Use when
#'   student headings are inconsistent and a fixed label cannot be relied upon.
#'   Required entry: \code{heading} — a regex pattern matched
#'   case-insensitively against section headings via \code{grepl(perl = TRUE)}
#'   (e.g. \code{list(heading = "interpret|gdp|koeffizient")}).  Optional
#'   entry: \code{min_words} — integer; sections whose body contains fewer
#'   words than this threshold are skipped.  The first surviving match is used.
#'   Mutually exclusive with \code{section}.
#' @param reference Model answer used as a grading standard.  Three forms are
#'   accepted:
#'   \describe{
#'     \item{Named list}{Extract a specific Markdown section:
#'       \code{list(path = "solutions.md", section = "Q3")}.  \code{section}
#'       uses case-insensitive partial matching against section headings.}
#'     \item{File path (character)}{The whole file is read via
#'       \code{read_student_text()}.}
#'     \item{Plain character string}{Used directly as the reference text.}
#'   }
#'   In Mode B (\code{question}/\code{rubric}) the reference is embedded in the
#'   auto-generated grading prompt.  In Mode A (\code{prompt}) the reference
#'   text is appended to the system prompt under the header
#'   \emph{"REFERENCE ANSWER (use as grading standard)"}, so the model can
#'   compare the student answer against the reference.
#' @param name      Character. Label for this result, shown in console output
#'   and Gradescope.
#' @param feedback  Logical. If \code{TRUE}, the LLM is asked to provide
#'   written feedback stored in \code{result$feedback} and included in the
#'   Gradescope output when the answer is not fully correct.  The feedback is
#'   constrained by prompt instructions: it must be constructive and precise,
#'   written in plain English, at most 3 sentences and 200 words, free of
#'   greetings or sign-offs, and must start directly with the substantive
#'   comment.  No reference to automated grading or language models is
#'   permitted.
#' @param model     Character. Model identifier.  Defaults to
#'   \code{getOption("robjgrader.llm.model")} if set, otherwise
#'   \code{"llama-3.3-70b-versatile"}.  See \code{\link{robjgrader_set_llm}}.
#' @param base_url  Character. Base URL of the OpenAI-compatible API endpoint.
#'   Defaults to \code{getOption("robjgrader.llm.base_url")} if set, otherwise
#'   the Groq endpoint.
#' @param api_key   Character. API bearer token.  Defaults to
#'   \code{getOption("robjgrader.llm.api_key")} if set, otherwise the
#'   \env{GROQ_API_KEY} environment variable.
#' @param max_retry Integer. Maximum regrading attempts on invalid responses.
#'   Default \code{3L}.
#'
#' @return A \code{robjgrader_result} with fields \code{score} (normalized
#'   0--1, used for partial credit in \code{run_autograder()}) and, when
#'   \code{feedback = TRUE}, \code{feedback} (character or \code{NULL}).
#' @export
validate_text <- function(
  text,
  section       = NULL,
  n_sections    = NULL,
  match_section = NULL,
  prompt        = NULL,
  question      = NULL,
  rubric        = NULL,
  reference     = NULL,
  name          = "text",
  feedback      = FALSE,
  model         = getOption("robjgrader.llm.model",    "llama-3.3-70b-versatile"),
  base_url      = getOption("robjgrader.llm.base_url", "https://api.groq.com/openai/v1"),
  api_key       = getOption("robjgrader.llm.api_key",  Sys.getenv("GROQ_API_KEY")),
  max_retry     = 3L
) {
  if (is.null(prompt) && (is.null(question) || is.null(rubric)))
    stop("Provide either 'prompt' (Mode A) or both 'question' and 'rubric' (Mode B).")
  if (!is.null(prompt) && (!is.null(question) || !is.null(rubric)))
    stop("'prompt' and 'question'/'rubric' are mutually exclusive.")
  if (!is.null(section) && !is.null(match_section))
    stop("'section' and 'match_section' are mutually exclusive.")
  if (nchar(api_key) == 0L)
    stop("No API key found. Call robjgrader_set_llm() or set GROQ_API_KEY.")

  if (!is.null(section)) {
    sections <- split_student_text(text, n_sections = n_sections)
    text     <- .extract_section(sections, section, name)
  } else if (!is.null(match_section)) {
    sections <- split_student_text(text, n_sections = n_sections)
    text     <- .extract_section_match(sections, match_section, name)
  }

  ref_text <- if (is.list(reference)) {
    full     <- read_student_text(reference$path)
    ref_secs <- split_student_text(full)
    .extract_section(ref_secs, reference$section, paste0(name, " reference"))
  } else if (is.character(reference) && length(reference) == 1L &&
             file.exists(reference)) {
    read_student_text(reference)
  } else if (!is.null(reference)) {
    as.character(reference)
  } else {
    NULL
  }

  sys_prompt <- if (!is.null(prompt)) {
    if (!is.null(ref_text))
      paste0(prompt, "\n\nREFERENCE ANSWER (use as grading standard):\n", ref_text)
    else
      prompt
  } else {
    .build_text_prompt(question, rubric, feedback, ref_text)
  }

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
      temperature = 0,
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


#' Validate multiple text answers in a single LLM call
#'
#' Sends all questions, rubrics, and student answers in one API request,
#' reducing token costs by paying for the system-prompt only once instead of
#' once per question.  Returns a named list of \code{robjgrader_result} objects
#' that can be passed directly to \code{run_autograder()}.
#'
#' @details
#' Each element of \code{items} is a named list with the following fields:
#' \describe{
#'   \item{\code{text}}{Character. The student answer text (required).}
#'   \item{\code{question}}{Character. The question prompt (required).}
#'   \item{\code{rubric}}{Named character vector of grading criteria (required).}
#'   \item{\code{name}}{Character. Label for the result object.  Defaults to
#'     the list element name or \code{"q<i>"}.}
#'   \item{\code{feedback}}{Logical. Request written feedback.  Defaults to
#'     \code{FALSE}.}
#'   \item{\code{reference}}{Character or \code{NULL}. Optional reference
#'     answer injected into the prompt.}
#'   \item{\code{section}}{Character or \code{NULL}. Section heading to
#'     extract from \code{text} before grading.}
#'   \item{\code{match_section}}{List or \code{NULL}. Regex-based section
#'     selector; see \code{\link{validate_text}}.}
#' }
#'
#' @param items     Named or unnamed list of item specs.  See Details.
#' @param model     Character. Model identifier.
#' @param base_url  Character. API base URL.
#' @param api_key   Character. Bearer token.
#' @param max_retry Integer. Number of retry attempts if the LLM returns
#'   malformed JSON (default \code{3L}).
#'
#' @return A named list of \code{robjgrader_result} objects, one per item.
#'
#' @examples
#' \dontrun{
#' robjgrader_set_llm(provider = "openai")
#'
#' text <- read_student_text("submission.pdf")
#'
#' results <- validate_text_batch(list(
#'   q1 = list(
#'     text     = text,
#'     question = "Interpret the coefficient on log GDP.",
#'     rubric   = c(direction = "mentions positive effect",
#'                  magnitude = "comments on magnitude")
#'   ),
#'   q2 = list(
#'     text     = text,
#'     question = "Why do we use clustered standard errors?",
#'     rubric   = c(reason = "mentions within-group correlation")
#'   )
#' ))
#'
#' run_autograder(list(
#'   list(name = "Q1", result = results$q1, max_score = 5),
#'   list(name = "Q2", result = results$q2, max_score = 3)
#' ))
#' }
#' @export
validate_text_batch <- function(
  items,
  model     = getOption("robjgrader.llm.model",    "llama-3.3-70b-versatile"),
  base_url  = getOption("robjgrader.llm.base_url", "https://api.groq.com/openai/v1"),
  api_key   = getOption("robjgrader.llm.api_key",  Sys.getenv("GROQ_API_KEY")),
  max_retry = 3L
) {
  if (!is.list(items) || length(items) == 0L)
    stop("'items' must be a non-empty list.")
  if (nchar(api_key) == 0L)
    stop("No API key found. Call robjgrader_set_llm() or set GROQ_API_KEY.")

  # Normalise item names
  item_names <- names(items)
  if (is.null(item_names)) item_names <- rep("", length(items))
  item_names <- ifelse(nzchar(item_names), item_names,
                       paste0("q", seq_along(items)))

  # Pre-process each item: extract section, resolve reference text, build name
  processed <- lapply(seq_along(items), function(i) {
    it   <- items[[i]]
    nm   <- item_names[[i]]
    text <- it[["text"]]
    if (is.null(text)) stop(sprintf("Item '%s' is missing 'text'.", nm))
    if (is.null(it[["question"]])) stop(sprintf("Item '%s' is missing 'question'.", nm))
    if (is.null(it[["rubric"]]))   stop(sprintf("Item '%s' is missing 'rubric'.", nm))

    # Section extraction
    if (!is.null(it[["section"]]) && !is.null(it[["match_section"]]))
      stop(sprintf("Item '%s': 'section' and 'match_section' are mutually exclusive.", nm))
    if (!is.null(it[["section"]])) {
      secs <- split_student_text(text)
      text <- .extract_section(secs, it[["section"]], nm)
    } else if (!is.null(it[["match_section"]])) {
      secs <- split_student_text(text)
      text <- .extract_section_match(secs, it[["match_section"]], nm)
    }

    # Reference text
    ref <- it[["reference"]]
    ref_text <- if (is.list(ref)) {
      full <- read_student_text(ref$path)
      ref_secs <- split_student_text(full)
      .extract_section(ref_secs, ref$section, paste0(nm, " reference"))
    } else if (is.character(ref) && length(ref) == 1L && file.exists(ref)) {
      read_student_text(ref)
    } else if (!is.null(ref)) {
      as.character(ref)
    } else {
      NULL
    }

    list(
      name      = it[["name"]] %||% nm,
      text      = text,
      question  = it[["question"]],
      rubric    = it[["rubric"]],
      feedback  = isTRUE(it[["feedback"]]),
      ref_text  = ref_text
    )
  })

  n <- length(processed)

  # Build system prompt once for all items
  sys_prompt <- .build_batch_prompt(processed)

  # Build user message: all answers numbered
  user_content <- paste(
    vapply(seq_len(n), function(i) {
      sprintf("### ANSWER %d\n%s", i, processed[[i]]$text)
    }, character(1L)),
    collapse = "\n\n"
  )

  messages <- list(
    list(role = "system", content = sys_prompt),
    list(role = "user",   content = user_content)
  )

  # Call LLM with retry
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

    # Scale max_tokens with number of items
    max_tok <- 400L + n * 300L

    raw <- tryCatch(
      .call_llm_tokens(msgs, model, base_url, api_key, max_tok),
      error = function(e) stop(sprintf("LLM API call failed: %s", conditionMessage(e)))
    )

    result <- .parse_llm_batch_response(raw, n)
    if (isTRUE(result$valid)) {
      parsed <- result$data
      break
    }
    error_note <- list(raw = raw, reason = result$reason)
  }

  # Build result list — one entry per item
  out <- vector("list", n)
  names(out) <- item_names

  for (i in seq_len(n)) {
    nm <- processed[[i]]$name
    if (is.null(parsed)) {
      out[[i]] <- .text_error_result(nm, sprintf(
        "LLM returned an invalid batch response after %d attempt(s): %s",
        max_retry, error_note$reason
      ))
      next
    }

    entry <- parsed[[i]]
    checks <- lapply(entry$criteria %||% list(), function(cr) {
      list(name    = cr$name    %||% "criterion",
           pass    = isTRUE(cr$pass),
           message = cr$message %||% "")
    })

    overall <- isTRUE(entry$pass)
    score   <- if (!is.null(entry$score)) {
      max(0, min(1, as.numeric(entry$score)))
    } else {
      if (overall) 1 else 0
    }

    fb <- if (processed[[i]]$feedback && !is.null(entry$feedback)) entry$feedback else NULL

    out[[i]] <- structure(
      list(
        object_name  = nm,
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

  out
}


# -- Batch helpers -------------------------------------------------------------

.build_batch_prompt <- function(processed) {
  n <- length(processed)

  items_block <- paste(
    vapply(seq_len(n), function(i) {
      it      <- processed[[i]]
      rubric  <- it$rubric
      crit_block <- paste(
        mapply(function(nm, desc) sprintf("    - %s: %s", nm, desc),
               names(rubric), as.character(rubric)),
        collapse = "\n"
      )
      ref_block <- if (!is.null(it$ref_text) && nzchar(trimws(it$ref_text)))
        sprintf("\n  Reference answer: %s", it$ref_text)
      else ""

      fb_note <- if (it$feedback) "\n  (also provide a 'feedback' field for this item)" else ""

      sprintf("ITEM %d\n  Question: %s\n  Criteria:\n%s%s%s",
              i, it$question, crit_block, ref_block, fb_note)
    }, character(1L)),
    collapse = "\n\n"
  )

  feedback_schema <- if (any(vapply(processed, `[[`, logical(1L), "feedback")))
    "\n    \"feedback\": \"<optional written feedback, max 3 sentences>\","
  else ""

  sprintf(
    paste0(
      "You are a grader for a political science methods course.\n\n",
      "You will receive %d student answers below (labeled ANSWER 1 through ANSWER %d).\n",
      "Grade each answer independently against its criteria.\n\n",
      "%s\n\n",
      "Return ONLY a JSON array with exactly %d objects -- no prose, ",
      "no markdown code fences:\n",
      "[\n",
      "  {\n",
      "    \"pass\": <true/false>,\n",
      "    \"score\": <0-1>,\n",
      "    \"criteria\": [\n",
      "      { \"name\": \"<criterion>\", \"pass\": <true/false>,",
      " \"message\": \"<1 sentence>\" }\n",
      "    ]%s\n",
      "  }\n",
      "]\n\n",
      "The array must have exactly %d elements in the same order as the answers.\n",
      "Return ONLY the JSON array."
    ),
    n, n, items_block, n, feedback_schema, n
  )
}


.call_llm_tokens <- function(messages, model, base_url, api_key, max_tokens) {
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")

  resp <- httr2::request(paste0(base_url, "/chat/completions")) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(list(
      model       = model,
      messages    = messages,
      temperature = 0,
      max_tokens  = max_tokens
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


.parse_llm_batch_response <- function(raw, expected_n) {
  cleaned <- gsub("^```(?:json)?\\s*|\\s*```$", "", trimws(raw), perl = TRUE)

  parsed <- tryCatch(
    jsonlite::fromJSON(cleaned, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || !is.list(parsed))
    return(list(valid = FALSE, reason = "Response is not a valid JSON array"))

  if (length(parsed) != expected_n)
    return(list(valid = FALSE, reason = sprintf(
      "Expected array of length %d, got %d", expected_n, length(parsed)
    )))

  for (i in seq_along(parsed)) {
    entry <- parsed[[i]]
    if (is.null(entry$pass))
      return(list(valid = FALSE,
                  reason = sprintf("Item %d missing required field 'pass'", i)))
    if (!is.list(entry$criteria) || length(entry$criteria) == 0L)
      return(list(valid = FALSE,
                  reason = sprintf("Item %d missing or empty 'criteria' array", i)))
    if (!is.null(entry$score)) {
      s <- suppressWarnings(as.numeric(entry$score))
      if (is.na(s) || s < 0 || s > 1)
        return(list(valid = FALSE,
                    reason = sprintf("Item %d 'score' must be 0-1, got: %s",
                                     i, entry$score)))
    }
  }

  list(valid = TRUE, data = parsed)
}
