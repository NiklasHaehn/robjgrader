#' Grade one question across multiple student answers in a single LLM call
#'
#' Sends all student answers for a single question to the LLM in one API request,
#' reducing token costs by paying for the system prompt (question + rubric) once
#' instead of once per student.  Returns a named list of
#' \code{robjgrader_result} objects keyed by the student identifiers supplied
#' in \code{answers}.
#'
#' @details
#' \strong{Retry logic:} The \code{max_retry} limit is a per-student absolute
#' ceiling on the total number of API calls that may touch a given student's
#' answer.  On the first call all students are included; invalid entries are
#' re-sent in subsequent calls until either the limit is reached or a valid
#' result is obtained.  Students that exhaust all retries receive an error
#' result (\code{overall = NA}, \code{score = NA}).
#'
#' @param question Character. The question text shown to the LLM.
#' @param rubric   Named character vector of grading criteria.  Names become
#'   criterion identifiers in the response.
#' @param answers  Named character vector or list mapping student IDs to answer
#'   text.  Names are used as student identifiers throughout.
#' @param reference Character or \code{NULL}.  Optional reference answer
#'   injected into the system prompt.
#' @param model    Character.  Model identifier.
#' @param base_url Character.  OpenAI-compatible API base URL.
#' @param api_key  Character.  Bearer token.
#' @param max_retry Integer.  Maximum API calls per student (absolute limit,
#'   default \code{3L}).
#'
#' @return A named list of \code{robjgrader_result} objects, one per element
#'   of \code{answers}.
#'
#' @seealso \code{\link{validate_text}}, \code{\link{validate_text_batch}},
#'   \code{\link{grade_async_submit}}, \code{\link{grade_async_collect}}
#'
#' @examples
#' \dontrun{
#' robjgrader_set_llm(provider = "openai", model = "gpt-4.1-mini")
#'
#' answers <- c(
#'   student_1 = "Higher values indicate more conservative cities.",
#'   student_2 = "I'm not sure what the sign means."
#' )
#'
#' results <- validate_text_students(
#'   question = "What does the sign of mrp_ideology indicate?",
#'   rubric   = c(direction = "Higher values = more conservative"),
#'   answers  = answers
#' )
#' results$student_1$overall  # TRUE
#' results$student_2$overall  # FALSE
#' }
#' @export
validate_text_students <- function(
  question,
  rubric,
  answers,
  reference  = NULL,
  model      = getOption("robjgrader.llm.model",    "gpt-4.1-mini"),
  base_url   = getOption("robjgrader.llm.base_url", "https://api.openai.com/v1"),
  api_key    = getOption("robjgrader.llm.api_key",  Sys.getenv("OPENAI_API_KEY")),
  max_retry  = 3L
) {
  if (is.null(names(answers)) || any(!nzchar(names(answers))))
    stop("'answers' must be a named vector or list with non-empty names (student IDs).")
  if (length(answers) == 0L)
    stop("'answers' must be non-empty.")
  if (is.null(question) || !nzchar(trimws(question)))
    stop("'question' must be a non-empty character string.")
  if (is.null(rubric) || length(rubric) == 0L || is.null(names(rubric)))
    stop("'rubric' must be a non-empty named character vector.")
  if (!is.numeric(max_retry) || max_retry < 1L)
    stop("'max_retry' must be a positive integer.")
  max_retry <- as.integer(max_retry)

  student_ids <- names(answers)
  n_all       <- length(student_ids)

  retry_count          <- integer(n_all)
  names(retry_count)   <- student_ids
  results              <- vector("list", n_all)
  names(results)       <- student_ids

  pending    <- student_ids
  error_note <- NULL
  rubric_names <- names(rubric)
  sys_prompt   <- .build_student_batch_prompt(question, rubric, reference)

  while (length(pending) > 0L) {
    can_retry <- pending[retry_count[pending] < max_retry]
    if (length(can_retry) == 0L) break

    user_content <- paste(
      vapply(can_retry, function(sid) {
        sprintf("### STUDENT %s\n%s", sid, as.character(answers[[sid]]))
      }, character(1L)),
      collapse = "\n\n"
    )

    msgs <- if (!is.null(error_note)) {
      list(
        list(role = "system",    content = sys_prompt),
        list(role = "user",      content = error_note$user),
        list(role = "assistant", content = error_note$raw),
        list(role = "user",
             content = paste0(
               "Your previous response was invalid: ", error_note$reason,
               "\nPlease return only valid JSON matching the required schema."
             ))
      )
    } else {
      list(
        list(role = "system", content = sys_prompt),
        list(role = "user",   content = user_content)
      )
    }

    max_tok <- 200L + length(can_retry) * 150L

    raw <- tryCatch(
      .call_llm_tokens(msgs, model, base_url, api_key, max_tok),
      error = function(e) stop(sprintf("LLM API call failed: %s", conditionMessage(e)))
    )

    retry_count[can_retry] <- retry_count[can_retry] + 1L

    parse_result <- .parse_student_batch_response(raw, can_retry, rubric_names)

    if (isTRUE(parse_result$valid)) {
      # Resolve valid entries
      for (sid in names(parse_result$data)) {
        entry  <- parse_result$data[[sid]]
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
        results[[sid]] <- structure(
          list(
            object_name  = sid,
            object_type  = "text",
            object_class = "character",
            overall      = overall,
            score        = score,
            checks       = checks,
            feedback     = NULL
          ),
          class = "robjgrader_result"
        )
        pending <- setdiff(pending, sid)
      }

      # Failed entries: build a consolidated error note for the retry prompt
      if (length(parse_result$failed) > 0L) {
        combined_reason <- paste(unlist(parse_result$failed), collapse = "; ")
        error_note <- list(user = user_content, raw = raw, reason = combined_reason)
      } else {
        error_note <- NULL
      }
    } else {
      # Whole array invalid — retry all can_retry
      error_note <- list(user = user_content, raw = raw, reason = parse_result$reason)
    }
  }

  for (sid in intersect(pending, student_ids)) {
    reason <- error_note$reason %||% "max retries exhausted"
    results[[sid]] <- .text_error_result(
      sid,
      sprintf("LLM returned invalid response after %d attempt(s): %s", max_retry, reason)
    )
  }

  results
}


# -- Internal helpers ----------------------------------------------------------

.build_student_batch_prompt <- function(question, rubric, reference = NULL) {
  crit_block <- paste(
    mapply(function(nm, desc) sprintf("- %s: %s", nm, desc),
           names(rubric), as.character(rubric)),
    collapse = "\n"
  )

  ref_block <- if (!is.null(reference) && nzchar(trimws(as.character(reference)))) {
    sprintf("\nREFERENCE ANSWER (use as the grading standard):\n%s\n",
            as.character(reference))
  } else {
    ""
  }

  n_crit <- length(rubric)
  sprintf(
    paste0(
      "You are a grader for a political science methods course.\n\n",
      "QUESTION:\n%s\n\n",
      "GRADING CRITERIA (assess each independently):\n%s\n",
      "%s\n",
      "You will receive answers from multiple students, each labeled ### STUDENT <id>.\n",
      "Grade each student's answer independently against all %d criterion/criteria above.\n\n",
      "Return ONLY a JSON array — no prose, no markdown fences:\n",
      "[\n",
      "  {\n",
      "    \"student_id\": \"<id as shown above>\",\n",
      "    \"pass\": <true if overall satisfactory, else false>,\n",
      "    \"score\": <0.0-1.0>,\n",
      "    \"criteria\": [\n",
      "      { \"name\": \"<criterion name>\", \"pass\": <true/false>,",
      " \"message\": \"<1 sentence>\" }\n",
      "    ]\n",
      "  }\n",
      "]\n\n",
      "One element per student in the order provided. Return ONLY the JSON array."
    ),
    question, crit_block, ref_block, n_crit
  )
}


# Returns:
#   list(valid = FALSE, reason = ...)     — array itself is not parseable
#   list(valid = TRUE,
#        data   = list(sid → entry),      — validated entries
#        failed = list(sid → reason))     — per-student failures within valid array
.parse_student_batch_response <- function(raw, student_ids, rubric_names = NULL) {
  cleaned <- gsub("^```(?:json)?\\s*|\\s*```$", "", trimws(raw), perl = TRUE)

  parsed <- tryCatch(
    jsonlite::fromJSON(cleaned, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || !is.list(parsed))
    return(list(valid = FALSE, reason = "Response is not a valid JSON array"))

  if (length(parsed) == 0L)
    return(list(valid = FALSE, reason = "Response JSON array is empty"))

  resp_ids <- vapply(parsed, function(e) {
    id <- e$student_id
    if (is.null(id)) "" else as.character(id)
  }, character(1L))

  dups <- resp_ids[duplicated(resp_ids) & nzchar(resp_ids)]
  if (length(dups) > 0L)
    return(list(valid = FALSE, reason = sprintf(
      "Duplicate student_id(s) in response: %s", paste(unique(dups), collapse = ", ")
    )))

  data   <- list()
  failed <- list()

  for (entry in parsed) {
    sid <- entry$student_id %||% ""
    if (!sid %in% student_ids) next

    fail_reason <- NULL

    if (is.null(entry$pass)) {
      fail_reason <- sprintf("Student '%s' missing required field 'pass'", sid)
    } else if (!is.list(entry$criteria) || length(entry$criteria) == 0L) {
      fail_reason <- sprintf("Student '%s' missing or empty 'criteria' array", sid)
    } else if (!is.null(rubric_names) && length(rubric_names) > 0L) {
      ret_names <- vapply(entry$criteria, function(cr) cr$name %||% "", character(1L))
      unmatched <- ret_names[!tolower(ret_names) %in% tolower(rubric_names)]
      if (length(unmatched) > 0L)
        fail_reason <- sprintf(
          "Student '%s' criteria name(s) not in rubric: '%s'. Expected: %s",
          sid, paste(unmatched, collapse = "', '"), paste(rubric_names, collapse = ", ")
        )
    }

    if (is.null(fail_reason) && !is.null(entry$score)) {
      s <- suppressWarnings(as.numeric(entry$score))
      if (is.na(s) || s < 0 || s > 1) {
        fail_reason <- sprintf("Student '%s' 'score' must be 0-1, got: %s", sid, entry$score)
      } else if (isTRUE(entry$pass) && s < 0.5) {
        fail_reason <- sprintf(
          "Student '%s' 'pass' is TRUE but 'score' is %.2f (< 0.5) -- inconsistent", sid, s
        )
      } else if (!isTRUE(entry$pass) && s >= 0.5) {
        fail_reason <- sprintf(
          "Student '%s' 'pass' is FALSE but 'score' is %.2f (>= 0.5) -- inconsistent", sid, s
        )
      }
    }

    if (!is.null(fail_reason)) {
      failed[[sid]] <- fail_reason
    } else {
      data[[sid]] <- entry
    }
  }

  # Students not mentioned at all in the response
  missing_ids <- setdiff(student_ids, c(names(data), names(failed)))
  for (sid in missing_ids)
    failed[[sid]] <- sprintf("Student '%s' missing from response", sid)

  list(valid = TRUE, data = data, failed = failed)
}
