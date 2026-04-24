#' Submit a cross-student grading job to the OpenAI Batch API
#'
#' Builds a JSONL batch file from one or more question specs, each containing
#' all student answers, uploads it to the OpenAI Files API, and creates a Batch
#' job with a 24-hour completion window.  Saves a manifest file so that
#' \code{\link{grade_async_collect}} can map results back to questions and
#' students.
#'
#' The OpenAI Batch API charges 50\% of standard token rates and is ideal when
#' results are not needed immediately.
#'
#' @param question_specs Named list.  Each element is a spec for one question:
#'   \describe{
#'     \item{\code{question}}{Character. The question text (required).}
#'     \item{\code{rubric}}{Named character vector of grading criteria (required).}
#'     \item{\code{answers}}{Named character vector/list: student ID → answer text
#'       (required).}
#'     \item{\code{reference}}{Character or \code{NULL}. Optional reference answer.}
#'     \item{\code{max_score}}{Numeric or \code{NULL}.  Stored in manifest for
#'       use by \code{run_llm_collect()}.}
#'   }
#' @param model         Character. Model identifier (default \code{"gpt-4.1-mini"}).
#' @param api_key       Character. OpenAI API bearer token.
#' @param manifest_path Character. Path where the manifest JSON is saved.
#'
#' @return The batch ID (character), invisibly.
#'
#' @seealso \code{\link{grade_async_collect}}, \code{\link{validate_text_students}}
#' @export
grade_async_submit <- function(
  question_specs,
  model        = "gpt-4.1-mini",
  api_key      = Sys.getenv("OPENAI_API_KEY"),
  manifest_path
) {
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  if (!nzchar(api_key))
    stop("'api_key' is empty. Set OPENAI_API_KEY or pass api_key explicitly.")
  if (length(question_specs) == 0L)
    stop("'question_specs' must be a non-empty named list.")
  if (is.null(names(question_specs)) || any(!nzchar(names(question_specs))))
    stop("All elements of 'question_specs' must be named.")

  base_url       <- "https://api.openai.com/v1"
  manifest_items <- list()
  jsonl_lines    <- character(length(question_specs))

  for (i in seq_along(question_specs)) {
    q_name <- names(question_specs)[[i]]
    spec   <- question_specs[[i]]

    for (field in c("question", "rubric", "answers")) {
      if (is.null(spec[[field]]))
        stop(sprintf("Question spec '%s' is missing required field '%s'.", q_name, field))
    }

    student_ids <- names(spec$answers)
    if (is.null(student_ids) || any(!nzchar(student_ids)))
      stop(sprintf("Question spec '%s': 'answers' must be a non-empty named vector.", q_name))

    # Use anonymous sequential indices so no identifying information is sent
    # to the LLM. The manifest stores the mapping for use during collection.
    idx_labels  <- as.character(seq_along(student_ids))
    idx_to_sid  <- setNames(student_ids, idx_labels)

    sys_prompt   <- .build_student_batch_prompt(spec$question, spec$rubric, spec$reference)
    user_content <- paste(
      vapply(idx_labels, function(i) {
        sprintf("### STUDENT %s\n%s", i, as.character(spec$answers[[idx_to_sid[[i]]]]))
      }, character(1L)),
      collapse = "\n\n"
    )

    max_tok <- 200L + length(student_ids) * 150L

    request_body <- list(
      model       = model,
      messages    = list(
        list(role = "system", content = sys_prompt),
        list(role = "user",   content = user_content)
      ),
      temperature = 0,
      max_tokens  = max_tok
    )

    jsonl_lines[[i]] <- jsonlite::toJSON(list(
      custom_id = q_name,
      method    = "POST",
      url       = "/v1/chat/completions",
      body      = request_body
    ), auto_unbox = TRUE)

    manifest_items[[q_name]] <- list(
      student_ids  = student_ids,   # real IDs, for keying the final output
      idx_labels   = idx_labels,    # anonymous indices used in the prompt
      rubric_names = names(spec$rubric),
      max_score    = spec$max_score %||% NULL
    )
  }

  tmp_jsonl <- tempfile(fileext = ".jsonl")
  writeLines(jsonl_lines, tmp_jsonl)
  on.exit(unlink(tmp_jsonl), add = TRUE)

  message(sprintf("Uploading batch input file (%d question(s))...", length(question_specs)))

  upload_resp <- httr2::request(paste0(base_url, "/files")) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_multipart(
      purpose = "batch",
      file    = curl::form_file(tmp_jsonl,
                                type = "application/jsonl",
                                name = "batch_input.jsonl")
    ) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(upload_resp) != 200L) {
    body <- tryCatch(httr2::resp_body_json(upload_resp), error = function(e) list())
    stop(sprintf("File upload failed (HTTP %d): %s",
                 httr2::resp_status(upload_resp),
                 body$error$message %||% "unknown error"))
  }

  file_id <- httr2::resp_body_json(upload_resp)$id
  message(sprintf("File uploaded: %s", file_id))

  message("Creating batch job...")
  batch_resp <- httr2::request(paste0(base_url, "/batches")) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(list(
      input_file_id     = file_id,
      endpoint          = "/v1/chat/completions",
      completion_window = "24h"
    )) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(batch_resp) != 200L) {
    body <- tryCatch(httr2::resp_body_json(batch_resp), error = function(e) list())
    stop(sprintf("Batch creation failed (HTTP %d): %s",
                 httr2::resp_status(batch_resp),
                 body$error$message %||% "unknown error"))
  }

  batch_id <- httr2::resp_body_json(batch_resp)$id

  manifest <- list(
    batch_id  = batch_id,
    model     = model,
    created   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    items     = manifest_items
  )
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

  message(sprintf(
    "Batch submitted successfully.\n  Batch ID  : %s\n  Manifest  : %s\n  Retrieve with grade_async_collect(\"%s\")",
    batch_id, manifest_path, manifest_path
  ))
  invisible(batch_id)
}


#' Collect results from a submitted OpenAI Batch API grading job
#'
#' Checks the status of a previously submitted batch job and, once complete,
#' downloads and parses the results.  Returns a named list of named lists of
#' \code{robjgrader_result} objects: \code{result[[q_name]][[student_id]]}.
#'
#' @param manifest_path Character. Path to the manifest JSON created by
#'   \code{\link{grade_async_submit}}.
#' @param api_key       Character. OpenAI API bearer token.
#' @param wait          Logical.  If \code{FALSE} (default), returns \code{NULL}
#'   when the batch is not yet complete.  If \code{TRUE}, polls until done.
#' @param poll_interval Integer.  Seconds between polls when \code{wait = TRUE}
#'   (default \code{60L}).
#'
#' @return A named list \code{list(q_name = list(student_id = robjgrader_result))},
#'   or \code{NULL} invisibly if the batch is not yet complete and
#'   \code{wait = FALSE}.
#'
#' @seealso \code{\link{grade_async_submit}}
#' @export
grade_async_collect <- function(
  manifest_path,
  api_key       = Sys.getenv("OPENAI_API_KEY"),
  wait          = FALSE,
  poll_interval = 60L
) {
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("Package 'httr2' is required. Install with: install.packages('httr2')")
  if (!file.exists(manifest_path))
    stop(sprintf("Manifest file not found: '%s'", manifest_path))

  base_url <- "https://api.openai.com/v1"
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  batch_id <- manifest$batch_id

  repeat {
    status_resp <- httr2::request(paste0(base_url, "/batches/", batch_id)) |>
      httr2::req_auth_bearer_token(api_key) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(status_resp) != 200L) {
      body <- tryCatch(httr2::resp_body_json(status_resp), error = function(e) list())
      stop(sprintf("Failed to check batch status (HTTP %d): %s",
                   httr2::resp_status(status_resp),
                   body$error$message %||% "unknown error"))
    }

    batch_info <- httr2::resp_body_json(status_resp)
    status     <- batch_info$status

    if (status == "completed") break

    if (status %in% c("failed", "cancelled", "expired")) {
      err_msg <- tryCatch(
        batch_info$errors$data[[1L]]$message,
        error = function(e) NULL
      ) %||% "see OpenAI dashboard for details"
      stop(sprintf("Batch '%s' ended with status '%s': %s", batch_id, status, err_msg))
    }

    if (!isTRUE(wait)) {
      message(sprintf(
        "Batch '%s' status: %s. Call grade_async_collect() again when complete.",
        batch_id, status
      ))
      return(invisible(NULL))
    }

    message(sprintf("Batch '%s' status: %s. Next check in %ds...",
                    batch_id, status, poll_interval))
    Sys.sleep(poll_interval)
  }

  output_file_id <- batch_info$output_file_id
  if (is.null(output_file_id))
    stop("Batch completed but 'output_file_id' is missing from the API response.")

  message(sprintf("Downloading results (file: %s)...", output_file_id))
  content_resp <- httr2::request(paste0(base_url, "/files/", output_file_id, "/content")) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(content_resp) != 200L)
    stop(sprintf("Failed to download batch output file (HTTP %d).",
                 httr2::resp_status(content_resp)))

  raw_text  <- httr2::resp_body_string(content_resp)
  raw_lines <- strsplit(raw_text, "\n")[[1L]]
  raw_lines <- raw_lines[nzchar(trimws(raw_lines))]

  out <- list()

  for (line in raw_lines) {
    line_parsed <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(line_parsed)) {
      warning("Could not parse JSONL line: ", substr(line, 1L, 100L))
      next
    }

    q_name <- line_parsed$custom_id
    if (is.null(q_name) || !q_name %in% names(manifest$items)) {
      warning(sprintf("Unknown custom_id in batch output: '%s'",
                      q_name %||% "<missing>"))
      next
    }

    item         <- manifest$items[[q_name]]
    student_ids  <- item$student_ids
    idx_labels   <- item$idx_labels %||% as.character(seq_along(student_ids))
    idx_to_sid   <- setNames(student_ids, idx_labels)
    rubric_names <- item$rubric_names %||% NULL

    if (!is.null(line_parsed$error)) {
      err_msg <- line_parsed$error$message %||% "unknown error"
      warning(sprintf("Batch request '%s' failed: %s", q_name, err_msg))
      out[[q_name]] <- .make_error_results(student_ids,
        sprintf("Batch request failed: %s", err_msg))
      next
    }

    raw_content <- tryCatch(
      line_parsed$response$body$choices[[1L]]$message$content,
      error = function(e) NULL
    )
    if (is.null(raw_content)) {
      warning(sprintf("Empty response content for question '%s'.", q_name))
      out[[q_name]] <- .make_error_results(student_ids, "Empty LLM response in batch result")
      next
    }

    parse_result <- .parse_student_batch_response(raw_content, idx_labels, rubric_names)

    if (!isTRUE(parse_result$valid)) {
      warning(sprintf("Invalid response for question '%s': %s", q_name, parse_result$reason))
      out[[q_name]] <- .make_error_results(student_ids,
        sprintf("Invalid batch response: %s", parse_result$reason))
      next
    }

    q_results <- vector("list", length(student_ids))
    names(q_results) <- student_ids

    for (idx in idx_labels) {
      sid        <- idx_to_sid[[idx]]
      entry_data <- parse_result$data[[idx]]
      if (is.null(entry_data)) {
        q_results[[sid]] <- .text_error_result(sid, "Missing from batch response")
        next
      }
      checks  <- lapply(entry_data$criteria %||% list(), function(cr) {
        list(name    = cr$name    %||% "criterion",
             pass    = isTRUE(cr$pass),
             message = cr$message %||% "")
      })
      overall <- isTRUE(entry_data$pass)
      score   <- if (!is.null(entry_data$score)) {
        max(0, min(1, as.numeric(entry_data$score)))
      } else {
        if (overall) 1 else 0
      }
      q_results[[sid]] <- structure(
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
    }
    out[[q_name]] <- q_results
  }

  missing_qs <- setdiff(names(manifest$items), names(out))
  if (length(missing_qs) > 0L)
    warning(sprintf("No results found for question(s): %s",
                    paste(missing_qs, collapse = ", ")))

  message(sprintf("Done. Retrieved results for %d question(s).", length(out)))
  out
}


# -- Internal helper -----------------------------------------------------------

.make_error_results <- function(student_ids, message) {
  res <- vector("list", length(student_ids))
  names(res) <- student_ids
  for (sid in student_ids) res[[sid]] <- .text_error_result(sid, message)
  res
}
