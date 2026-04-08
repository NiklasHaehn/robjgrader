library(testthat)

# ==============================================================================
# Helpers
# ==============================================================================

# Build a valid batch JSON response (array of n items)
batch_json <- function(items) {
  jsonlite::toJSON(items, auto_unbox = TRUE)
}

# One valid batch item
# score defaults are consistent with pass: TRUE → 0.8, FALSE → 0.2
batch_item <- function(pass = TRUE, score = if (isTRUE(pass)) 0.8 else 0.2,
                       criteria = list(list(name = "c1", pass = TRUE,
                                            message = "ok")),
                       feedback = NULL) {
  obj <- list(pass = pass, score = score, criteria = criteria)
  if (!is.null(feedback)) obj$feedback <- feedback
  obj
}

# Criterion names match base_items rubric: q1 = "direction", q2 = "reason"
two_items_json <- function(pass1 = TRUE, pass2 = FALSE,
                            score1 = 1.0, score2 = 0.3) {
  as.character(batch_json(list(
    batch_item(pass = pass1, score = score1,
               criteria = list(list(name = "direction", pass = isTRUE(pass1),
                                    message = "ok"))),
    batch_item(pass = pass2, score = score2,
               criteria = list(list(name = "reason", pass = isTRUE(pass2),
                                    message = "missing")))
  )))
}

base_items <- list(
  q1 = list(
    text     = "GDP increases welfare.",
    question = "What is the effect of GDP on welfare?",
    rubric   = c(direction = "mentions increase")
  ),
  q2 = list(
    text     = "Clustering handles within-group correlation.",
    question = "Why use clustered SEs?",
    rubric   = c(reason = "mentions correlation")
  )
)

# ==============================================================================
# Input validation
# ==============================================================================

test_that("validate_text_batch: errors on empty items", {
  expect_error(validate_text_batch(list(), api_key = "k"), "non-empty")
})

test_that("validate_text_batch: errors on missing 'text'", {
  items <- list(q1 = list(question = "Q", rubric = c(a = "b")))
  expect_error(validate_text_batch(items, api_key = "k"), "missing 'text'")
})

test_that("validate_text_batch: errors on missing 'question'", {
  items <- list(q1 = list(text = "x", rubric = c(a = "b")))
  expect_error(validate_text_batch(items, api_key = "k"), "missing 'question'")
})

test_that("validate_text_batch: errors on missing 'rubric'", {
  items <- list(q1 = list(text = "x", question = "Q"))
  expect_error(validate_text_batch(items, api_key = "k"), "missing 'rubric'")
})

test_that("validate_text_batch: errors on no API key", {
  withr::with_envvar(list(GROQ_API_KEY = ""), {
    expect_error(
      validate_text_batch(base_items, api_key = ""),
      "No API key"
    )
  })
})

# ==============================================================================
# Return structure
# ==============================================================================

test_that("validate_text_batch: returns named list of robjgrader_result", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) two_items_json(),
    .package = "Robjgrader"
  )
  res <- validate_text_batch(base_items, api_key = "test-key")

  expect_type(res, "list")
  expect_length(res, 2L)
  expect_named(res, c("q1", "q2"))
  expect_s3_class(res$q1, "robjgrader_result")
  expect_s3_class(res$q2, "robjgrader_result")
})

test_that("validate_text_batch: overall and score correct per item", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) two_items_json(pass1 = TRUE,  score1 = 1.0,
                                                    pass2 = FALSE, score2 = 0.3),
    .package = "Robjgrader"
  )
  res <- validate_text_batch(base_items, api_key = "test-key")

  expect_true(res$q1$overall)
  expect_equal(res$q1$score, 1.0)
  expect_false(res$q2$overall)
  expect_equal(res$q2$score, 0.3)
})

test_that("validate_text_batch: object_name uses item name", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) two_items_json(),
    .package = "Robjgrader"
  )
  res <- validate_text_batch(base_items, api_key = "test-key")
  expect_equal(res$q1$object_name, "q1")
  expect_equal(res$q2$object_name, "q2")
})

test_that("validate_text_batch: item name overrides key name", {
  items <- list(
    q1 = list(text = "x", question = "Q", rubric = c(a = "b"), name = "my_q1")
  )
  local_mocked_bindings(
    .call_llm_tokens = function(...) as.character(batch_json(list(
      batch_item(criteria = list(list(name = "a", pass = TRUE, message = "ok")))
    ))),
    .package = "Robjgrader"
  )
  res <- validate_text_batch(items, api_key = "test-key")
  expect_equal(res$q1$object_name, "my_q1")
})

test_that("validate_text_batch: checks field populated from criteria", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) two_items_json(),
    .package = "Robjgrader"
  )
  res <- validate_text_batch(base_items, api_key = "test-key")
  expect_true(is.list(res$q1$checks))
  expect_length(res$q1$checks, 1L)
  expect_equal(res$q1$checks[[1L]]$name, "direction")
})

test_that("validate_text_batch: unnamed items get q1/q2/... names", {
  items <- list(
    list(text = "a", question = "Q1", rubric = c(a = "b")),
    list(text = "b", question = "Q2", rubric = c(c = "d"))
  )
  local_mocked_bindings(
    .call_llm_tokens = function(...) as.character(batch_json(list(
      batch_item(criteria = list(list(name = "a", pass = TRUE, message = "ok"))),
      batch_item(pass = FALSE, criteria = list(list(name = "c", pass = FALSE, message = "missing")))
    ))),
    .package = "Robjgrader"
  )
  res <- validate_text_batch(items, api_key = "test-key")
  expect_named(res, c("q1", "q2"))
})

# ==============================================================================
# Feedback
# ==============================================================================

test_that("validate_text_batch: feedback populated when requested", {
  items <- list(
    q1 = list(text = "x", question = "Q", rubric = c(a = "b"), feedback = TRUE)
  )
  resp <- as.character(batch_json(list(
    batch_item(criteria = list(list(name = "a", pass = TRUE, message = "ok")),
               feedback = "Well done.")
  )))
  local_mocked_bindings(
    .call_llm_tokens = function(...) resp,
    .package = "Robjgrader"
  )
  res <- validate_text_batch(items, api_key = "test-key")
  expect_equal(res$q1$feedback, "Well done.")
})

test_that("validate_text_batch: feedback NULL when feedback = FALSE", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) as.character(batch_json(list(
      batch_item(criteria = list(list(name = "a", pass = TRUE, message = "ok")))
    ))),
    .package = "Robjgrader"
  )
  items <- list(q1 = list(text = "x", question = "Q", rubric = c(a = "b")))
  res <- validate_text_batch(items, api_key = "test-key")
  expect_null(res$q1$feedback)
})

# ==============================================================================
# Error handling
# ==============================================================================

test_that("validate_text_batch: invalid JSON → error results after retries", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) "not json at all",
    .package = "Robjgrader"
  )
  res <- validate_text_batch(base_items, api_key = "test-key", max_retry = 1L)
  expect_true(all(vapply(res, function(r) is.na(r$overall), logical(1L))))
  expect_match(res$q1$checks[[1L]]$message, "invalid batch response")
})

test_that("validate_text_batch: wrong array length → error results", {
  # Return array of 1 instead of 2
  resp <- as.character(batch_json(list(batch_item())))
  local_mocked_bindings(
    .call_llm_tokens = function(...) resp,
    .package = "Robjgrader"
  )
  res <- validate_text_batch(base_items, api_key = "test-key", max_retry = 1L)
  expect_true(all(vapply(res, function(r) is.na(r$overall), logical(1L))))
  expect_match(res$q1$checks[[1L]]$message, "invalid batch response")
})

# ==============================================================================
# .build_batch_prompt() internals
# ==============================================================================

test_that(".build_batch_prompt: contains all question texts", {
  items <- list(
    list(name = "q1", text = "a", question = "First question",
         rubric = c(x = "criterion x"), feedback = FALSE, ref_text = NULL),
    list(name = "q2", text = "b", question = "Second question",
         rubric = c(y = "criterion y"), feedback = FALSE, ref_text = NULL)
  )
  prompt <- .build_batch_prompt(items)
  expect_match(prompt, "First question")
  expect_match(prompt, "Second question")
  expect_match(prompt, "criterion x")
  expect_match(prompt, "criterion y")
})

test_that(".build_batch_prompt: mentions expected array length", {
  items <- list(
    list(name = "q1", text = "a", question = "Q", rubric = c(x = "y"),
         feedback = FALSE, ref_text = NULL)
  )
  prompt <- .build_batch_prompt(items)
  expect_match(prompt, "1 element")
})

test_that(".build_batch_prompt: includes reference text when present", {
  items <- list(
    list(name = "q1", text = "a", question = "Q", rubric = c(x = "y"),
         feedback = FALSE, ref_text = "The GDP coefficient is 0.5.")
  )
  prompt <- .build_batch_prompt(items)
  expect_match(prompt, "GDP coefficient")
})

# ==============================================================================
# .parse_llm_batch_response() internals
# ==============================================================================

test_that(".parse_llm_batch_response: valid input → valid = TRUE", {
  json <- as.character(batch_json(list(batch_item(), batch_item(pass = FALSE))))
  res  <- .parse_llm_batch_response(json, 2L)
  expect_true(res$valid)
  expect_length(res$data, 2L)
})

test_that(".parse_llm_batch_response: not JSON → valid = FALSE", {
  res <- .parse_llm_batch_response("garbage", 2L)
  expect_false(res$valid)
  expect_match(res$reason, "JSON array")
})

test_that(".parse_llm_batch_response: wrong length → valid = FALSE", {
  json <- as.character(batch_json(list(batch_item())))
  res  <- .parse_llm_batch_response(json, 3L)
  expect_false(res$valid)
  expect_match(res$reason, "length 3")
})

test_that(".parse_llm_batch_response: missing pass field → valid = FALSE", {
  items <- list(list(score = 0.5, criteria = list(list(name = "c", pass = TRUE,
                                                        message = "ok"))))
  json  <- as.character(batch_json(items))
  res   <- .parse_llm_batch_response(json, 1L)
  expect_false(res$valid)
  expect_match(res$reason, "pass")
})

test_that(".parse_llm_batch_response: out-of-range score → valid = FALSE", {
  items <- list(list(pass = TRUE, score = 1.5,
                     criteria = list(list(name = "c", pass = TRUE, message = "ok"))))
  json  <- as.character(batch_json(items))
  res   <- .parse_llm_batch_response(json, 1L)
  expect_false(res$valid)
  expect_match(res$reason, "score")
})

test_that(".parse_llm_batch_response: criteria name not in rubric → valid = FALSE", {
  items <- list(list(pass = TRUE, score = 0.9,
                     criteria = list(list(name = "invented", pass = TRUE, message = "ok"))))
  json  <- as.character(batch_json(items))
  res   <- .parse_llm_batch_response(json, 1L, rubric_names_list = list(c("direction", "magnitude")))
  expect_false(res$valid)
  expect_match(res$reason, "invented")
  expect_match(res$reason, "rubric")
})

test_that(".parse_llm_batch_response: criteria names match rubric (case-insensitive) → valid", {
  items <- list(list(pass = TRUE, score = 0.9,
                     criteria = list(list(name = "Direction", pass = TRUE, message = "ok"))))
  json  <- as.character(batch_json(items))
  res   <- .parse_llm_batch_response(json, 1L, rubric_names_list = list(c("direction")))
  expect_true(res$valid)
})

test_that(".parse_llm_batch_response: pass=TRUE score < 0.5 → valid = FALSE", {
  items <- list(list(pass = TRUE, score = 0.2,
                     criteria = list(list(name = "c1", pass = TRUE, message = "ok"))))
  json  <- as.character(batch_json(items))
  res   <- .parse_llm_batch_response(json, 1L)
  expect_false(res$valid)
  expect_match(res$reason, "inconsistent")
})

test_that(".parse_llm_batch_response: pass=FALSE score >= 0.5 → valid = FALSE", {
  items <- list(list(pass = FALSE, score = 0.7,
                     criteria = list(list(name = "c1", pass = FALSE, message = "fail"))))
  json  <- as.character(batch_json(items))
  res   <- .parse_llm_batch_response(json, 1L)
  expect_false(res$valid)
  expect_match(res$reason, "inconsistent")
})

test_that(".parse_llm_batch_response: consistent pass/score → valid", {
  items <- list(list(pass = TRUE, score = 0.8,
                     criteria = list(list(name = "c1", pass = TRUE, message = "ok"))))
  json  <- as.character(batch_json(items))
  res   <- .parse_llm_batch_response(json, 1L)
  expect_true(res$valid)
})

# ==============================================================================
# Integration: validate_text_batch() results feed into run_autograder()
# ==============================================================================

test_that("validate_text_batch + run_autograder: scores computed correctly", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) two_items_json(pass1 = TRUE, score1 = 0.8,
                                                    pass2 = TRUE, score2 = 1.0),
    .package = "Robjgrader"
  )
  res <- validate_text_batch(base_items, api_key = "test-key")

  test_cases <- list(
    list(name = "Q1", result = res$q1, max_score = 10),
    list(name = "Q2", result = res$q2, max_score = 5)
  )
  out <- run_autograder(test_cases, json_path = tempfile(), verbose = FALSE)

  expect_equal(out$tests[[1L]]$score, 8)    # 0.8 * 10
  expect_equal(out$tests[[2L]]$score, 5)    # 1.0 * 5
})
