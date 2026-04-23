library(testthat)

# ==============================================================================
# Helpers
# ==============================================================================

student_response <- function(sid, pass = TRUE,
                              score = if (isTRUE(pass)) 0.9 else 0.2,
                              crit_name = "direction") {
  list(
    student_id = sid,
    pass       = pass,
    score      = score,
    criteria   = list(list(name = crit_name,
                           pass = isTRUE(pass),
                           message = if (isTRUE(pass)) "correct" else "missing"))
  )
}

students_json <- function(...) {
  items <- list(...)
  as.character(jsonlite::toJSON(items, auto_unbox = TRUE))
}

base_rubric  <- c(direction = "Higher values = more conservative")
base_answers <- c(s1 = "Higher values are more conservative.",
                  s2 = "I am not sure.")

# ==============================================================================
# Input validation
# ==============================================================================

test_that("validate_text_students: errors on unnamed answers", {
  expect_error(
    validate_text_students("Q", base_rubric, c("a", "b"), api_key = "k"),
    "named"
  )
})

test_that("validate_text_students: errors on empty answers", {
  expect_error(
    validate_text_students("Q", base_rubric, character(0), api_key = "k"),
    "non-empty"
  )
})

test_that("validate_text_students: errors on missing question", {
  expect_error(
    validate_text_students("", base_rubric, base_answers, api_key = "k"),
    "question"
  )
})

test_that("validate_text_students: errors on unnamed rubric", {
  expect_error(
    validate_text_students("Q", c("a", "b"), base_answers, api_key = "k"),
    "rubric"
  )
})

test_that("validate_text_students: errors on invalid max_retry", {
  expect_error(
    validate_text_students("Q", base_rubric, base_answers, api_key = "k", max_retry = 0),
    "max_retry"
  )
})

# ==============================================================================
# .build_student_batch_prompt()
# ==============================================================================

test_that(".build_student_batch_prompt: contains question and criteria", {
  p <- Robjgrader:::.build_student_batch_prompt(
    "What is X?",
    c(a = "criterion a", b = "criterion b")
  )
  expect_match(p, "What is X\\?")
  expect_match(p, "criterion a")
  expect_match(p, "criterion b")
  expect_match(p, "student_id")
  expect_match(p, "JSON array")
})

test_that(".build_student_batch_prompt: includes reference when provided", {
  p <- Robjgrader:::.build_student_batch_prompt(
    "What is X?",
    c(a = "criterion a"),
    reference = "The answer is Y."
  )
  expect_match(p, "The answer is Y\\.")
  expect_match(p, "REFERENCE ANSWER")
})

test_that(".build_student_batch_prompt: no reference block when NULL", {
  p <- Robjgrader:::.build_student_batch_prompt("What is X?", c(a = "a"))
  expect_false(grepl("REFERENCE ANSWER", p))
})

# ==============================================================================
# .parse_student_batch_response()
# ==============================================================================

test_that(".parse_student_batch_response: valid input → valid=TRUE", {
  json <- students_json(student_response("s1"), student_response("s2", pass = FALSE))
  res  <- Robjgrader:::.parse_student_batch_response(json, c("s1", "s2"))
  expect_true(res$valid)
  expect_named(res$data, c("s1", "s2"), ignore.order = TRUE)
})

test_that(".parse_student_batch_response: invalid JSON → valid=FALSE", {
  res <- Robjgrader:::.parse_student_batch_response("not json", c("s1"))
  expect_false(res$valid)
  expect_match(res$reason, "JSON array")
})

test_that(".parse_student_batch_response: empty array → valid=FALSE", {
  res <- Robjgrader:::.parse_student_batch_response("[]", c("s1"))
  expect_false(res$valid)
  expect_match(res$reason, "empty")
})

test_that(".parse_student_batch_response: missing student_id → in failed", {
  json <- students_json(student_response("s1"))
  res  <- Robjgrader:::.parse_student_batch_response(json, c("s1", "s2"))
  expect_true(res$valid)
  expect_true("s1" %in% names(res$data))
  expect_true("s2" %in% names(res$failed))
  expect_match(res$failed$s2, "missing")
})

test_that(".parse_student_batch_response: duplicate student_id → valid=FALSE", {
  json <- students_json(student_response("s1"), student_response("s1"))
  res  <- Robjgrader:::.parse_student_batch_response(json, c("s1"))
  expect_false(res$valid)
  expect_match(res$reason, "Duplicate")
})

test_that(".parse_student_batch_response: missing 'pass' → in failed", {
  items <- list(list(student_id = "s1", score = 0.5,
                     criteria = list(list(name = "direction", pass = TRUE, message = "ok"))))
  json  <- as.character(jsonlite::toJSON(items, auto_unbox = TRUE))
  res   <- Robjgrader:::.parse_student_batch_response(json, "s1")
  expect_true(res$valid)
  expect_true("s1" %in% names(res$failed))
  expect_match(res$failed$s1, "pass")
})

test_that(".parse_student_batch_response: empty criteria → in failed", {
  items <- list(list(student_id = "s1", pass = TRUE, score = 0.9, criteria = list()))
  json  <- as.character(jsonlite::toJSON(items, auto_unbox = TRUE))
  res   <- Robjgrader:::.parse_student_batch_response(json, "s1")
  expect_true(res$valid)
  expect_true("s1" %in% names(res$failed))
  expect_match(res$failed$s1, "criteria")
})

test_that(".parse_student_batch_response: criteria name not in rubric → in failed", {
  json <- students_json(student_response("s1", crit_name = "invented"))
  res  <- Robjgrader:::.parse_student_batch_response(json, "s1",
                                                      rubric_names = c("direction"))
  expect_true(res$valid)
  expect_true("s1" %in% names(res$failed))
  expect_match(res$failed$s1, "invented")
  expect_match(res$failed$s1, "rubric")
})

test_that(".parse_student_batch_response: criteria name matching is case-insensitive", {
  json <- students_json(student_response("s1", crit_name = "Direction"))
  res  <- Robjgrader:::.parse_student_batch_response(json, "s1",
                                                      rubric_names = c("direction"))
  expect_true(res$valid)
  expect_true("s1" %in% names(res$data))
  expect_length(res$failed, 0L)
})

test_that(".parse_student_batch_response: score out of range → in failed", {
  items <- list(list(student_id = "s1", pass = TRUE, score = 1.5,
                     criteria = list(list(name = "direction", pass = TRUE, message = "ok"))))
  json  <- as.character(jsonlite::toJSON(items, auto_unbox = TRUE))
  res   <- Robjgrader:::.parse_student_batch_response(json, "s1")
  expect_true(res$valid)
  expect_match(res$failed$s1, "score")
})

test_that(".parse_student_batch_response: pass=TRUE score<0.5 → in failed", {
  items <- list(list(student_id = "s1", pass = TRUE, score = 0.2,
                     criteria = list(list(name = "direction", pass = TRUE, message = "ok"))))
  json  <- as.character(jsonlite::toJSON(items, auto_unbox = TRUE))
  res   <- Robjgrader:::.parse_student_batch_response(json, "s1")
  expect_true(res$valid)
  expect_match(res$failed$s1, "inconsistent")
})

test_that(".parse_student_batch_response: pass=FALSE score>=0.5 → in failed", {
  items <- list(list(student_id = "s1", pass = FALSE, score = 0.7,
                     criteria = list(list(name = "direction", pass = FALSE, message = "bad"))))
  json  <- as.character(jsonlite::toJSON(items, auto_unbox = TRUE))
  res   <- Robjgrader:::.parse_student_batch_response(json, "s1")
  expect_true(res$valid)
  expect_match(res$failed$s1, "inconsistent")
})

# ==============================================================================
# validate_text_students() — mocked LLM
# ==============================================================================

test_that("validate_text_students: returns robjgrader_result per student", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) {
      students_json(student_response("s1", pass = TRUE, score = 0.9),
                    student_response("s2", pass = FALSE, score = 0.2))
    },
    .package = "Robjgrader"
  )
  res <- validate_text_students(
    question = "What does the sign mean?",
    rubric   = base_rubric,
    answers  = base_answers,
    api_key  = "test-key"
  )
  expect_named(res, c("s1", "s2"), ignore.order = FALSE)
  expect_s3_class(res$s1, "robjgrader_result")
  expect_true(res$s1$overall)
  expect_false(res$s2$overall)
  expect_equal(res$s1$score, 0.9)
  expect_equal(res$s2$score, 0.2)
})

test_that("validate_text_students: object_name matches student ID", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) students_json(student_response("s1")),
    .package = "Robjgrader"
  )
  res <- validate_text_students("Q", base_rubric, c(s1 = "answer"), api_key = "k")
  expect_equal(res$s1$object_name, "s1")
  expect_equal(res$s1$object_type, "text")
})

test_that("validate_text_students: checks populated from criteria", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) students_json(student_response("s1")),
    .package = "Robjgrader"
  )
  res <- validate_text_students("Q", base_rubric, c(s1 = "answer"), api_key = "k")
  expect_true(is.list(res$s1$checks))
  expect_length(res$s1$checks, 1L)
  expect_equal(res$s1$checks[[1L]]$name, "direction")
})

test_that("validate_text_students: returns error result after max_retry exhausted", {
  local_mocked_bindings(
    .call_llm_tokens = function(...) "not valid json",
    .package = "Robjgrader"
  )
  res <- validate_text_students("Q", base_rubric, c(s1 = "answer"),
                                 api_key = "k", max_retry = 2L)
  expect_true(is.na(res$s1$overall))
  expect_true(is.na(res$s1$score))
  expect_match(res$s1$checks[[1L]]$message, "invalid response")
})

test_that("validate_text_students: max_retry=2 → at most 2 API calls per student", {
  call_count <- 0L
  local_mocked_bindings(
    .call_llm_tokens = function(...) { call_count <<- call_count + 1L; "bad json" },
    .package = "Robjgrader"
  )
  validate_text_students("Q", base_rubric, c(s1 = "a", s2 = "b"),
                          api_key = "k", max_retry = 2L)
  expect_lte(call_count, 2L)
})

test_that("validate_text_students: valid student resolved early, only failing retried", {
  call_n <- 0L
  local_mocked_bindings(
    .call_llm_tokens = function(...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) {
        # s1 valid, s2 invalid (missing pass)
        items <- list(
          list(student_id = "s1", pass = TRUE, score = 0.9,
               criteria = list(list(name = "direction", pass = TRUE, message = "ok"))),
          list(student_id = "s2", score = 0.5,
               criteria = list(list(name = "direction", pass = TRUE, message = "ok")))
        )
        as.character(jsonlite::toJSON(items, auto_unbox = TRUE))
      } else {
        # second call: only s2
        students_json(student_response("s2", pass = FALSE, score = 0.2))
      }
    },
    .package = "Robjgrader"
  )
  res <- validate_text_students("Q", base_rubric, c(s1 = "a", s2 = "b"),
                                 api_key = "k", max_retry = 3L)
  expect_true(res$s1$overall)
  expect_false(res$s2$overall)
  expect_equal(call_n, 2L)
})

test_that("validate_text_students: score=NULL defaults to 1 when pass=TRUE", {
  items <- list(list(student_id = "s1", pass = TRUE,
                     criteria = list(list(name = "direction", pass = TRUE, message = "ok"))))
  local_mocked_bindings(
    .call_llm_tokens = function(...)
      as.character(jsonlite::toJSON(items, auto_unbox = TRUE)),
    .package = "Robjgrader"
  )
  res <- validate_text_students("Q", base_rubric, c(s1 = "answer"), api_key = "k")
  expect_equal(res$s1$score, 1)
})

test_that("validate_text_students: score=NULL defaults to 0 when pass=FALSE", {
  items <- list(list(student_id = "s1", pass = FALSE,
                     criteria = list(list(name = "direction", pass = FALSE, message = "bad"))))
  local_mocked_bindings(
    .call_llm_tokens = function(...)
      as.character(jsonlite::toJSON(items, auto_unbox = TRUE)),
    .package = "Robjgrader"
  )
  res <- validate_text_students("Q", base_rubric, c(s1 = "answer"), api_key = "k")
  expect_equal(res$s1$score, 0)
})
