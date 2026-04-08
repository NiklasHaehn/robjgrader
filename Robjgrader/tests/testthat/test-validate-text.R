library(testthat)

# ==============================================================================
# Helpers
# ==============================================================================

# Build a minimal valid LLM JSON response
llm_json <- function(
  pass     = TRUE,
  score    = 0.8,
  criteria = list(list(name = "direction", pass = TRUE, message = "correct")),
  feedback = NULL
) {
  obj <- list(pass = pass, score = score, criteria = criteria)
  if (!is.null(feedback)) obj$feedback <- feedback
  jsonlite::toJSON(obj, auto_unbox = TRUE)
}

# Write temp text file and return its path
tmp_txt <- function(content, ext = "txt") {
  p <- tempfile(fileext = paste0(".", ext))
  writeLines(content, p)
  p
}


# ==============================================================================
# read_student_text()
# ==============================================================================

test_that("read_student_text: reads .txt file into single string", {
  p   <- tmp_txt(c("Line one", "Line two"))
  out <- read_student_text(p)
  expect_type(out, "character")
  expect_length(out, 1L)
  expect_match(out, "Line one")
  expect_match(out, "Line two")
})

test_that("read_student_text: reads .md file", {
  p   <- tmp_txt(c("# Heading", "body text"), ext = "md")
  out <- read_student_text(p)
  expect_match(out, "Heading")
  expect_match(out, "body text")
})

test_that("read_student_text: errors on missing file", {
  expect_error(read_student_text("/nonexistent/file.txt"), "not found")
})

test_that("read_student_text: errors on unsupported extension", {
  p <- tempfile(fileext = ".docx")
  writeLines("dummy", p)
  expect_error(read_student_text(p), "Unsupported")
})


# ==============================================================================
# split_student_text()
# ==============================================================================

test_that("split_student_text: splits on markdown headings", {
  txt <- "# Q1: Interpretation\nsome answer\n\n# Q2: Fixed Effects\nother answer"
  sections <- split_student_text(txt)
  expect_length(sections, 2L)
  expect_match(names(sections)[1L], "Q1")
  expect_match(names(sections)[2L], "Q2")
  expect_match(sections[[1L]], "some answer")
  expect_match(sections[[2L]], "other answer")
})

test_that("split_student_text: splits on numbered prefixes in plain text", {
  txt <- "Q1: First answer\nsome text\n\nQ2: Second answer\nmore text"
  sections <- split_student_text(txt, format = "plain")
  expect_length(sections, 2L)
  expect_match(sections[[1L]], "some text")
})

test_that("split_student_text: falls back to blank-line split", {
  txt <- "First chunk\n\nSecond chunk\n\nThird chunk"
  sections <- split_student_text(txt, format = "plain")
  expect_gte(length(sections), 2L)
})

test_that("split_student_text: n_sections limits blank-line fallback", {
  txt <- "A\n\nB\n\nC\n\nD"
  sections <- split_student_text(txt, n_sections = 2L, format = "plain")
  expect_lte(length(sections), 2L)
})

test_that("split_student_text: returns full text as single section when no boundaries found", {
  txt      <- "Just a single block of text with no headings or prefixes."
  sections <- split_student_text(txt, format = "plain")
  expect_length(sections, 1L)
  expect_match(sections[[1L]], "single block")
})

test_that("split_student_text: auto-detects markdown format", {
  txt      <- "# Section\nbody"
  sections <- split_student_text(txt)            # format = "auto"
  expect_length(sections, 1L)
  expect_match(names(sections), "Section")
})


# ==============================================================================
# .extract_section() (internal)
# ==============================================================================

test_that(".extract_section: retrieves by integer index", {
  secs <- c("Q1" = "answer one", "Q2" = "answer two")
  expect_equal(Robjgrader:::.extract_section(secs, 2L, "test"), "answer two")
})

test_that(".extract_section: retrieves by exact name match (case-insensitive)", {
  secs <- c("Q1: Interpretation" = "answer one", "Q2: FE" = "answer two")
  expect_equal(Robjgrader:::.extract_section(secs, "q1: interpretation", "test"),
               "answer one")
})

test_that(".extract_section: retrieves by partial name match", {
  secs <- c("Q1: Interpretation" = "the interpretation")
  expect_equal(Robjgrader:::.extract_section(secs, "q1", "test"), "the interpretation")
})

test_that(".extract_section: errors when index out of range", {
  secs <- c("Q1" = "a")
  expect_error(Robjgrader:::.extract_section(secs, 5L, "test"), "out of range")
})

test_that(".extract_section: errors when name not found", {
  secs <- c("Q1" = "a", "Q2" = "b")
  expect_error(Robjgrader:::.extract_section(secs, "Q9", "test"), "not found")
})


# ==============================================================================
# .parse_llm_response() (internal)
# ==============================================================================

test_that(".parse_llm_response: returns valid on well-formed JSON", {
  raw    <- llm_json()
  result <- Robjgrader:::.parse_llm_response(raw)
  expect_true(result$valid)
  expect_true(result$data$pass)
  expect_equal(result$data$score, 0.8)
})

test_that(".parse_llm_response: strips markdown code fences", {
  raw    <- paste0("```json\n", llm_json(), "\n```")
  result <- Robjgrader:::.parse_llm_response(raw)
  expect_true(result$valid)
})

test_that(".parse_llm_response: invalid on non-JSON", {
  result <- Robjgrader:::.parse_llm_response("this is not JSON")
  expect_false(result$valid)
  expect_match(result$reason, "JSON")
})

test_that(".parse_llm_response: invalid when 'pass' missing", {
  raw    <- '{"score": 0.5, "criteria": [{"name": "x", "pass": true, "message": "ok"}]}'
  result <- Robjgrader:::.parse_llm_response(raw)
  expect_false(result$valid)
  expect_match(result$reason, "pass")
})

test_that(".parse_llm_response: invalid when criteria empty", {
  raw    <- '{"pass": true, "score": 0.5, "criteria": []}'
  result <- Robjgrader:::.parse_llm_response(raw)
  expect_false(result$valid)
  expect_match(result$reason, "criteria")
})

test_that(".parse_llm_response: invalid when score out of range", {
  raw    <- '{"pass": true, "score": 1.5, "criteria": [{"name": "x", "pass": true, "message": "ok"}]}'
  result <- Robjgrader:::.parse_llm_response(raw)
  expect_false(result$valid)
  expect_match(result$reason, "score")
})

test_that(".parse_llm_response: valid when score field absent", {
  raw    <- '{"pass": false, "criteria": [{"name": "x", "pass": false, "message": "wrong"}]}'
  result <- Robjgrader:::.parse_llm_response(raw)
  expect_true(result$valid)
})

test_that(".parse_llm_response: invalid when criteria name not in rubric", {
  raw <- '{"pass": true, "score": 0.8, "criteria": [{"name": "invented", "pass": true, "message": "ok"}]}'
  result <- Robjgrader:::.parse_llm_response(raw, rubric_names = c("direction", "magnitude"))
  expect_false(result$valid)
  expect_match(result$reason, "invented")
  expect_match(result$reason, "rubric")
})

test_that(".parse_llm_response: valid when criteria names match rubric (case-insensitive)", {
  raw <- '{"pass": true, "score": 0.9, "criteria": [{"name": "Direction", "pass": true, "message": "ok"}]}'
  result <- Robjgrader:::.parse_llm_response(raw, rubric_names = c("direction", "magnitude"))
  expect_true(result$valid)
})

test_that(".parse_llm_response: invalid when pass=TRUE but score < 0.5", {
  raw <- '{"pass": true, "score": 0.2, "criteria": [{"name": "x", "pass": true, "message": "ok"}]}'
  result <- Robjgrader:::.parse_llm_response(raw)
  expect_false(result$valid)
  expect_match(result$reason, "inconsistent")
})

test_that(".parse_llm_response: invalid when pass=FALSE but score >= 0.5", {
  raw <- '{"pass": false, "score": 0.7, "criteria": [{"name": "x", "pass": false, "message": "fail"}]}'
  result <- Robjgrader:::.parse_llm_response(raw)
  expect_false(result$valid)
  expect_match(result$reason, "inconsistent")
})

test_that(".parse_llm_response: valid when pass=TRUE and score >= 0.5", {
  raw <- '{"pass": true, "score": 0.8, "criteria": [{"name": "x", "pass": true, "message": "ok"}]}'
  result <- Robjgrader:::.parse_llm_response(raw)
  expect_true(result$valid)
})

test_that(".parse_llm_response: valid when pass=FALSE and score < 0.5", {
  raw <- '{"pass": false, "score": 0.3, "criteria": [{"name": "x", "pass": false, "message": "fail"}]}'
  result <- Robjgrader:::.parse_llm_response(raw)
  expect_true(result$valid)
})


# ==============================================================================
# validate_text() — with mocked LLM
# ==============================================================================

test_that("validate_text: returns robjgrader_result with overall TRUE on pass", {
  local_mocked_bindings(
    .call_llm = function(...) as.character(llm_json(pass = TRUE, score = 0.9)),
    .package  = "Robjgrader"
  )
  res <- validate_text(
    text     = "The coefficient is positive and significant.",
    question = "Interpret the coefficient.",
    rubric   = c(direction = "Identifies the sign."),
    api_key  = "test-key"
  )
  expect_s3_class(res, "robjgrader_result")
  expect_true(res$overall)
  expect_equal(res$score, 0.9)
})

test_that("validate_text: overall FALSE when LLM returns pass = FALSE", {
  local_mocked_bindings(
    .call_llm = function(...) as.character(llm_json(pass = FALSE, score = 0.2)),
    .package  = "Robjgrader"
  )
  res <- validate_text(
    text     = "I don't know.",
    question = "Interpret the coefficient.",
    rubric   = c(direction = "Identifies the sign."),
    api_key  = "test-key"
  )
  expect_false(res$overall)
  expect_equal(res$score, 0.2)
})

test_that("validate_text: feedback populated when feedback = TRUE", {
  resp <- llm_json(
    pass     = TRUE,
    score    = 1,
    feedback = "Good interpretation."
  )
  local_mocked_bindings(
    .call_llm = function(...) as.character(resp),
    .package  = "Robjgrader"
  )
  res <- validate_text(
    text     = "The coefficient is positive.",
    question = "Interpret.",
    rubric   = c(direction = "sign"),
    feedback = TRUE,
    api_key  = "test-key"
  )
  expect_equal(res$feedback, "Good interpretation.")
})

test_that("validate_text: feedback NULL when feedback = FALSE", {
  local_mocked_bindings(
    .call_llm = function(...) as.character(llm_json(pass = TRUE)),
    .package  = "Robjgrader"
  )
  res <- validate_text(
    text     = "positive",
    question = "q",
    rubric   = c(x = "y"),
    feedback = FALSE,
    api_key  = "test-key"
  )
  expect_null(res$feedback)
})

test_that("validate_text: returns NA overall after max retries on invalid JSON", {
  local_mocked_bindings(
    .call_llm = function(...) "not valid json at all",
    .package  = "Robjgrader"
  )
  res <- validate_text(
    text      = "answer",
    question  = "q",
    rubric    = c(x = "y"),
    max_retry = 2L,
    api_key   = "test-key"
  )
  expect_true(is.na(res$overall))
  expect_true(is.na(res$score))
})

test_that("validate_text: extracts section before grading when section given", {
  txt <- "# Q1\nanswer one\n\n# Q2\nanswer two"
  captured_text <- NULL

  local_mocked_bindings(
    .call_llm = function(messages, ...) {
      captured_text <<- messages[[2L]]$content
      as.character(llm_json())
    },
    .package = "Robjgrader"
  )
  validate_text(
    text       = txt,
    section    = "Q2",
    n_sections = 2L,
    question   = "q",
    rubric     = c(x = "y"),
    api_key    = "test-key"
  )
  expect_match(captured_text, "answer two")
  expect_false(grepl("answer one", captured_text))
})

test_that("validate_text: errors when both prompt and question supplied", {
  expect_error(
    validate_text(
      text     = "x",
      prompt   = "do something",
      question = "also this",
      rubric   = c(x = "y")
    ),
    "mutually exclusive"
  )
})

test_that("validate_text: errors when neither prompt nor question supplied", {
  expect_error(
    validate_text(text = "x"),
    "Mode A"
  )
})

test_that("validate_text: errors when api_key empty", {
  withr::with_envvar(
    c(GROQ_API_KEY = ""),
    withr::with_options(
      list(robjgrader.llm.api_key = NULL),
      expect_error(
        validate_text(
          text     = "x",
          question = "q",
          rubric   = c(x = "y"),
          api_key  = ""
        ),
        "API key"
      )
    )
  )
})


# ==============================================================================
# robjgrader_set_llm() — options integration
# ==============================================================================

test_that("robjgrader_set_llm: sets model option", {
  withr::with_options(list(robjgrader.llm.model = NULL), {
    robjgrader_set_llm(model = "gpt-4")
    expect_equal(getOption("robjgrader.llm.model"), "gpt-4")
  })
})

test_that("robjgrader_set_llm: provider groq sets all three options", {
  withr::with_envvar(c(GROQ_API_KEY = "test-key"), {
    withr::with_options(
      list(robjgrader.llm.model = NULL,
           robjgrader.llm.base_url = NULL,
           robjgrader.llm.api_key  = NULL), {
      robjgrader_set_llm(provider = "groq")
      expect_match(getOption("robjgrader.llm.model"),    "llama")
      expect_match(getOption("robjgrader.llm.base_url"), "groq\\.com")
      expect_equal(getOption("robjgrader.llm.api_key"),  "test-key")
    })
  })
})

test_that("robjgrader_set_llm: explicit model overrides provider default", {
  withr::with_options(list(robjgrader.llm.model = NULL), {
    robjgrader_set_llm(provider = "groq", model = "my-custom-model")
    expect_equal(getOption("robjgrader.llm.model"), "my-custom-model")
  })
})

test_that("validate_text: uses model from options when not passed explicitly", {
  local_mocked_bindings(
    .call_llm = function(messages, model, ...) {
      # Return the model name encoded in the response for inspection
      as.character(llm_json())
    },
    .package = "Robjgrader"
  )
  # Just verify it doesn't error when option is set
  withr::with_options(
    list(robjgrader.llm.model    = "test-model",
         robjgrader.llm.base_url = "https://api.groq.com/openai/v1",
         robjgrader.llm.api_key  = "test-key"), {
    res <- validate_text(
      text     = "answer",
      question = "q",
      rubric   = c(x = "y")
    )
    expect_s3_class(res, "robjgrader_result")
  })
})


# ==============================================================================
# validate_text() — match_section
# ==============================================================================

test_that("validate_text: match_section selects section by regex on heading", {
  txt <- "# Q1: Regression\nanswer one\n\n# Q2: Interpretation\nanswer two"
  captured_text <- NULL

  local_mocked_bindings(
    .call_llm = function(messages, ...) {
      captured_text <<- messages[[2L]]$content
      as.character(llm_json())
    },
    .package = "Robjgrader"
  )
  validate_text(
    text          = txt,
    match_section = list(heading = "interpret"),
    question      = "q",
    rubric        = c(x = "y"),
    api_key       = "test-key"
  )
  expect_match(captured_text, "answer two")
  expect_false(grepl("answer one", captured_text))
})

test_that("validate_text: match_section with min_words filters short sections", {
  txt <- "# Q1: Short\nok\n\n# Q2: Long\nThis is a much longer answer with many words here."
  captured_text <- NULL

  local_mocked_bindings(
    .call_llm = function(messages, ...) {
      captured_text <<- messages[[2L]]$content
      as.character(llm_json())
    },
    .package = "Robjgrader"
  )
  # Both headings contain no specific word; match_section matches Q2 (long enough)
  validate_text(
    text          = txt,
    match_section = list(heading = "Q[12]", min_words = 5L),
    question      = "q",
    rubric        = c(x = "y"),
    api_key       = "test-key"
  )
  expect_match(captured_text, "longer answer")
})

test_that("validate_text: match_section errors when no heading matches", {
  txt <- "# Q1\nanswer one\n\n# Q2\nanswer two"

  local_mocked_bindings(
    .call_llm = function(...) as.character(llm_json()),
    .package  = "Robjgrader"
  )
  expect_error(
    validate_text(
      text          = txt,
      match_section = list(heading = "nonexistent_xyz"),
      question      = "q",
      rubric        = c(x = "y"),
      api_key       = "test-key"
    ),
    "No section heading matches"
  )
})

test_that("validate_text: section and match_section are mutually exclusive", {
  expect_error(
    validate_text(
      text          = "# Q1\nsome text",
      section       = "Q1",
      match_section = list(heading = "Q1"),
      question      = "q",
      rubric        = c(x = "y")
    ),
    "mutually exclusive"
  )
})


# ==============================================================================
# validate_text() — list-based reference (Markdown section)
# ==============================================================================

test_that("validate_text: reference as list reads Markdown section", {
  ref_content  <- "# Q1: Interpretation\nThe coefficient is positive.\n\n# Q2: FE\nFixed effects control for..."
  ref_path     <- tmp_txt(ref_content, ext = "md")
  captured_sys <- NULL

  local_mocked_bindings(
    .call_llm = function(messages, ...) {
      captured_sys <<- messages[[1L]]$content
      as.character(llm_json())
    },
    .package = "Robjgrader"
  )
  validate_text(
    text      = "The sign is positive.",
    reference = list(path = ref_path, section = "Q1"),
    question  = "q",
    rubric    = c(x = "y"),
    api_key   = "test-key"
  )
  expect_match(captured_sys, "coefficient is positive")
  expect_false(grepl("Fixed effects", captured_sys))
})

test_that("validate_text: reference as file path reads whole file", {
  ref_path     <- tmp_txt("Model answer: positive coefficient.", ext = "txt")
  captured_sys <- NULL

  local_mocked_bindings(
    .call_llm = function(messages, ...) {
      captured_sys <<- messages[[1L]]$content
      as.character(llm_json())
    },
    .package = "Robjgrader"
  )
  validate_text(
    text      = "The sign is positive.",
    reference = ref_path,
    question  = "q",
    rubric    = c(x = "y"),
    api_key   = "test-key"
  )
  expect_match(captured_sys, "Model answer")
})


# ==============================================================================
# validate_text() — Mode A with reference injection
# ==============================================================================

test_that("validate_text: Mode A injects reference into system prompt", {
  captured_sys <- NULL

  local_mocked_bindings(
    .call_llm = function(messages, ...) {
      captured_sys <<- messages[[1L]]$content
      as.character(llm_json())
    },
    .package = "Robjgrader"
  )
  validate_text(
    text      = "answer",
    prompt    = "Grade the following answer strictly.",
    reference = "The correct interpretation is positive.",
    api_key   = "test-key"
  )
  expect_match(captured_sys, "Grade the following answer strictly")
  expect_match(captured_sys, "REFERENCE ANSWER")
  expect_match(captured_sys, "positive")
})

test_that("validate_text: Mode A without reference uses prompt unchanged", {
  captured_sys <- NULL

  local_mocked_bindings(
    .call_llm = function(messages, ...) {
      captured_sys <<- messages[[1L]]$content
      as.character(llm_json())
    },
    .package = "Robjgrader"
  )
  validate_text(
    text    = "answer",
    prompt  = "Grade this.",
    api_key = "test-key"
  )
  expect_equal(captured_sys, "Grade this.")
})
