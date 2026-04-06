library(testthat)

# ==============================================================================
# flag_submission()
# ==============================================================================

# Helper: run code in a temporary directory containing specific files
with_tmp_files <- function(filenames, code) {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  for (f in filenames) writeLines("", file.path(dir, f))
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  force(code)
}

test_that("flag_submission: PASS when no matching file present", {
  with_tmp_files(character(0L), {
    res <- flag_submission("do")
    expect_s3_class(res, "robjgrader_result")
    expect_true(res$overall)
  })
})

test_that("flag_submission: FAIL when matching file found", {
  with_tmp_files("student_script.do", {
    res <- flag_submission("do")
    expect_false(res$overall)
    expect_match(res$checks$format$message, "student_script\\.do")
  })
})

test_that("flag_submission: custom message used when provided", {
  with_tmp_files("analysis.do", {
    res <- flag_submission("do", message = "Please submit an R script.")
    expect_false(res$overall)
    expect_equal(res$checks$format$message, "Please submit an R script.")
  })
})

test_that("flag_submission: extension without leading dot works", {
  with_tmp_files("data.dta", {
    res <- flag_submission("dta")
    expect_false(res$overall)
  })
})

test_that("flag_submission: extension with leading dot works", {
  with_tmp_files("data.dta", {
    res <- flag_submission(".dta")
    expect_false(res$overall)
  })
})

test_that("flag_submission: multiple extensions checked", {
  with_tmp_files(c("analysis.py"), {
    res_do <- flag_submission(c("do", "dta"))
    res_py <- flag_submission(c("do", "py"))
    expect_true(res_do$overall)
    expect_false(res_py$overall)
  })
})

test_that("flag_submission: PASS when only .R files present", {
  with_tmp_files("student.R", {
    res <- flag_submission(c("do", "dta", "py"))
    expect_true(res$overall)
  })
})

test_that("flag_submission: check name field propagated", {
  with_tmp_files(character(0L), {
    res <- flag_submission("do", name = "format_check")
    expect_equal(res$object_name, "format_check")
  })
})


# ==============================================================================
# run_autograder() — abort_on_fail
# ==============================================================================

# Helper: make a minimal passing robjgrader_result
pass_result <- function(name = "x") {
  structure(
    list(object_name = name, object_type = "df", object_class = "data.frame",
         overall = TRUE, checks = list(), score = NULL, feedback = NULL),
    class = "robjgrader_result"
  )
}

fail_result <- function(name = "x", msg = "check failed") {
  structure(
    list(object_name = name, object_type = "df", object_class = "data.frame",
         overall = FALSE,
         checks = list(c1 = list(check = "c1", pass = FALSE,
                                 expected = TRUE, observed = FALSE,
                                 message = msg)),
         score = NULL, feedback = NULL),
    class = "robjgrader_result"
  )
}

test_that("run_autograder: abort_on_fail skips subsequent tests on failure", {
  tcs <- list(
    list(name = "Q1", result = fail_result(), max_score = 5, abort_on_fail = TRUE),
    list(name = "Q2", result = pass_result(), max_score = 10),
    list(name = "Q3", result = pass_result(), max_score = 5)
  )
  out <- run_autograder(tcs, json_path = tempfile(fileext = ".json"), verbose = FALSE)
  expect_equal(out$tests[[1]]$score, 0)
  expect_equal(out$tests[[2]]$score, 0)
  expect_match(out$tests[[2]]$output, "Skipped")
  expect_equal(out$tests[[3]]$score, 0)
  expect_match(out$tests[[3]]$output, "Skipped")
})

test_that("run_autograder: abort_on_fail does NOT skip when test passes", {
  tcs <- list(
    list(name = "Q1", result = pass_result(), max_score = 5, abort_on_fail = TRUE),
    list(name = "Q2", result = pass_result(), max_score = 10)
  )
  out <- run_autograder(tcs, json_path = tempfile(fileext = ".json"), verbose = FALSE)
  expect_equal(out$tests[[1]]$score, 5)
  expect_equal(out$tests[[2]]$score, 10)
})

test_that("run_autograder: without abort_on_fail all tests run independently", {
  tcs <- list(
    list(name = "Q1", result = fail_result(), max_score = 5),
    list(name = "Q2", result = pass_result(), max_score = 10)
  )
  out <- run_autograder(tcs, json_path = tempfile(fileext = ".json"), verbose = FALSE)
  expect_equal(out$tests[[1]]$score, 0)
  expect_equal(out$tests[[2]]$score, 10)
})

test_that("run_autograder: abort_on_fail on last test has no effect", {
  tcs <- list(
    list(name = "Q1", result = pass_result(), max_score = 5),
    list(name = "Q2", result = fail_result(), max_score = 10, abort_on_fail = TRUE)
  )
  out <- run_autograder(tcs, json_path = tempfile(fileext = ".json"), verbose = FALSE)
  expect_equal(out$tests[[1]]$score, 5)
  expect_equal(out$tests[[2]]$score, 0)
  expect_equal(length(out$tests), 2L)
})

test_that("run_autograder + flag_submission: skips grading on wrong format", {
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  writeLines("", file.path(dir, "script.do"))
  old <- setwd(dir); on.exit(setwd(old), add = TRUE)

  flag_res <- flag_submission("do", message = "Wrong format.")
  tcs <- list(
    list(name = "Format", result = flag_res, max_score = 0, abort_on_fail = TRUE),
    list(name = "Q1",     result = pass_result(), max_score = 10),
    list(name = "Q2",     result = pass_result(), max_score = 5)
  )
  out <- run_autograder(tcs, json_path = tempfile(fileext = ".json"), verbose = FALSE)
  expect_equal(out$tests[[1]]$score, 0)
  expect_match(out$tests[[1]]$output, "Wrong format")
  expect_match(out$tests[[2]]$output, "Skipped")
  expect_match(out$tests[[3]]$output, "Skipped")
})
