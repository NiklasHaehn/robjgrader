library(testthat)

# ==============================================================================
# check_spec() constructor
# ==============================================================================

test_that("check_spec: stores value and weight", {
  s <- check_spec("mpg", weight = 3)
  expect_equal(s$value, "mpg")
  expect_equal(s$weight, 3)
  expect_s3_class(s, "robjgrader_check_spec")
})

test_that("check_spec: weight = NULL by default", {
  s <- check_spec(42L)
  expect_null(s$weight)
})

test_that("check_spec: rejects non-positive weight", {
  expect_error(check_spec("x", weight = 0),   "positive")
  expect_error(check_spec("x", weight = -1),  "positive")
  expect_error(check_spec("x", weight = "a"), "positive")
})

test_that(".parse_check_spec: plain value -> weight NULL", {
  sp <- .parse_check_spec(42L)
  expect_equal(sp$value,  42L)
  expect_null(sp$weight)
})

test_that(".parse_check_spec: check_spec object passes through", {
  sp <- .parse_check_spec(check_spec("x", weight = 2))
  expect_equal(sp$value,  "x")
  expect_equal(sp$weight, 2)
})

# ==============================================================================
# .make_result() score computation
# ==============================================================================

test_that(".make_result: score NULL when no weights", {
  checks <- list(
    a = .make_check("a", TRUE,  1, 1, "ok"),
    b = .make_check("b", FALSE, 2, 3, "fail")
  )
  res <- .make_result("x", "df", "data.frame", checks)
  expect_null(res$score)
  expect_false(res$overall)
})

test_that(".make_result: score computed when weights present", {
  checks <- list(
    a = .make_check("a", TRUE,  1, 1, "ok",   weight = 3),
    b = .make_check("b", FALSE, 2, 3, "fail", weight = 1)
  )
  res <- .make_result("x", "df", "data.frame", checks)
  expect_equal(res$score, 3 / 4)
  expect_false(res$overall)
})

test_that(".make_result: score 1.0 when all weighted checks pass", {
  checks <- list(
    a = .make_check("a", TRUE, 1, 1, "ok", weight = 2),
    b = .make_check("b", TRUE, 2, 2, "ok", weight = 3)
  )
  res <- .make_result("x", "df", "data.frame", checks)
  expect_equal(res$score, 1.0)
  expect_true(res$overall)
})

test_that(".make_result: mixed weighted/unweighted — unweighted treated as 1", {
  checks <- list(
    a = .make_check("a", TRUE,  1, 1, "ok",   weight = 4),
    b = .make_check("b", FALSE, 2, 3, "fail")   # no weight → 1
  )
  res <- .make_result("x", "df", "data.frame", checks)
  # weighted sum passed = 4, total = 5
  expect_equal(res$score, 4 / 5)
})

# ==============================================================================
# validate_df: weighted checks → score propagates
# ==============================================================================

test_that("validate_df: weighted nrow/ncol produce score", {
  df   <- data.frame(x = 1:5, y = letters[1:5])
  recs <- make_records(list(obj = df, name = "df1"))

  res <- validate(recs, name = "df1",
                  checks = list(
                    nrow = check_spec(5L, weight = 2),
                    ncol = check_spec(9L, weight = 1)   # FAIL
                  ))
  expect_false(res$overall)
  expect_equal(res$score, 2 / 3)
  expect_true(res$checks$nrow$pass)
  expect_false(res$checks$ncol$pass)
  expect_equal(res$checks$nrow$weight, 2)
  expect_equal(res$checks$ncol$weight, 1)
})

test_that("validate_df: all weighted checks pass → score 1", {
  df   <- data.frame(x = 1:5, y = letters[1:5])
  recs <- make_records(list(obj = df, name = "df1"))

  res <- validate(recs, name = "df1",
                  checks = list(
                    nrow = check_spec(5L, weight = 3),
                    ncol = check_spec(2L, weight = 1)
                  ))
  expect_true(res$overall)
  expect_equal(res$score, 1.0)
})

test_that("validate_df: no weights → score NULL", {
  df   <- data.frame(x = 1:5)
  recs <- make_records(list(obj = df, name = "df1"))
  res  <- validate(recs, name = "df1", checks = list(nrow = 5L))
  expect_null(res$score)
  expect_true(res$overall)
})

# ==============================================================================
# validate_model: weighted checks
# ==============================================================================

test_that("validate_model: weighted outcome/nobs produce score", {
  m    <- lm(mpg ~ wt, data = mtcars)
  recs <- make_records(list(obj = m, name = "m"))

  res <- validate(recs, name = "m",
                  checks = list(
                    outcome = check_spec("mpg", weight = 3),
                    nobs    = check_spec(99L,   weight = 1)  # FAIL
                  ))
  expect_false(res$overall)
  expect_equal(res$score, 3 / 4)
})

test_that("validate_model: coef_sign with weight", {
  m    <- lm(mpg ~ wt + hp, data = mtcars)
  recs <- make_records(list(obj = m, name = "m"))

  # wt is negative, hp is negative
  res <- validate(recs, name = "m",
                  checks = list(
                    coef_sign = check_spec(c(wt = "negative", hp = "negative"), weight = 2)
                  ))
  expect_true(res$checks$coef_sign.wt$pass)
  expect_true(res$checks$coef_sign.hp$pass)
  expect_equal(res$checks$coef_sign.wt$weight, 2)
  expect_equal(res$score, 1.0)
})

# ==============================================================================
# validate (ggplot): weighted aes checks
# ==============================================================================

test_that("validate_plot: weighted aes_x produces score", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  recs <- make_records(list(obj = p, name = "p1"))

  res <- validate(recs, name = "p1",
                  checks = list(
                    aes_x = check_spec("wt",  weight = 2),
                    aes_y = check_spec("cyl", weight = 1)  # FAIL
                  ))
  expect_false(res$overall)
  expect_equal(res$score, 2 / 3)
})

# ==============================================================================
# validate_stata: weighted contains
# ==============================================================================

test_that("validate_do: weighted contains produce score", {
  code <- "reg y x, robust"
  res  <- validate_do(code,
                      checks = list(
                        contains = check_spec(c("robust", "cluster"), weight = 1)
                      ))
  # "robust" found, "cluster" not → 1/2
  expect_false(res$overall)
  expect_equal(res$score, 0.5)
})

test_that("validate_do: weighted command all pass → score 1", {
  code <- "reg y x, robust\nlogit y x"
  res  <- validate_do(code,
                      checks = list(
                        command = check_spec(c("reg", "logit"), weight = 3)
                      ))
  expect_true(res$overall)
  expect_equal(res$score, 1.0)
})

# ==============================================================================
# run_autograder: score field drives partial credit
# ==============================================================================

test_that("run_autograder: partial score from weighted checks", {
  df   <- data.frame(x = 1:5, y = 1:5)
  recs <- make_records(list(obj = df, name = "df1"))

  res <- validate(recs, name = "df1",
                  checks = list(
                    nrow = check_spec(5L, weight = 3),
                    ncol = check_spec(9L, weight = 1)   # FAIL
                  ))

  test_cases <- list(list(name = "Q1", result = res, max_score = 8))
  out <- run_autograder(test_cases, json_path = tempfile(), verbose = FALSE)

  # 3/4 of 8 = 6
  expect_equal(out$tests[[1L]]$score, 6)
  expect_equal(out$tests[[1L]]$max_score, 8)
})

test_that("run_autograder: full score when all weighted checks pass", {
  df   <- data.frame(x = 1:5, y = 1:5)
  recs <- make_records(list(obj = df, name = "df1"))

  res <- validate(recs, name = "df1",
                  checks = list(
                    nrow = check_spec(5L, weight = 2),
                    ncol = check_spec(2L, weight = 3)
                  ))

  test_cases <- list(list(name = "Q1", result = res, max_score = 10))
  out <- run_autograder(test_cases, json_path = tempfile(), verbose = FALSE)

  expect_equal(out$tests[[1L]]$score, 10)
})
