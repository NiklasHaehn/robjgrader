test_that("record_script() errors on missing file", {
  expect_error(record_script("nonexistent_file.R"), "File not found")
})

test_that("record_script() errors when recording is already active", {
  .recorder_env$active <- TRUE
  on.exit(.recorder_env$active <- FALSE, add = TRUE)
  tmp <- tempfile(fileext = ".R")
  writeLines("x <- 1", tmp)
  expect_error(record_script(tmp), "already active")
})

test_that("record_script() returns robjgrader_records", {
  tmp <- tempfile(fileext = ".R")
  writeLines("df1 <- data.frame(a = 1:3)", tmp)
  recs <- record_script(tmp)
  expect_s3_class(recs, "robjgrader_records")
})

test_that("record_script() captures a data frame assignment", {
  tmp <- tempfile(fileext = ".R")
  writeLines("df1 <- data.frame(a = 1:3, b = letters[1:3])", tmp)
  recs <- record_script(tmp)
  expect_length(recs, 1L)
  expect_equal(recs[[1L]]$object_name, "df1")
  expect_equal(recs[[1L]]$object_type, "df")
  expect_equal(recs[[1L]]$event_type, "assignment")
})

test_that("record_script() captures multiple objects in script order", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "df1 <- data.frame(x = 1:5)",
    "m1  <- lm(x ~ 1, data = df1)"
  ), tmp)
  recs <- record_script(tmp)
  expect_length(recs, 2L)
  expect_equal(recs[[1L]]$object_type, "df")
  expect_equal(recs[[2L]]$object_type, "model")
})

test_that("record_script() respects record_types filtering", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "df1 <- data.frame(x = 1:5)",
    "m1  <- lm(x ~ 1, data = df1)"
  ), tmp)
  recs <- record_script(tmp, record_df = FALSE, record_model = TRUE)
  expect_length(recs, 1L)
  expect_equal(recs[[1L]]$object_type, "model")
})

test_that("record_script() captures visible return (no assignment)", {
  tmp <- tempfile(fileext = ".R")
  writeLines("data.frame(a = 1:3)", tmp)
  recs <- record_script(tmp)
  expect_length(recs, 1L)
  expect_equal(recs[[1L]]$event_type, "visible_return")
  expect_null(recs[[1L]]$object_name)
})

test_that("record_script() skips non-recordable objects", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "x <- 42",
    "y <- 'hello'",
    "df1 <- data.frame(a = 1)"
  ), tmp)
  recs <- record_script(tmp)
  expect_length(recs, 1L)
  expect_equal(recs[[1L]]$object_name, "df1")
})

test_that("record_script() recovers from expression errors with stop_on_error = FALSE", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "df1 <- data.frame(a = 1:3)",
    "stop('deliberate error')",
    "df2 <- data.frame(b = 4:6)"
  ), tmp)
  expect_warning(
    recs <- record_script(tmp, stop_on_error = FALSE),
    "deliberate error"
  )
  expect_length(recs, 2L)
  expect_equal(recs[[1L]]$object_name, "df1")
  expect_equal(recs[[2L]]$object_name, "df2")
})

test_that("record_script() re-throws with stop_on_error = TRUE", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "df1 <- data.frame(a = 1:3)",
    "stop('deliberate error')",
    "df2 <- data.frame(b = 4:6)"
  ), tmp)
  expect_error(record_script(tmp, stop_on_error = TRUE), "deliberate error")
})

test_that("record_script() leaves .recorder_env$active = FALSE after completion", {
  tmp <- tempfile(fileext = ".R")
  writeLines("df1 <- data.frame(a = 1)", tmp)
  record_script(tmp)
  expect_false(.recorder_env$active)
})

test_that("record_script() leaves .recorder_env$active = FALSE after error", {
  tmp <- tempfile(fileext = ".R")
  writeLines("stop('boom')", tmp)
  suppressWarnings(record_script(tmp, stop_on_error = FALSE))
  expect_false(.recorder_env$active)
})

test_that("record_script() evaluates into envir (default .GlobalEnv)", {
  tmp <- tempfile(fileext = ".R")
  writeLines("rs_test_global_var_ <- 999L", tmp)
  on.exit({
    if (exists("rs_test_global_var_", envir = .GlobalEnv, inherits = FALSE))
      rm("rs_test_global_var_", envir = .GlobalEnv)
  }, add = TRUE)
  record_script(tmp)
  expect_true(exists("rs_test_global_var_", envir = .GlobalEnv, inherits = FALSE))
  expect_equal(get("rs_test_global_var_", envir = .GlobalEnv), 999L)
})

test_that("record_script() evaluates into custom envir", {
  tmp <- tempfile(fileext = ".R")
  writeLines("rs_test_custom_df_ <- data.frame(x = 1)", tmp)
  e <- new.env(parent = .GlobalEnv)
  record_script(tmp, envir = e)
  expect_true(exists("rs_test_custom_df_", envir = e, inherits = FALSE))
  expect_false(exists("rs_test_custom_df_", envir = .GlobalEnv, inherits = FALSE))
})

test_that("record_script() captures ggplot objects", {
  skip_if_not_installed("ggplot2")
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "library(ggplot2)",
    "p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()"
  ), tmp)
  recs <- record_script(tmp)
  types <- vapply(recs, `[[`, character(1), "object_type")
  expect_true("ggplot" %in% types)
})

# ---- P1 table capture: end-to-end via record_script() -----------------------

test_that("record_script() captures table returned invisibly (Mode A)", {
  tmp <- tempfile(fileext = ".R")
  # invisible() wraps the return value so visible = FALSE, no assignment
  writeLines(c(
    "fake_tbl_a <- structure(list(), class = c('gt_tbl', 'list'))",
    "invisible(fake_tbl_a)"
  ), tmp)
  recs <- record_script(tmp)
  types  <- vapply(recs, `[[`, character(1L), "object_type")
  etypes <- vapply(recs, `[[`, character(1L), "event_type")
  expect_true("table" %in% types)
  expect_true("invisible_return" %in% etypes)
})

test_that("record_script() captures table from print() with inline call arg (Mode B)", {
  e <- new.env(parent = .GlobalEnv)
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "fake_tbl_b <- structure(list(), class = c('gt_tbl', 'list'))",
    "print(identity(fake_tbl_b))"
  ), tmp)
  recs <- record_script(tmp, envir = e)
  types  <- vapply(recs, `[[`, character(1L), "object_type")
  etypes <- vapply(recs, `[[`, character(1L), "event_type")
  expect_true("table" %in% types)
  expect_true("print_call" %in% etypes)
})

test_that("record_script() captures gt table via invisible return (Mode A, real gt)", {
  skip_if_not_installed("gt")
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "library(gt)",
    "invisible(gt::gt(mtcars[1:3, 1:3]))"
  ), tmp)
  recs <- record_script(tmp)
  types  <- vapply(recs, `[[`, character(1L), "object_type")
  etypes <- vapply(recs, `[[`, character(1L), "event_type")
  expect_true("table" %in% types)
  expect_true("invisible_return" %in% etypes)
})

test_that("record_script() captures gt table from print() with inline call (Mode B, real gt)", {
  skip_if_not_installed("gt")
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "library(gt)",
    "print(gt::gt(mtcars[1:3, 1:3]))"
  ), tmp)
  recs <- record_script(tmp)
  types  <- vapply(recs, `[[`, character(1L), "object_type")
  etypes <- vapply(recs, `[[`, character(1L), "event_type")
  expect_true("table" %in% types)
  expect_true("print_call" %in% etypes)
})

test_that("record_script() captures table from pipe-print (gt |> print(), Mode B)", {
  skip_if_not_installed("gt")
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "library(gt)",
    "gt::gt(mtcars[1:3, 1:3]) |> print()"
  ), tmp)
  recs <- record_script(tmp)
  types <- vapply(recs, `[[`, character(1L), "object_type")
  expect_true("table" %in% types)
})


# ---- source_student_file() integration ---------------------------------------

test_that("source_student_file() uses record_script() and sets student_file attr", {
  dir <- tempfile()
  dir.create(dir)
  tmp <- file.path(dir, "student.R")
  writeLines("df_ssf_test <- data.frame(a = 1:5)", tmp)
  old_wd <- setwd(dir)
  on.exit({
    setwd(old_wd)
    unlink(dir, recursive = TRUE)
    if (exists("df_ssf_test", envir = .GlobalEnv, inherits = FALSE))
      rm("df_ssf_test", envir = .GlobalEnv)
  }, add = TRUE)
  recs <- source_student_file(autograder_name = "autograde.R",
                              record_types = c("df", "model"))
  expect_s3_class(recs, "robjgrader_records")
  expect_true(grepl("student\\.R$", attr(recs, "student_file")))
  expect_length(recs, 1L)
  expect_equal(recs[[1L]]$object_type, "df")
})
