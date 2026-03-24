library(testthat)

# Recorder internals are accessible via devtools::load_all() in test mode.
# addTaskCallback fires only at the REPL top level, so we call
# .recorder_callback() directly to simulate what would happen interactively.

# ---- internal helper predicates ----------------------------------------------

test_that(".is_print_call() detects print/show calls", {
  expect_true(.is_print_call(quote(print(x))))
  expect_true(.is_print_call(quote(show(x))))
  expect_false(.is_print_call(quote(x <- 1)))
  expect_false(.is_print_call(quote(x + y)))
  expect_false(.is_print_call(quote(1)))
})

test_that(".extract_assign_name() extracts LHS of <- / = / <<-", {
  expect_equal(.extract_assign_name(quote(x <- 1)), "x")
  # `=` as assignment — must construct the call explicitly since quote(y = ...) is invalid syntax
  expr_eq <- call("=", as.name("y"), call("lm", quote(mpg ~ wt), quote(mtcars)))
  expect_equal(.extract_assign_name(expr_eq), "y")
  expect_equal(.extract_assign_name(quote(z <<- TRUE)), "z")
  expect_null(.extract_assign_name(quote(lm(mpg ~ wt, mtcars))))
  expect_null(.extract_assign_name(quote(print(x))))
  expect_null(.extract_assign_name(1L))
})

test_that(".classify_object() classifies data frames and tibbles as 'df'", {
  cfg <- list(df = TRUE, ggplot = TRUE, model = TRUE, table = TRUE)
  expect_equal(.classify_object(data.frame(x = 1), cfg), "df")
})

test_that(".classify_object() classifies lm/glm as 'model'", {
  cfg <- list(df = TRUE, ggplot = TRUE, model = TRUE, table = TRUE)
  expect_equal(.classify_object(lm(mpg ~ wt, mtcars), cfg), "model")
  expect_equal(.classify_object(glm(am ~ wt, mtcars, family = binomial), cfg), "model")
})

test_that(".classify_object() classifies ggplot as 'ggplot'", {
  skip_if_not_installed("ggplot2")
  cfg <- list(df = TRUE, ggplot = TRUE, model = TRUE, table = TRUE)
  expect_equal(.classify_object(ggplot2::ggplot(), cfg), "ggplot")
})

test_that(".classify_object() returns NULL for unknown types", {
  cfg <- list(df = TRUE, ggplot = TRUE, model = TRUE, table = TRUE)
  expect_null(.classify_object(list(a = 1), cfg))
  expect_null(.classify_object(42L, cfg))
  expect_null(.classify_object("foo", cfg))
})

test_that(".classify_object() respects config flags", {
  cfg_no_df <- list(df = FALSE, ggplot = TRUE, model = TRUE, table = TRUE)
  expect_null(.classify_object(data.frame(x = 1), cfg_no_df))

  cfg_no_model <- list(df = TRUE, ggplot = TRUE, model = FALSE, table = TRUE)
  expect_null(.classify_object(lm(mpg ~ wt, mtcars), cfg_no_model))
})

# ---- record_start / record_stop state management ----------------------------

test_that("record_start() activates recording and resets state", {
  record_start()
  on.exit(if (.recorder_env$active) suppressMessages(record_stop()))
  expect_true(.recorder_env$active)
  expect_equal(.recorder_env$counter, 0L)
  expect_length(.recorder_env$records, 0L)
  suppressMessages(record_stop())
})

test_that("record_start() warns if already active", {
  record_start()
  on.exit(if (.recorder_env$active) suppressMessages(record_stop()))
  expect_warning(record_start(), "already active")
  suppressMessages(record_stop())
})

test_that("record_stop() deactivates recording", {
  record_start()
  suppressMessages(record_stop())
  expect_false(.recorder_env$active)
})

test_that("record_stop() warns if no active recording", {
  if (.recorder_env$active) suppressMessages(record_stop())
  expect_warning(record_stop(), "No active recording")
})

# ---- .recorder_callback() simulated invocations ------------------------------

test_that("callback captures data frame assignments", {
  suppressMessages(record_start())
  df <- data.frame(x = 1:3)
  .recorder_callback(quote(df <- data.frame(x = 1:3)), df, ok = TRUE, visible = FALSE)
  recs <- get_records()
  suppressMessages(record_stop())

  expect_length(recs, 1L)
  expect_equal(recs[[1]]$event_type,   "assignment")
  expect_equal(recs[[1]]$object_name,  "df")
  expect_equal(recs[[1]]$object_type,  "df")
  expect_equal(recs[[1]]$object_class, "data.frame")
})

test_that("callback captures lm as visible_return when expression is not an assignment", {
  suppressMessages(record_start())
  m <- lm(mpg ~ wt, mtcars)
  .recorder_callback(quote(lm(mpg ~ wt, mtcars)), m, ok = TRUE, visible = TRUE)
  recs <- get_records()
  suppressMessages(record_stop())

  expect_length(recs, 1L)
  expect_equal(recs[[1]]$event_type,  "visible_return")
  expect_null(recs[[1]]$object_name)
  expect_equal(recs[[1]]$object_type, "model")
})

test_that("callback ignores expressions where ok = FALSE", {
  suppressMessages(record_start())
  .recorder_callback(quote(x <- 1), 1, ok = FALSE, visible = FALSE)
  recs <- get_records()
  suppressMessages(record_stop())
  expect_length(recs, 0L)
})

test_that("callback ignores NULL values", {
  suppressMessages(record_start())
  .recorder_callback(quote(invisible(NULL)), NULL, ok = TRUE, visible = FALSE)
  recs <- get_records()
  suppressMessages(record_stop())
  expect_length(recs, 0L)
})

test_that("callback ignores non-recordable types", {
  suppressMessages(record_start())
  .recorder_callback(quote(x <- "hello"), "hello", ok = TRUE, visible = FALSE)
  recs <- get_records()
  suppressMessages(record_stop())
  expect_length(recs, 0L)
})

test_that("callback records sequential events with incrementing event_id", {
  suppressMessages(record_start())
  df1 <- data.frame(a = 1)
  df2 <- data.frame(b = 2)
  .recorder_callback(quote(df1 <- data.frame(a = 1)), df1, TRUE, FALSE)
  .recorder_callback(quote(df2 <- data.frame(b = 2)), df2, TRUE, FALSE)
  recs <- get_records()
  suppressMessages(record_stop())

  expect_length(recs, 2L)
  expect_equal(recs[[1]]$event_id, 1L)
  expect_equal(recs[[2]]$event_id, 2L)
})

test_that("overwriting a variable creates two separate records", {
  suppressMessages(record_start())
  m1 <- lm(mpg ~ wt, mtcars)
  m2 <- lm(mpg ~ hp, mtcars)
  .recorder_callback(quote(x <- lm(mpg ~ wt, mtcars)), m1, TRUE, FALSE)
  .recorder_callback(quote(x <- lm(mpg ~ hp, mtcars)), m2, TRUE, FALSE)
  recs <- get_records(name = "x")
  suppressMessages(record_stop())

  expect_length(recs, 2L)
  expect_equal(recs[[1]]$object_name, "x")
  expect_equal(recs[[2]]$object_name, "x")
})

test_that("record_start(record_df = FALSE) skips data frames", {
  suppressMessages(record_start(record_df = FALSE))
  df_obj <- data.frame(x = 1)
  m_obj  <- lm(mpg ~ wt, mtcars)
  .recorder_callback(quote(df_obj <- data.frame(x = 1)), df_obj, TRUE, FALSE)
  .recorder_callback(quote(m_obj  <- lm(mpg ~ wt, mtcars)), m_obj, TRUE, FALSE)
  recs <- get_records()
  suppressMessages(record_stop())

  expect_length(recs, 1L)
  expect_equal(recs[[1]]$object_type, "model")
})

# ---- get_records() filtering -------------------------------------------------

test_that("get_records(type) filters by object type", {
  suppressMessages(record_start())
  df_obj <- data.frame(x = 1)
  m_obj  <- lm(mpg ~ wt, mtcars)
  .recorder_callback(quote(df_obj <- data.frame(x = 1)), df_obj, TRUE, FALSE)
  .recorder_callback(quote(m_obj  <- lm(mpg ~ wt, mtcars)), m_obj, TRUE, FALSE)

  recs_df  <- get_records(type = "df")
  recs_mod <- get_records(type = "model")
  recs_all <- get_records()
  suppressMessages(record_stop())

  expect_length(recs_df,  1L)
  expect_length(recs_mod, 1L)
  expect_length(recs_all, 2L)
})

test_that("get_records(name) filters by object name", {
  suppressMessages(record_start())
  df1 <- data.frame(x = 1)
  df2 <- data.frame(y = 2)
  .recorder_callback(quote(df1 <- data.frame(x = 1)), df1, TRUE, FALSE)
  .recorder_callback(quote(df2 <- data.frame(y = 2)), df2, TRUE, FALSE)
  recs <- get_records(name = "df1")
  suppressMessages(record_stop())

  expect_length(recs, 1L)
  expect_equal(recs[[1]]$object_name, "df1")
})

test_that("get_records() returns robjgrader_records S3 class", {
  suppressMessages(record_start())
  suppressMessages(record_stop())
  recs <- get_records()
  expect_s3_class(recs, "robjgrader_records")
})

test_that("expr_text is stored correctly per record", {
  suppressMessages(record_start())
  df <- data.frame(x = 1)
  expr <- quote(df <- data.frame(x = 1))
  .recorder_callback(expr, df, TRUE, FALSE)
  recs <- get_records()
  suppressMessages(record_stop())

  expect_equal(recs[[1]]$expr_text, deparse(expr, nlines = 1L))
})


# ---- P1 table capture fixes --------------------------------------------------

# Helper: set recorder state without the addTaskCallback machinery
.setup_callback_state <- function(envir = .GlobalEnv) {
  .recorder_env$active  <- TRUE
  .recorder_env$records <- list()
  .recorder_env$counter <- 0L
  .recorder_env$config  <- list(df = TRUE, ggplot = TRUE, model = TRUE, table = TRUE)
  .recorder_env$envir   <- envir
}

test_that("Mode A: table returned invisibly without assignment is captured as invisible_return", {
  .setup_callback_state()
  on.exit(.recorder_env$active <- FALSE, add = TRUE)

  fake_tbl <- structure(list(), class = c("gt_tbl", "list"))
  # Simulate: invisible return, not a print call, no assignment
  .recorder_callback(quote(some_fn(data)), fake_tbl, ok = TRUE, visible = FALSE)

  expect_length(.recorder_env$records, 1L)
  expect_equal(.recorder_env$records[[1L]]$event_type, "invisible_return")
  expect_equal(.recorder_env$records[[1L]]$object_type, "table")
  expect_null(.recorder_env$records[[1L]]$object_name)
})

test_that("Mode A: invisible non-table return is still dropped", {
  .setup_callback_state()
  on.exit(.recorder_env$active <- FALSE, add = TRUE)

  .recorder_callback(quote(some_fn()), 42L, ok = TRUE, visible = FALSE)
  .recorder_callback(quote(some_fn()), "text", ok = TRUE, visible = FALSE)

  expect_length(.recorder_env$records, 0L)
})

test_that("Mode B: print() with bare-name arg recovers table via envir lookup", {
  e <- new.env(parent = emptyenv())
  .setup_callback_state(envir = e)
  on.exit(.recorder_env$active <- FALSE, add = TRUE)

  fake_tbl <- structure(list(), class = c("gt_tbl", "list"))
  assign("my_tbl", fake_tbl, envir = e)

  # value is NULL (what print.flextable() would return); arg is a name
  .recorder_callback(quote(print(my_tbl)), value = NULL, ok = TRUE, visible = FALSE)

  expect_length(.recorder_env$records, 1L)
  expect_equal(.recorder_env$records[[1L]]$event_type, "print_call")
  expect_equal(.recorder_env$records[[1L]]$object_type, "table")
})

test_that("Mode B: print() with inline call arg recovers table by re-evaluating inner expr", {
  e <- new.env(parent = .GlobalEnv)
  .setup_callback_state(envir = e)
  on.exit(.recorder_env$active <- FALSE, add = TRUE)

  fake_tbl <- structure(list(), class = c("gt_tbl", "list"))
  assign("tbl_source", fake_tbl, envir = e)

  # expr = print(identity(tbl_source)) — arg is a call, not a name
  # value is NULL (simulating print.gt_tbl() / print.flextable() return)
  expr <- call("print", call("identity", as.name("tbl_source")))
  .recorder_callback(expr, value = NULL, ok = TRUE, visible = FALSE)

  expect_length(.recorder_env$records, 1L)
  expect_equal(.recorder_env$records[[1L]]$event_type, "print_call")
  expect_equal(.recorder_env$records[[1L]]$object_type, "table")
})

test_that("Mode B: print() with inline call that errors falls back gracefully", {
  .setup_callback_state()
  on.exit(.recorder_env$active <- FALSE, add = TRUE)

  # Inner call will error; value is also non-table -> nothing captured
  expr <- call("print", call("stop", "inner error"))
  .recorder_callback(expr, value = NULL, ok = TRUE, visible = FALSE)

  expect_length(.recorder_env$records, 0L)
})

test_that("Mode A + B: table assignment still captured normally (no regression)", {
  .setup_callback_state()
  on.exit(.recorder_env$active <- FALSE, add = TRUE)

  fake_tbl <- structure(list(), class = c("gt_tbl", "list"))
  .recorder_callback(
    call("<-", as.name("t1"), call("some_fn")),
    value = fake_tbl, ok = TRUE, visible = FALSE
  )

  expect_length(.recorder_env$records, 1L)
  expect_equal(.recorder_env$records[[1L]]$event_type, "assignment")
  expect_equal(.recorder_env$records[[1L]]$object_name, "t1")
})
