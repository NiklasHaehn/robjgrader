library(testthat)

# ==============================================================================
# split_do_file()
# ==============================================================================

test_that("split_do_file: returns all section names", {
  sections <- split_do_file("")
  expected <- c("packages", "setup", "data_load", "data_mgmt",
                "estimation", "tables", "figures", "other")
  expect_true(all(expected %in% names(sections)))
})

test_that("split_do_file: classifies regression command correctly", {
  code <- "reg y x1 x2, robust"
  s    <- split_do_file(code)
  expect_true(length(s$estimation) > 0L)
  expect_match(s$estimation[[1L]], "reg")
})

test_that("split_do_file: classifies logit correctly", {
  code <- "logit y x1 x2"
  s    <- split_do_file(code)
  expect_true(length(s$estimation) > 0L)
})

test_that("split_do_file: classifies data load commands", {
  code <- 'use "data/final.dta", clear'
  s    <- split_do_file(code)
  expect_true(length(s$data_load) > 0L)
})

test_that("split_do_file: classifies gen as data_mgmt", {
  code <- "gen log_gdp = log(gdp)"
  s    <- split_do_file(code)
  expect_true(length(s$data_mgmt) > 0L)
})

test_that("split_do_file: classifies esttab as tables", {
  code <- 'esttab using "output/table1.tex", replace'
  s    <- split_do_file(code)
  expect_true(length(s$tables) > 0L)
})

test_that("split_do_file: classifies scatter as figures", {
  code <- "scatter y x, mcolor(blue)"
  s    <- split_do_file(code)
  expect_true(length(s$figures) > 0L)
})

test_that("split_do_file: classifies ssc install as packages", {
  code <- "ssc install reghdfe"
  s    <- split_do_file(code)
  expect_true(length(s$packages) > 0L)
})

test_that("split_do_file: classifies set more off as setup", {
  code <- "set more off"
  s    <- split_do_file(code)
  expect_true(length(s$setup) > 0L)
})

test_that("split_do_file: ignores comment lines (star)", {
  code <- "* this is a comment\nreg y x"
  s    <- split_do_file(code)
  expect_equal(length(s$estimation), 1L)
})

test_that("split_do_file: ignores comment lines (//)", {
  code <- "// this is a comment\nreg y x"
  s    <- split_do_file(code)
  expect_equal(length(s$estimation), 1L)
})

test_that("split_do_file: unknown command goes to other", {
  code <- "display \"hello\""
  s    <- split_do_file(code)
  expect_true(length(s$other) > 0L)
})

test_that("split_do_file: multiple commands classified independently", {
  code <- paste(
    'use "data.dta"',
    "gen logy = log(y)",
    "reg y x, robust",
    'esttab using "t.tex"',
    sep = "\n"
  )
  s <- split_do_file(code)
  expect_equal(length(s$data_load),  1L)
  expect_equal(length(s$data_mgmt),  1L)
  expect_equal(length(s$estimation), 1L)
  expect_equal(length(s$tables),     1L)
})

# ==============================================================================
# read_do_file()
# ==============================================================================

test_that("read_do_file: reads file and returns string", {
  tmp <- tempfile(fileext = ".do")
  writeLines(c("reg y x", "gen z = x^2"), tmp)
  on.exit(unlink(tmp))
  code <- read_do_file(tmp)
  expect_type(code, "character")
  expect_match(code, "reg y x")
  expect_match(code, "gen z")
})

test_that("read_do_file: stops on missing file", {
  expect_error(read_do_file("nonexistent_file.do"), "not found")
})

# ==============================================================================
# validate_do() — contains
# ==============================================================================

test_that("validate_do: contains PASS when pattern found", {
  res <- validate_do("reg y x, robust", checks = list(contains = "robust"))
  expect_true(res$overall)
})

test_that("validate_do: contains FAIL when pattern absent", {
  res <- validate_do("reg y x", checks = list(contains = "robust"))
  expect_false(res$overall)
  expect_match(res$checks[["contains.robust"]]$message, "not found")
})

test_that("validate_do: multiple contains patterns checked independently", {
  code <- "reg y x, robust cluster(id)"
  res  <- validate_do(code, checks = list(contains = c("robust", "cluster")))
  checks_named <- names(res$checks)
  expect_true(any(grepl("robust",  checks_named)))
  expect_true(any(grepl("cluster", checks_named)))
  expect_true(res$overall)
})

# ==============================================================================
# validate_do() — not_contains
# ==============================================================================

test_that("validate_do: not_contains PASS when pattern absent", {
  res <- validate_do("reg y x", checks = list(not_contains = "quietly"))
  expect_true(res$overall)
})

test_that("validate_do: not_contains FAIL when forbidden pattern present", {
  res <- validate_do("quietly reg y x", checks = list(not_contains = "quietly"))
  expect_false(res$overall)
  expect_match(res$checks[["not_contains.quietly"]]$message, "Forbidden")
})

# ==============================================================================
# validate_do() — command
# ==============================================================================

test_that("validate_do: command PASS when command found", {
  res <- validate_do("reg y x, robust", checks = list(command = "reg"))
  expect_true(res$overall)
})

test_that("validate_do: command FAIL when command absent", {
  res <- validate_do("logit y x", checks = list(command = "reg"))
  expect_false(res$overall)
  expect_match(res$checks$command.reg$message, "not found")
})

test_that("validate_do: command is case-insensitive", {
  res <- validate_do("REG y x", checks = list(command = "reg"))
  expect_true(res$checks$command.reg$pass)
})

test_that("validate_do: command does not match partial word", {
  res <- validate_do("reghdfe y x, absorb(id)", checks = list(command = "reg"))
  expect_false(res$checks$command.reg$pass)
})

# ==============================================================================
# validate_do() — variable
# ==============================================================================

test_that("validate_do: variable PASS when variable found", {
  res <- validate_do("reg y log_gdp year", checks = list(variable = "log_gdp"))
  expect_true(res$overall)
})

test_that("validate_do: variable FAIL when variable absent", {
  res <- validate_do("reg y x", checks = list(variable = "log_gdp"))
  expect_false(res$overall)
})

# ==============================================================================
# validate_do() — n_commands
# ==============================================================================

test_that("validate_do: n_commands PASS with exact count", {
  code <- "reg y x\nreg y x z"
  res  <- validate_do(code, checks = list(
    n_commands = list(type = "estimation", expected = 2L)
  ))
  expect_true(res$overall)
})

test_that("validate_do: n_commands PASS within tolerance", {
  code <- "reg y x\nreg y x z\nreg y x z w"
  res  <- validate_do(code, checks = list(
    n_commands = list(type = "estimation", expected = 2L, tolerance = 1L)
  ))
  expect_true(res$overall)
})

test_that("validate_do: n_commands FAIL outside tolerance", {
  code <- "reg y x"
  res  <- validate_do(code, checks = list(
    n_commands = list(type = "estimation", expected = 3L, tolerance = 0L)
  ))
  expect_false(res$overall)
  expect_match(res$checks$n_commands.estimation$message, "expected 3")
})

# ==============================================================================
# validate_do() — section restriction
# ==============================================================================

test_that("validate_do: section restricts target code", {
  code <- paste(
    'use "data.dta"',
    "reg y x, robust",
    sep = "\n"
  )
  # 'robust' is in estimation, not data_load
  res_est  <- validate_do(code, section = "estimation",
                           checks = list(contains = "robust"))
  res_load <- validate_do(code, section = "data_load",
                           checks = list(contains = "robust"))
  expect_true(res_est$overall)
  expect_false(res_load$overall)
})

# ==============================================================================
# validate_do() — result structure
# ==============================================================================

test_that("validate_do: returns robjgrader_result", {
  res <- validate_do("reg y x")
  expect_s3_class(res, "robjgrader_result")
})

test_that("validate_do: custom name propagated", {
  res <- validate_do("reg y x", name = "stata_q1")
  expect_equal(res$object_name, "stata_q1")
})

test_that("validate_do: accepts file path as code argument", {
  tmp <- tempfile(fileext = ".do")
  writeLines("reg y x, robust", tmp)
  on.exit(unlink(tmp))
  res <- validate_do(tmp, checks = list(contains = "robust"))
  expect_true(res$overall)
})
