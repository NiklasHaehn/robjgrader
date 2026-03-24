# Core autograder runner.
#
# Source this file from a PS-specific autograde.R, then:
#   1. Load any additional libraries the PS needs.
#   2. Call source_student_file() to load the student submission.
#   3. Define your test functions.
#   4. Define a `test.cases` list (see structure below).
#   5. Call run_autograder(test.cases).
#
# test.cases structure:
#   list(
#     list(
#       name       = "Human-readable criterion name",
#       fun        = "testFunctionName",   # function name as string, OR a function object
#       args       = list(),               # arguments passed to fun via do.call
#       expect     = "SUCCESS",            # expected return value
#       visibility = "visible",            # "visible" or "hidden"
#       weight     = 2                     # points for this criterion
#     ),
#     ...
#   )

suppressPackageStartupMessages(library(jsonlite))


# ---- Environment detection ---------------------------------------------------

# Returns the correct JSON output path depending on whether we are running
# locally or inside a Gradescope container.
.autograder_json_path <- function() {
  if (Sys.getenv("ASSIGNMENT_TITLE") != "") {
    "/autograder/results/results.json"
  } else {
    "results.json"
  }
}


# ---- Student file sourcing ---------------------------------------------------

# Finds and sources the student's R submission in the current working directory.
# Excludes `autograder_name` from the candidate list (default "autograde.R").
# Returns the path to the sourced file invisibly.
#
# Note: call setwd() before this if running interactively in RStudio.
# On Gradescope the working directory is already set correctly.
source_student_file <- function(autograder_name = "autograde.R") {
  r_files <- list.files(pattern = "\\.[Rr]$", full.names = TRUE)
  student <- r_files[!basename(r_files) %in% autograder_name]

  if (length(student) == 0L)
    stop("No student R file found in the current directory.")
  if (length(student) > 1L)
    warning(sprintf(
      "%d R files found; using the first: %s",
      length(student), basename(student[[1L]])
    ))

  student_file <- student[[1L]]
  message(sprintf("Sourcing student file: %s", basename(student_file)))
  source(student_file, local = FALSE)
  invisible(student_file)
}


# ---- Built-in test functions -------------------------------------------------

# Always passes. Use as the first test case to confirm the submission ran.
testSubmission <- function() "SUCCESS"


# ---- Comparison utilities ----------------------------------------------------

# Converts any R object to a character string suitable for equality comparison
# in test output messages.
my.toString <- function(obj) {
  if (inherits(obj, "data.frame")) {
    toString(tibble::as_tibble(lapply(obj, as.character)))
  } else {
    toString(obj)
  }
}

# Element-wise equality check; returns a single logical.
my.isEqual <- function(obj1, obj2) {
  isTRUE(all(obj1 == obj2))
}

# Converts factor columns to character (useful before string comparisons).
factors.to.chars <- function(column) {
  if (is.factor(column)) as.character(column) else column
}


# ---- Test runner -------------------------------------------------------------

# Runs all test cases, prints results to the console, and writes the
# Gradescope-compatible JSON to `json_path`.
#
# Returns the results list invisibly.
run_autograder <- function(test.cases,
                           json_path = .autograder_json_path(),
                           verbose   = TRUE) {
  results <- list(stdout_visibility = "visible", tests = list())

  for (i in seq_along(test.cases)) {
    tc <- test.cases[[i]]

    ret <- tryCatch(
      do.call(tc[["fun"]], tc[["args"]]),
      error   = function(e) e,
      warning = function(w) w,
      message = function(m) m
    )

    passed <- my.toString(ret) == my.toString(tc[["expect"]])

    entry <- list(
      name      = tc[["name"]],
      score     = if (passed) tc[["weight"]] else 0,
      max_score = tc[["weight"]],
      output    = if (passed)
        "Test passed!\n"
      else
        paste0(
          "\n\nExpected: ", my.toString(tc[["expect"]]),
          "\n\nGot: ",      my.toString(ret)
        )
    )

    if (!is.null(tc[["visibility"]]) && tc[["visibility"]] != "visible")
      entry[["visibility"]] <- tc[["visibility"]]

    results[["tests"]][[i]] <- entry
  }

  if (verbose) .print_results(test.cases, results)

  dir.create(dirname(json_path), recursive = TRUE, showWarnings = FALSE)
  write(toJSON(results, auto_unbox = TRUE), file = json_path)

  invisible(results)
}


# ---- Console output ----------------------------------------------------------

.print_results <- function(test.cases, results) {
  for (i in seq_along(results[["tests"]])) {
    tc  <- test.cases[[i]]
    res <- results[["tests"]][[i]]
    cat(sprintf(
      "Test %s: %s(%s)\nExpected: %s\nOutput:\n %s\nScore: %g/%g\n%s\n\n",
      tc[["name"]],
      if (is.character(tc[["fun"]])) tc[["fun"]] else "<fn>",
      my.toString(tc[["args"]]),
      my.toString(tc[["expect"]]),
      my.toString(res[["output"]]),
      res[["score"]],
      res[["max_score"]],
      strrep("=", 52)
    ))
  }
}
