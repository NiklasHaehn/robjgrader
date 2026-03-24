#' Source a student submission and return recorded objects
#'
#' Sources a student R file from the current working directory and builds a
#' \code{robjgrader_records} object from all new named objects of a recordable
#' type that appear in \code{.GlobalEnv} after sourcing.
#'
#' @details
#' The calling script is automatically excluded from the candidate list — no
#' manual \code{autograder_name} argument is needed in the typical case.
#' Detection works both when the autograder is run via
#' \code{Rscript autograde.R} (reads \code{--file=} from \code{commandArgs()})
#' and when it is loaded interactively via \code{source("autograde.R")} (walks
#' \code{sys.calls()}).  Pass \code{autograder_name} to add further exclusions.
#'
#' The student file path is stored as \code{attr(records, "student_file")} and
#' is available to code-inspection functions such as those in
#' \file{extract_code.R}.
#'
#' @param autograder_name Character or \code{NULL}. Additional filename(s) to
#'   exclude beyond the auto-detected calling file.  Default \code{NULL}.
#' @param record_types Character vector of object types to capture. Any subset
#'   of \code{c("df", "ggplot", "model", "table")}.
#'
#' @return A \code{robjgrader_records} object with a \code{"student_file"}
#'   attribute, returned invisibly.
#' @export
source_student_file <- function(
  autograder_name = NULL,
  record_types    = c("df", "ggplot", "model", "table")
) {
  calling <- .calling_file()
  exclude  <- unique(c(
    if (!is.null(calling))        basename(calling),
    if (!is.null(autograder_name)) autograder_name
  ))

  r_files <- list.files(pattern = "\\.[Rr]$", full.names = TRUE)
  student <- r_files[!basename(r_files) %in% exclude]

  if (length(student) == 0L)
    stop("No student R file found in the current directory.")
  if (length(student) > 1L)
    warning(sprintf(
      "%d R files found; using the first: %s",
      length(student), basename(student[[1L]])
    ))

  student_file <- student[[1L]]
  message(sprintf("Sourcing student file: %s", basename(student_file)))

  recs <- record_script(
    path          = student_file,
    record_df     = "df"     %in% record_types,
    record_ggplot = "ggplot" %in% record_types,
    record_model  = "model"  %in% record_types,
    record_table  = "table"  %in% record_types
  )

  attr(recs, "student_file") <- student_file
  invisible(recs)
}


#' Wrap a named global object into a robjgrader_records list
#'
#' Convenience helper: fetches \code{name} from \code{envir} and returns a
#' one-element \code{robjgrader_records} object ready to pass to
#' \code{validate()}.  Useful when a test function needs to validate a single
#' known object without holding the full records from
#' \code{source_student_file()}.
#'
#' @param name   Character. Name of the object to fetch.
#' @param envir  Environment to look in. Default \code{.GlobalEnv}.
#'
#' @return A \code{robjgrader_records} object with one entry.
#' @export
grab <- function(name, envir = .GlobalEnv) {
  obj <- tryCatch(
    get(name, envir = envir, inherits = FALSE),
    error = function(e)
      stop(sprintf("Object '%s' not found in the specified environment.", name))
  )
  cfg      <- list(df = TRUE, ggplot = TRUE, model = TRUE, table = TRUE)
  obj_type <- .classify_object(obj, cfg) %||% "unknown"
  structure(list(list(
    event_id    = 1L,
    event_type  = "assignment",
    object_name = name,
    object_type = obj_type,
    object_class = class(obj)[1L],
    expr_text   = name,
    timestamp   = Sys.time(),
    object      = obj
  )), class = "robjgrader_records")
}


#' Convert a validate() result to an autograder outcome string
#'
#' Maps a \code{robjgrader_result} to \code{"SUCCESS"} when all checks pass,
#' or to a concatenated string of all failing check messages otherwise.
#'
#' @param result A \code{robjgrader_result} from \code{validate()}.
#'
#' @return \code{"SUCCESS"} or a newline-separated string of all failing
#'   messages.
#' @export
result_to_outcome <- function(result) {
  if (isTRUE(result$overall)) return("SUCCESS")
  failing <- Filter(function(chk) !isTRUE(chk$pass), result$checks)
  if (length(failing) == 0L) return("SUCCESS")
  paste(vapply(failing, `[[`, character(1L), "message"), collapse = "\n")
}


#' Submission test: always returns "SUCCESS"
#'
#' Use this as the first test case to confirm the student submission ran
#' without errors.
#'
#' @return \code{"SUCCESS"}
#' @export
ag_submission_test <- function() "SUCCESS"


#' Run autograder test cases and write Gradescope-compatible JSON
#'
#' Iterates over \code{test_cases}, calls each test function, compares the
#' return value to the expected value, accumulates scores, and writes the
#' results to a JSON file that Gradescope can parse.  Also prints a summary
#' to the console when \code{verbose = TRUE}.
#'
#' @param test_cases A list of test-case lists.  Each entry must contain:
#'   \describe{
#'     \item{\code{name}}{Human-readable criterion label shown to students.}
#'     \item{\code{fun}}{The test function: a function object or a character
#'       string naming a function visible in the calling environment.}
#'     \item{\code{args}}{A list of arguments passed to \code{fun} via
#'       \code{do.call}.}
#'     \item{\code{expect}}{Expected return value; compared via
#'       \code{.ag_to_string()}.}
#'     \item{\code{visibility}}{\code{"visible"} (default) or
#'       \code{"hidden"}.}
#'     \item{\code{weight}}{Point value for this criterion.}
#'   }
#' @param json_path   Output path for the JSON results file.  Defaults to
#'   \code{"/autograder/results/results.json"} inside a Gradescope container
#'   and \code{"results.json"} otherwise (detected via the
#'   \code{ASSIGNMENT_TITLE} environment variable).
#' @param verbose     Logical. Print per-test summaries to the console.
#'   Default \code{TRUE}.
#'
#' @return The results list, invisibly.
#' @export
run_autograder <- function(test_cases,
                           json_path = .autograder_json_path(),
                           verbose   = TRUE) {
  results <- list(stdout_visibility = "visible", tests = list())

  for (i in seq_along(test_cases)) {
    tc <- test_cases[[i]]

    ret <- tryCatch(
      do.call(tc[["fun"]], tc[["args"]]),
      error   = function(e) paste("Error:", conditionMessage(e)),
      warning = function(w) paste("Warning:", conditionMessage(w)),
      message = function(m) paste("Message:", conditionMessage(m))
    )

    passed <- .ag_to_string(ret) == .ag_to_string(tc[["expect"]])

    entry <- list(
      name      = tc[["name"]],
      score     = if (passed) tc[["weight"]] else 0,
      max_score = tc[["weight"]],
      output    = if (passed)
        "Test passed!\n"
      else
        paste0("\n\nExpected: ", .ag_to_string(tc[["expect"]]),
               "\n\nGot: ",      .ag_to_string(ret))
    )

    if (!is.null(tc[["visibility"]]) && tc[["visibility"]] != "visible")
      entry[["visibility"]] <- tc[["visibility"]]

    results[["tests"]][[i]] <- entry
  }

  if (verbose) .print_results(test_cases, results)

  dir.create(dirname(json_path), recursive = TRUE, showWarnings = FALSE)
  write(jsonlite::toJSON(results, auto_unbox = TRUE), file = json_path)

  invisible(results)
}


# ---- Internal helpers --------------------------------------------------------

.ag_to_string <- function(obj) {
  if (inherits(obj, "data.frame")) {
    toString(as.data.frame(lapply(obj, as.character)))
  } else {
    toString(obj)
  }
}

.calling_file <- function() {
  # Case 1: Rscript --file=autograde.R
  args  <- commandArgs(trailingOnly = FALSE)
  flag  <- grep("^--file=", args, value = TRUE)
  if (length(flag) > 0L) {
    path <- sub("^--file=", "", flag[[1L]])
    return(normalizePath(path, mustWork = FALSE))
  }
  # Case 2: interactive source("autograde.R") — walk call stack
  calls <- sys.calls()
  for (call in rev(calls)) {
    if (is.call(call) && length(call) >= 2L &&
        identical(call[[1L]], quote(source))) {
      path <- tryCatch(
        eval(call[[2L]], envir = parent.frame()),
        error = function(e) NULL
      )
      if (is.character(path) && nzchar(path))
        return(normalizePath(path, mustWork = FALSE))
    }
  }
  NULL
}

.autograder_json_path <- function() {
  if (Sys.getenv("ASSIGNMENT_TITLE") != "") {
    "/autograder/results/results.json"
  } else {
    "results.json"
  }
}

.print_results <- function(test_cases, results) {
  for (i in seq_along(results[["tests"]])) {
    tc  <- test_cases[[i]]
    res <- results[["tests"]][[i]]
    cat(sprintf(
      "Test %s: %s(%s)\nExpected: %s\nOutput:\n %s\nScore: %g/%g\n%s\n\n",
      tc[["name"]],
      if (is.character(tc[["fun"]])) tc[["fun"]] else "<fn>",
      .ag_to_string(tc[["args"]]),
      .ag_to_string(tc[["expect"]]),
      .ag_to_string(res[["output"]]),
      res[["score"]],
      res[["max_score"]],
      strrep("=", 52)
    ))
  }
}
