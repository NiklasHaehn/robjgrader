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
#' The student file path is stored as \code{attr(records, "student_file")}
#' for downstream use.
#'
#' @param autograder_name Character or \code{NULL}. Additional filename(s) to
#'   exclude beyond the auto-detected calling file.  Default \code{NULL}.
#' @param record_types Character vector of object types to capture. Any subset
#'   of \code{c("df", "ggplot", "model", "table")}.
#'
#' @return A \code{robjgrader_records} object with a \code{"student_file"}
#'   attribute (path of the sourced file) and a \code{.used_ids} environment
#'   attribute for exclusive object matching, returned invisibly.  Pass this
#'   object to all \code{validate()} calls in the same autograder run.
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
#' A zero-cost test case that always passes.  Use it as the first entry in
#' \code{test_cases} to verify that the student file was sourced successfully
#' and that the autograder infrastructure itself is working.  If
#' \code{source_student_file()} throws an error before this function is
#' called, Gradescope will report a failed submission rather than a
#' zero-score result.
#'
#' @return \code{"SUCCESS"}
#' @export
ag_submission_test <- function() "SUCCESS"


#' Detect wrong-format submission files
#'
#' Scans the current working directory for files with the given extension(s).
#' Returns a failing \code{robjgrader_result} (with your custom message) when
#' one or more matching files are found, and a passing result otherwise.
#'
#' Pair with \code{abort_on_fail = TRUE} in \code{\link{run_autograder}} to
#' skip all remaining grading steps when a wrong-format file is detected:
#'
#' \preformatted{
#' run_autograder(list(
#'   list(
#'     name          = "File format",
#'     result        = flag_submission("do",
#'                       message = "Please submit an R script, not a Stata .do file."),
#'     max_score     = 0,
#'     abort_on_fail = TRUE
#'   ),
#'   list(name = "Q1 model", result = validate(records, ...), max_score = 10),
#'   ...
#' ))
#' }
#'
#' @param extensions Character vector of file extensions to flag, with or
#'   without a leading dot (e.g. \code{c("do", "dta")} or \code{c(".py")}).
#' @param message    Character. Message shown to the student when a matching
#'   file is found.  If \code{NULL} (default), an informative message listing
#'   the detected filenames is generated automatically.
#' @param name       Character. Label for this check in the result object.
#'   Default \code{"submission_check"}.
#'
#' @return A \code{robjgrader_result} with \code{overall = FALSE} and the
#'   supplied message when a flagged file is found, or \code{overall = TRUE}
#'   when the submission directory is clean.
#' @export
flag_submission <- function(extensions, message = NULL, name = "submission_check") {
  exts <- unique(sub("^\\.?", ".", as.character(extensions)))
  pat  <- paste0("\\", exts, "$", collapse = "|")

  found <- list.files(pattern = pat, ignore.case = TRUE)

  if (length(found) == 0L) {
    return(.make_result(
      name, "submission", "none",
      list(format = .make_check("format", TRUE, "none", "none",
                                "No wrong-format files detected."))
    ))
  }

  msg <- message %||% sprintf(
    "Wrong file type detected: %s. Please submit an R script (.R).",
    paste(found, collapse = ", ")
  )

  .make_result(
    name, "submission", found[[1L]],
    list(format = .make_check("format", FALSE, "none", found, msg))
  )
}


#' Run autograder test cases and write Gradescope-compatible JSON
#'
#' Iterates over \code{test_cases}, calls each test function, compares the
#' return value to the expected value, accumulates scores, and writes the
#' results to a JSON file that Gradescope can parse.  Also prints a summary
#' to the console when \code{verbose = TRUE}.
#'
#' @param test_cases A list of test-case lists.  Two interfaces are supported:
#'
#'   \strong{Result interface} (recommended -- works with \code{validate()} and
#'   \code{validate_text()} output):
#'   \describe{
#'     \item{\code{name}}{Human-readable criterion label shown to students.}
#'     \item{\code{result}}{A \code{robjgrader_result} from \code{validate()}
#'       or \code{validate_text()}.}
#'     \item{\code{max_score}}{Maximum point value for this criterion.  For
#'       text results with a normalized \code{score} field (0--1), the actual
#'       score is \code{score * max_score}, enabling partial credit.  For all
#'       other results, \code{max_score} is awarded in full if
#'       \code{overall == TRUE} and 0 otherwise.}
#'     \item{\code{visibility}}{Optional. \code{"visible"} (default),
#'       \code{"hidden"}, \code{"after_due_date"}, or
#'       \code{"after_published"} (Gradescope visibility keys).}
#'     \item{\code{abort_on_fail}}{Optional logical. If \code{TRUE} and this
#'       test case fails (\code{overall != TRUE}), all subsequent test cases
#'       are skipped with score 0 and the output
#'       \code{"Skipped — prior check failed."}.  Use with
#'       \code{\link{flag_submission}} to halt grading when the submission
#'       format is incorrect.  Default \code{FALSE}.}
#'   }
#'
#'   \strong{Legacy function interface} (for custom test functions):
#'   \describe{
#'     \item{\code{name}}{Human-readable criterion label.}
#'     \item{\code{fun}}{A function object or character string naming a
#'       function in the calling environment.}
#'     \item{\code{args}}{A list of arguments passed to \code{fun} via
#'       \code{do.call()}.}
#'     \item{\code{expect}}{Expected return value; compared as a string via
#'       \code{toString()}.}
#'     \item{\code{weight}}{Point value for this criterion.}
#'     \item{\code{visibility}}{Optional. Same values as above.}
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

    # -- Result-based interface (validate() / validate_text() output) ----------
    if (!is.null(tc[["result"]])) {
      res    <- tc[["result"]]
      max_sc <- tc[["max_score"]] %||% 0

      score_val <- if (!is.null(res$score) && !is.na(res$score)) {
        round(res$score * max_sc, 2)
      } else if (isTRUE(res$overall)) {
        max_sc
      } else {
        0
      }

      outcome <- result_to_outcome(res)
      output  <- if (outcome == "SUCCESS") {
        "Test passed!\n"
      } else {
        fb_str <- if (!is.null(res$feedback) && nchar(res$feedback) > 0L)
          paste0("\n\nFeedback: ", res$feedback)
        else
          ""
        paste0(outcome, fb_str)
      }

      entry <- list(
        name      = tc[["name"]],
        score     = score_val,
        max_score = max_sc,
        output    = output
      )
      if (!is.null(tc[["visibility"]]) && tc[["visibility"]] != "visible")
        entry[["visibility"]] <- tc[["visibility"]]
      results[["tests"]][[i]] <- entry

      if (isTRUE(tc[["abort_on_fail"]]) && !isTRUE(res$overall)) {
        for (j in seq_len(length(test_cases) - i) + i) {
          tc_j <- test_cases[[j]]
          results[["tests"]][[j]] <- list(
            name      = tc_j[["name"]] %||% paste("Test", j),
            score     = 0,
            max_score = tc_j[["max_score"]] %||% tc_j[["weight"]] %||% 0,
            output    = "Skipped — prior check failed."
          )
        }
        break
      }
      next
    }

    # -- Legacy function-based interface ---------------------------------------
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

    if (isTRUE(tc[["abort_on_fail"]]) && !passed) {
      for (j in seq_len(length(test_cases) - i) + i) {
        tc_j <- test_cases[[j]]
        results[["tests"]][[j]] <- list(
          name      = tc_j[["name"]] %||% paste("Test", j),
          score     = 0,
          max_score = tc_j[["max_score"]] %||% tc_j[["weight"]] %||% 0,
          output    = "Skipped — prior check failed."
        )
      }
      break
    }
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
