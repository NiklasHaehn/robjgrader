# Validator for scalar and atomic vector values.
#
# Works as a standalone function (like validate_text) -- does NOT go through
# the recorder/records system, because scalars are not captured by the recorder.
# The instructor fetches the student's value from .GlobalEnv via
# get_student_value() after source_student_file() has run.
#
# Groups (for reference comparison):
#   value   compare to reference using all.equal() (numeric: floating-point safe)
#   type    compare R class to reference
#
# Individual checks:
#   expected   exact expected value (all.equal); use with tolerance for numeric
#   tolerance  numeric tolerance applied to the expected check
#   type       expected class string (e.g. "numeric", "character", "logical")
#   length     expected vector length
#   range      list(min = , max = ) for numeric values
#   sign       "positive" | "negative" | "non-negative" | "non-positive" | "zero"
#   one_of     character/numeric vector of allowed values
#   regex      regex pattern (character values only)

.scalar_default_groups <- c("value", "type")
.scalar_all_groups     <- c("value", "type")


.validate_scalar <- function(obj, reference, checks, exclude = NULL, name) {
  resolved <- .resolve_groups(checks, exclude, .scalar_default_groups, .scalar_all_groups)
  results  <- list()

  # -- Group checks (require reference) -----------------------------------------
  if (length(resolved$groups) > 0L) {
    if (is.null(reference))
      stop("'reference' is required for group-level scalar checks.")
    for (grp in resolved$groups) {
      cmp <- .compare_scalar_group(obj, reference, grp)
      results[[grp]] <- .make_check(grp, cmp$pass, cmp$expected, cmp$observed, cmp$msg)
    }
  }

  # -- Individual checks --------------------------------------------------------
  chk <- resolved$checks

  if (!is.null(chk$type)) {
    exp  <- chk$type
    obs  <- class(obj)[1L]
    pass <- obs == exp ||
            (exp == "numeric"   && is.numeric(obj))  ||
            (exp == "integer"   && is.integer(obj))  ||
            (exp == "character" && is.character(obj)) ||
            (exp == "logical"   && is.logical(obj))
    results[["type"]] <- .make_check(
      "type", pass, exp, obs,
      if (pass) sprintf("type: %s", obs)
      else      sprintf("type: expected '%s', found '%s'", exp, obs)
    )
  }

  if (!is.null(chk$length)) {
    exp <- as.integer(chk$length); obs <- length(obj)
    results[["length"]] <- .make_check(
      "length", obs == exp, exp, obs,
      sprintf("length: expected %d, found %d", exp, obs)
    )
  }

  if (!is.null(chk$expected)) {
    exp <- chk$expected
    tol <- chk$tolerance
    if (!is.null(tol) && is.numeric(exp) && is.numeric(obj)) {
      pass <- length(obj) == length(exp) && all(abs(obj - exp) <= tol)
      msg  <- if (pass)
        sprintf("value %s within tolerance %s of %s",
                .scalar_str(obj), .scalar_str(tol), .scalar_str(exp))
      else
        sprintf("expected %s (\u00b1%s), found %s",
                .scalar_str(exp), .scalar_str(tol), .scalar_str(obj))
    } else {
      pass <- isTRUE(all.equal(obj, exp))
      msg  <- if (pass) sprintf("value: %s", .scalar_str(obj))
              else      sprintf("expected %s, found %s",
                                .scalar_str(exp), .scalar_str(obj))
    }
    results[["expected"]] <- .make_check("expected", pass, exp, obj, msg)
  }

  if (!is.null(chk$range)) {
    rng <- chk$range
    if (!is.numeric(obj)) {
      results[["range"]] <- .make_check(
        "range", FALSE, rng, obj, "range check requires a numeric value"
      )
    } else {
      lo   <- rng$min %||% -Inf
      hi   <- rng$max %||%  Inf
      pass <- all(obj >= lo) && all(obj <= hi)
      results[["range"]] <- .make_check(
        "range", pass, rng, obj,
        if (pass)
          sprintf("value %s in [%s, %s]",
                  .scalar_str(obj), .scalar_str(lo), .scalar_str(hi))
        else
          sprintf("value %s outside [%s, %s]",
                  .scalar_str(obj), .scalar_str(lo), .scalar_str(hi))
      )
    }
  }

  if (!is.null(chk$sign)) {
    sgn <- chk$sign
    if (!is.numeric(obj)) {
      results[["sign"]] <- .make_check(
        "sign", FALSE, sgn, obj, "sign check requires a numeric value"
      )
    } else {
      pass <- switch(sgn,
        positive      = all(obj  > 0),
        negative      = all(obj  < 0),
        `non-negative` = all(obj >= 0),
        `non-positive` = all(obj <= 0),
        zero          = all(obj == 0),
        stop(sprintf(
          "Unknown sign '%s'. Use 'positive', 'negative', ",
          "'non-negative', 'non-positive', or 'zero'.", sgn
        ))
      )
      results[["sign"]] <- .make_check(
        "sign", pass, sgn, obj,
        if (pass) sprintf("sign '%s' confirmed: %s", sgn, .scalar_str(obj))
        else      sprintf("expected sign '%s', found %s", sgn, .scalar_str(obj))
      )
    }
  }

  if (!is.null(chk$one_of)) {
    allowed <- chk$one_of
    pass    <- all(obj %in% allowed)
    results[["one_of"]] <- .make_check(
      "one_of", pass, allowed, obj,
      if (pass)
        sprintf("value %s is in allowed set", .scalar_str(obj))
      else
        sprintf("value %s not in {%s}",
                .scalar_str(obj),
                paste(.scalar_str(allowed), collapse = ", "))
    )
  }

  if (!is.null(chk$regex)) {
    pat  <- chk$regex
    obs  <- as.character(obj)
    pass <- all(grepl(pat, obs, perl = TRUE))
    results[["regex"]] <- .make_check(
      "regex", pass, pat, obs,
      if (pass) sprintf("value matches pattern '%s'", pat)
      else      sprintf("'%s' does not match pattern '%s'", obs, pat)
    )
  }

  .make_result(name, "scalar", class(obj)[1L], results)
}


.compare_scalar_group <- function(obj, reference, group) {
  switch(group,

    value = {
      pass <- isTRUE(all.equal(obj, reference))
      list(
        pass     = pass,
        expected = reference,
        observed = obj,
        msg      = if (pass) sprintf("value: %s", .scalar_str(obj))
                   else      sprintf("expected %s, found %s",
                                     .scalar_str(reference), .scalar_str(obj))
      )
    },

    type = {
      exp  <- class(reference)[1L]
      obs  <- class(obj)[1L]
      pass <- obs == exp
      list(
        pass     = pass,
        expected = exp,
        observed = obs,
        msg      = if (pass) sprintf("type: %s", obs)
                   else      sprintf("type: expected '%s', found '%s'", exp, obs)
      )
    }
  )
}


.scalar_str <- function(x) {
  if (is.null(x) || length(x) == 0L) return("NULL")
  s <- if (length(x) > 5L)
    sprintf("[%s, ...]", paste(utils::head(x, 5L), collapse = ", "))
  else
    sprintf("[%s]", paste(x, collapse = ", "))
  s
}


# ==============================================================================
# Public API
# ==============================================================================

#' Get a named value from the student's execution environment
#'
#' Fetches \code{name} from \code{envir} after \code{source_student_file()}
#' has evaluated the student script.  Returns \code{NULL} silently when the
#' variable does not exist, so the result can be passed directly to
#' \code{validate_scalar()} without an explicit \code{tryCatch()} call.
#'
#' @details
#' \code{source_student_file()} evaluates the student script in
#' \code{.GlobalEnv} (via \code{record_script()}), so all top-level
#' assignments from the student file are accessible in \code{.GlobalEnv}
#' after sourcing completes.
#'
#' @param name  Character. Variable name to fetch.
#' @param envir Environment to search.  Default \code{.GlobalEnv}.
#'
#' @return The named object, or \code{NULL} if not found.
#' @seealso \code{\link{validate_scalar}}, \code{\link{source_student_file}}
#' @export
get_student_value <- function(name, envir = .GlobalEnv) {
  tryCatch(
    get(name, envir = envir, inherits = FALSE),
    error = function(e) NULL
  )
}


#' Validate a scalar or atomic vector from a student submission
#'
#' Validates a raw R value against a reference object, a set of named checks,
#' or both.  Intended for grading single computed answers such as a rounded
#' coefficient, a p-value, or a character response -- objects that are not
#' captured by the recorder but are assigned by students in their scripts.
#'
#' @details
#' The typical workflow is:
#' \enumerate{
#'   \item Source the student file with \code{source_student_file()}.
#'   \item Fetch the scalar with \code{get_student_value("var_name")}, which
#'     returns \code{NULL} if the student never defined the variable.
#'   \item Pass the result to \code{validate_scalar()}.  A \code{NULL} value
#'     immediately returns a failing result with a "not found" message.
#' }
#'
#' \strong{Groups} (run against \code{reference}; used when no \code{checks}
#' are given):
#' \describe{
#'   \item{\code{value}}{Compares the value to \code{reference} using
#'     \code{all.equal()} (floating-point safe for numeric vectors).}
#'   \item{\code{type}}{Compares \code{class(value)[1]} to
#'     \code{class(reference)[1]}.}
#' }
#'
#' \strong{Individual checks} (named entries in \code{checks}):
#' \describe{
#'   \item{\code{expected}}{Expected value.  For numeric: compared with
#'     \code{all.equal()} unless \code{tolerance} is also supplied.}
#'   \item{\code{tolerance}}{Numeric. Absolute tolerance applied to the
#'     \code{expected} check when both \code{expected} and \code{tolerance}
#'     are numeric.}
#'   \item{\code{type}}{Character. Expected class string, e.g.
#'     \code{"numeric"}, \code{"character"}, \code{"logical"}.}
#'   \item{\code{length}}{Integer. Expected vector length.}
#'   \item{\code{range}}{Named list with optional \code{min} and \code{max}.
#'     Requires a numeric value.}
#'   \item{\code{sign}}{One of \code{"positive"}, \code{"negative"},
#'     \code{"non-negative"}, \code{"non-positive"}, \code{"zero"}.
#'     Requires a numeric value.}
#'   \item{\code{one_of}}{Atomic vector. The value must be an element of this
#'     set.}
#'   \item{\code{regex}}{Character. Regex pattern; the value (coerced to
#'     character) must match.}
#' }
#'
#' @param value     The student's value, typically from
#'   \code{get_student_value()}.  \code{NULL} is treated as "variable not
#'   found" and returns a failing result immediately.
#' @param reference Optional reference value.  Used for group-level comparison
#'   (\code{"value"} and \code{"type"} groups) when no individual checks are
#'   given.
#' @param checks    Named list of individual checks, or a character vector of
#'   group names.  See Details.
#' @param exclude   Character vector of group names to skip when
#'   \code{reference} is provided.
#' @param name      Character. Label for this result, shown in console output
#'   and Gradescope.
#'
#' @return A \code{robjgrader_result} object.
#' @seealso \code{\link{get_student_value}}, \code{\link{run_autograder}}
#' @export
validate_scalar <- function(
  value,
  reference = NULL,
  checks    = list(),
  exclude   = NULL,
  name      = "scalar"
) {
  if (is.null(value)) {
    return(
      .make_result(
        name, "scalar", "NULL",
        list(.make_check(
          "found", FALSE, TRUE, FALSE,
          "Variable not found in student submission"
        ))
      )
    )
  }
  .validate_scalar(value, reference, checks, exclude, name)
}
