#' Validate a recorded object
#'
#' Retrieves an object from a \code{robjgrader_records} result and validates
#' it against a reference object, a set of named checks, or both.
#'
#' @details
#' \strong{Exclusive matching:} Each student object is matched to at most one
#' \code{validate()} call per autograder run.  Once a record is matched it is
#' marked as used, so a second \code{validate()} call with identical criteria
#' will match the \emph{next best} available candidate rather than reusing the
#' same object.  This ensures that two separate grading steps targeting the
#' same type of object (e.g. "first lm" and "second lm") receive distinct
#' student objects.  The used-object state lives in the
#' \code{.used_ids} environment attribute of the \code{records} object, which
#' is initialised automatically by \code{record_script()} and
#' \code{source_student_file()}.
#'
#' \strong{Graceful not-found behaviour:} When no matching object is available
#' (either because the student never created an object of that type, or all
#' candidates are already used), \code{validate()} returns a
#' \code{robjgrader_result} with \code{overall = FALSE} and a descriptive
#' "not found" message.  It never calls \code{stop()}, so subsequent grading
#' steps continue to run regardless.
#'
#' @param records   A \code{robjgrader_records} object returned by
#'   \code{record_script()} or \code{source_student_file()}.  The object
#'   carries a \code{.used_ids} environment attribute that tracks which records
#'   have already been matched; pass the \emph{same} \code{records} object to
#'   all \code{validate()} calls within one autograder run to get correct
#'   exclusive-matching behaviour.
#' @param name      Character. Variable name of the recorded object. Primary
#'   identification method.
#' @param match     Named list of coarse matching criteria used when \code{name}
#'   is absent or unknown (e.g. student chose a different variable name, or the
#'   object was never assigned).  Must include \code{type}; all other keys are
#'   optional.  Criteria are applied in order of discriminating power (most to
#'   least) until one candidate remains; a criterion that would eliminate all
#'   remaining candidates is skipped.
#'
#'   \strong{Universal (all types):}
#'   \describe{
#'     \item{\code{type}}{Required. One of \code{"ggplot"}, \code{"model"},
#'       \code{"df"}, \code{"table"}.}
#'     \item{\code{expr_contains}}{Character. Fixed substring matched against
#'       the source expression text (e.g. \code{"filter(year == 2007)"}).}
#'   }
#'
#'   \strong{ggplot:}
#'   \describe{
#'     \item{\code{aes_x}, \code{aes_y}, \code{aes_color}, \code{aes_colour},
#'       \code{aes_fill}, \code{aes_size}, \code{aes_shape}, \code{aes_alpha},
#'       \code{aes_group}, \code{aes_linetype}}{Character. Expected variable
#'       name mapped to that aesthetic.}
#'     \item{\code{geom}}{Character. Geom class name, either short
#'       (\code{"point"}) or full (\code{"GeomPoint"}). Matches if any layer
#'       uses that geom.}
#'     \item{\code{facet_var}}{Character vector. Variable name(s) that must
#'       appear in the facet specification.}
#'   }
#'
#'   \strong{model:}
#'   \describe{
#'     \item{\code{outcome}}{Character. Name of the response variable (LHS).}
#'     \item{\code{estimator}}{Character. Class the model must inherit from,
#'       e.g. \code{"lm"}, \code{"glm"}, \code{"fixest"}.}
#'     \item{\code{predictors}}{Character vector. RHS variable names that must
#'       all be present in the model formula.}
#'     \item{\code{fixed_effects}}{Character vector. Fixed-effect variable
#'       names (relevant for \pkg{fixest} models).}
#'     \item{\code{cluster}}{Character vector. Clustering variable names.}
#'     \item{\code{nobs}}{Integer. Exact number of observations.}
#'   }
#'
#'   \strong{df:}
#'   \describe{
#'     \item{\code{names}}{Character vector. Column names that must all be
#'       present.}
#'     \item{\code{nrow}}{Integer. Exact number of rows.}
#'     \item{\code{ncol}}{Integer. Exact number of columns.}
#'   }
#'
#'   \strong{table:}
#'   \describe{
#'     \item{\code{expr_contains}}{Only \code{expr_contains} is effective for
#'       tables; structural criteria are not yet supported.}
#'   }
#' @param reference Optional reference object.  Serves two roles: (1) when
#'   neither \code{name} nor \code{match} is given, the reference is used to
#'   locate the best-matching candidate automatically by extracting structural
#'   criteria (type, aesthetics, model outcome, column names, etc.) and running
#'   them through the normal match logic; (2) when a candidate has been
#'   identified, the reference is used for group-level semantic comparison.
#' @param checks    Named list of type-specific property checks, or a character
#'   vector of group names to check against \code{reference}.
#' @param exclude   Character vector of group names to skip when
#'   \code{reference} is provided. Individual \code{checks} are unaffected.
#' @param position  Positional fallback when matching yields > 1 candidate:
#'   \code{"last"} (default), \code{"first"}, or an integer index among the
#'   filtered candidates.
#'
#' @return A \code{robjgrader_result} object.
#' @export
validate <- function(
  records,
  name      = NULL,
  match     = list(),
  reference = NULL,
  checks    = list(),
  exclude   = NULL,
  position  = "last"
) {
  if (is.null(name) && length(match) == 0L && is.null(reference)) {
    stop("Provide 'name', 'match', or 'reference' to identify the object.")
  }
  if (!is.null(name) && length(match) > 0L) {
    stop("'name' and 'match' are mutually exclusive.")
  }

  # Filter out records already matched in this session (exclusive assignment)
  used      <- attr(records, ".used_ids") %||% new.env(parent = emptyenv())
  available <- Filter(
    function(r) is.null(used[[as.character(r$event_id)]]),
    records
  )

  # Attempt lookup — never stop(), always return a result
  err_msg <- NULL
  record  <- tryCatch(
    {
      if (!is.null(name))          .lookup_by_name(available, name, position)
      else if (length(match) > 0L) .lookup_by_match(available, match, position)
      else                         .lookup_by_reference(available, reference, position)
    },
    error = function(e) { err_msg <<- conditionMessage(e); NULL }
  )

  if (is.null(record)) {
    obj_type  <- match[["type"]] %||%
      if (!is.null(reference)) {
        .classify_object(reference,
          list(df = TRUE, ggplot = TRUE, model = TRUE, table = TRUE)) %||% "unknown"
      } else "unknown"
    obj_label <- name %||% obj_type
    return(.make_result(
      obj_label, obj_type, "NULL",
      list(found = .make_check(
        "found", FALSE, TRUE, FALSE,
        sprintf("No available student object found: %s", err_msg %||% "unknown error")
      ))
    ))
  }

  # Mark this record as used so it cannot be matched again
  assign(as.character(record$event_id), TRUE, envir = used)

  obj      <- record$object
  obj_type <- record$object_type
  obj_name <- record$object_name %||% "<anonymous>"

  switch(obj_type,
    df     = .validate_df(obj, reference, checks, exclude, obj_name),
    ggplot = .validate_plot(obj, reference, checks, exclude, obj_name),
    model  = .validate_model(obj, reference, checks, exclude, obj_name),
    table  = .validate_table(obj, reference, checks, exclude, obj_name),
    stop(sprintf("No validator available for type '%s'.", obj_type))
  )
}


# -- Object lookup -------------------------------------------------------------

.lookup_by_name <- function(records, name, position) {
  matching <- Filter(
    function(r) !is.null(r$object_name) && r$object_name == name,
    records
  )
  if (length(matching) == 0L) {
    stop(sprintf("No recorded object named '%s' found.", name))
  }
  .select_position(matching, position, context = sprintf("name '%s'", name))
}


.lookup_by_match <- function(records, match, position) {
  type <- match[["type"]]
  if (is.null(type)) stop("'match' must include a 'type' entry.")

  candidates <- Filter(function(r) r$object_type == type, records)
  if (length(candidates) == 0L) {
    stop(sprintf("No recorded objects of type '%s' found.", type))
  }

  # Default criterion order per type (most to least discriminating)
  order_map <- list(
    ggplot = c("aes_x", "aes_y", "geom", "aes_color", "aes_colour",
               "aes_fill", "facet_var", "expr_contains"),
    model  = c("outcome", "estimator", "fixed_effects", "predictors",
               "cluster", "nobs", "expr_contains"),
    df     = c("names", "nrow", "ncol", "col_types", "expr_contains"),
    table  = c("n_models", "terms", "nrow", "ncol", "expr_contains")
  )
  criterion_order <- order_map[[type]] %||% character(0L)

  # Sort provided criteria by discriminating power; unknowns go last
  keys <- setdiff(names(match), "type")
  known   <- intersect(criterion_order, keys)
  unknown <- setdiff(keys, criterion_order)
  ordered_keys <- c(known, unknown)

  for (key in ordered_keys) {
    value <- match[[key]]
    filtered <- .filter_candidates(candidates, key, value, type)

    if (length(filtered) == 0L) {
      # Backtrack: skip this criterion
      next
    }
    candidates <- filtered
    if (length(candidates) == 1L) break
  }

  if (length(candidates) > 1L) {
    warning(sprintf(
      "%d candidates remain after matching; using position '%s'. ",
      length(candidates), position
    ), call. = FALSE)
  }

  .select_position(candidates, position, context = "match")
}


.lookup_by_reference <- function(records, reference, position) {
  cfg  <- list(df = TRUE, ggplot = TRUE, model = TRUE, table = TRUE)
  type <- .classify_object(reference, cfg)
  if (is.null(type))
    stop("Cannot determine type of reference object for automatic matching.")
  m <- .reference_to_match(reference, type)
  .lookup_by_match(records, m, position)
}


.reference_to_match <- function(reference, type) {
  m <- list(type = type)

  if (type == "df") {
    m$nrow  <- nrow(reference)
    m$ncol  <- ncol(reference)
    m$names <- names(reference)

  } else if (type == "ggplot") {
    for (aes in c("x", "y", "color", "colour", "fill",
                  "size", "shape", "alpha", "group", "linetype")) {
      val <- tryCatch(.get_aes_label(reference, aes), error = function(e) NULL)
      if (!is.null(val)) m[[paste0("aes_", aes)]] <- val
    }
    if (length(reference$layers) > 0L)
      m$geom <- class(reference$layers[[1L]]$geom)[1L]
    fvars <- tryCatch(.get_facet_vars(reference), error = function(e) character(0L))
    if (length(fvars) > 0L) m$facet_var <- fvars

  } else if (type == "model") {
    m$outcome   <- tryCatch(.get_model_outcome(reference),    error = function(e) NULL)
    m$estimator <- class(reference)[1L]
    preds       <- tryCatch(.get_model_rhs_vars(reference),   error = function(e) character(0L))
    if (length(preds) > 0L) m$predictors <- preds
    fes         <- tryCatch(.get_fixed_effects(reference),    error = function(e) character(0L))
    if (length(fes) > 0L) m$fixed_effects <- fes
    cl          <- tryCatch(.get_cluster(reference),          error = function(e) NULL)
    if (!is.null(cl) && length(cl) > 0L) m$cluster <- cl
  }
  # table: no structural match criteria implemented yet

  m
}


.filter_candidates <- function(candidates, key, value, type) {
  Filter(function(r) {
    obj <- r$object

    if (key == "expr_contains") {
      return(isTRUE(grepl(value, r$expr_text, fixed = TRUE)))
    }

    result <- tryCatch({
      if (type == "ggplot") {
        .match_ggplot(obj, key, value)
      } else if (type == "model") {
        .match_model(obj, key, value)
      } else if (type == "df") {
        .match_df(obj, key, value)
      } else {
        TRUE
      }
    }, error = function(e) FALSE)

    isTRUE(result)
  }, candidates)
}


.match_ggplot <- function(obj, key, value) {
  if (grepl("^aes_", key)) {
    obs <- .get_aes_label(obj, sub("^aes_", "", key))
    return(!is.null(obs) && obs == value)
  }
  if (key == "geom") {
    geoms <- vapply(obj$layers, function(l) class(l$geom)[1L], character(1L))
    return(any(.normalize_geom(value) %in% geoms))
  }
  if (key == "facet_var") {
    return(all(value %in% .get_facet_vars(obj)))
  }
  TRUE
}


.match_model <- function(obj, key, value) {
  if (key == "outcome")      return(isTRUE(.get_model_outcome(obj) == value))
  if (key == "estimator")    return(inherits(obj, value))
  if (key == "nobs")         return(isTRUE(tryCatch(nobs(obj), error = function(e) NA) == value))
  if (key == "predictors")   return(all(value %in% .get_model_rhs_vars(obj)))
  if (key == "fixed_effects") return(all(value %in% .get_fixed_effects(obj)))
  if (key == "cluster")      return(all(value %in% (.get_cluster(obj) %||% character(0L))))
  TRUE
}


.match_df <- function(obj, key, value) {
  if (key == "nrow")  return(isTRUE(nrow(obj) == value))
  if (key == "ncol")  return(isTRUE(ncol(obj) == value))
  if (key == "names") return(all(value %in% names(obj)))
  TRUE
}


.select_position <- function(candidates, position, context) {
  n <- length(candidates)
  idx <- switch(
    as.character(position),
    "last"  = n,
    "first" = 1L,
    {
      i <- as.integer(position)
      if (i < 1L || i > n)
        stop(sprintf(
          "position %d out of range (1 to %d) for %s.", i, n, context
        ))
      i
    }
  )
  candidates[[idx]]
}


# -- Result infrastructure -----------------------------------------------------

.make_check <- function(check, pass, expected, observed, message) {
  list(
    check    = check,
    pass     = pass,
    expected = expected,
    observed = observed,
    message  = message
  )
}

.make_result <- function(name, type, cls, checks_list) {
  overall <- if (length(checks_list) == 0L) NA else
    all(vapply(checks_list, `[[`, logical(1L), "pass"))

  structure(
    list(
      object_name  = name,
      object_type  = type,
      object_class = cls,
      overall      = overall,
      checks       = checks_list
    ),
    class = "robjgrader_result"
  )
}


#' @export
print.robjgrader_result <- function(x, ...) {
  status <- if (is.na(x$overall)) "--" else if (x$overall) "PASS" else "FAIL"

  score_str <- if (!is.null(x$score) && !is.na(x$score) &&
                   x$object_type == "text") {
    sprintf("  score: %.0f%%", x$score * 100)
  } else {
    ""
  }

  cat(sprintf(
    "robjgrader result: %s (%s)  [%s]%s\n\n",
    x$object_name, x$object_class, status, score_str
  ))

  if (length(x$checks) == 0L) {
    cat("  No checks performed.\n")
    return(invisible(x))
  }

  for (chk in x$checks) {
    mark <- if (is.na(chk$pass)) "[?]" else if (chk$pass) "[+]" else "[-]"
    cat(sprintf("  %s  %s\n", mark, chk$message))
  }

  if (!is.null(x$feedback) && nchar(x$feedback) > 0L) {
    cat(sprintf("\n  Feedback: %s\n", x$feedback))
  }

  invisible(x)
}


# -- Group resolution ----------------------------------------------------------

# Separates group names (unnamed list entries or character vector) from
# individual property checks (named list entries). Determines which groups to
# run against a reference based on checks, exclude, and per-type defaults.
#
# Returns: list(groups = character vector, checks = named list)
.resolve_groups <- function(checks, exclude, default_groups, all_groups) {
  if (is.character(checks)) {
    group_entries     <- checks
    individual_checks <- list()
  } else if (is.list(checks) && length(checks) > 0L) {
    nms               <- names(checks)
    if (is.null(nms)) nms <- rep("", length(checks))
    group_entries     <- unlist(checks[nms == ""], use.names = FALSE)
    individual_checks <- checks[nzchar(nms)]
  } else {
    group_entries     <- character(0L)
    individual_checks <- list()
  }

  if (length(group_entries) > 0L) {
    invalid <- setdiff(group_entries, all_groups)
    if (length(invalid) > 0L)
      warning(sprintf("Unknown group(s) ignored: %s",
                      paste(invalid, collapse = ", ")), call. = FALSE)
    groups_to_run <- intersect(group_entries, all_groups)
  } else if (length(individual_checks) == 0L) {
    groups_to_run <- default_groups
  } else {
    groups_to_run <- character(0L)
  }

  groups_to_run <- setdiff(groups_to_run, exclude %||% character(0L))
  list(groups = groups_to_run, checks = individual_checks)
}


# Backport of null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a
