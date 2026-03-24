# Package-level recorder state
.recorder_env <- new.env(parent = emptyenv())
.recorder_env$active  <- FALSE
.recorder_env$records <- list()
.recorder_env$counter <- 0L
.recorder_env$config  <- list()
.recorder_env$envir   <- NULL   # evaluation environment; set on each recording start

.recordable_types <- c("df", "ggplot", "model", "table")


#' Start recording analytical objects
#'
#' Attaches a task callback that intercepts top-level assignments, visible
#' returns, and explicit print() calls for supported object classes.
#'
#' @param record_df      Record data frames and tibbles. Default TRUE.
#' @param record_ggplot  Record ggplot objects. Default TRUE.
#' @param record_model   Record model objects (lm, glm, fixest). Default TRUE.
#' @param record_table   Record table objects (gt, tinytable, flextable,
#'   huxtable). Default TRUE.
#'
#' @return Invisibly NULL. Called for its side effect.
#' @export
record_start <- function(
  record_df     = TRUE,
  record_ggplot = TRUE,
  record_model  = TRUE,
  record_table  = TRUE
) {
  if (.recorder_env$active) {
    warning("Recording is already active. Call record_stop() first.")
    return(invisible(NULL))
  }

  .recorder_env$active  <- TRUE
  .recorder_env$records <- list()
  .recorder_env$counter <- 0L
  .recorder_env$config  <- list(
    df     = record_df,
    ggplot = record_ggplot,
    model  = record_model,
    table  = record_table
  )
  .recorder_env$envir <- .GlobalEnv

  addTaskCallback(.recorder_callback, name = "robjgrader_recorder")
  message("Recording started.")
  invisible(NULL)
}


#' Stop recording and summarise captured objects
#'
#' Removes the task callback installed by \code{record_start()}.
#'
#' @return Invisibly NULL. Called for its side effect.
#' @export
record_stop <- function() {
  if (!.recorder_env$active) {
    warning("No active recording.")
    return(invisible(NULL))
  }

  removeTaskCallback("robjgrader_recorder")
  .recorder_env$active <- FALSE

  n <- length(.recorder_env$records)
  message(sprintf("Recording stopped. %d object(s) captured.", n))
  invisible(NULL)
}


#' Retrieve recorded objects
#'
#' Returns a list of recorded events. Each element contains event metadata
#' and the captured object under \code{$object}.
#'
#' @param type  Character vector of object types to include. One or more of
#'   \code{"df"}, \code{"ggplot"}, \code{"model"}, \code{"table"}.
#'   \code{NULL} (default) returns all types.
#' @param name  Character vector of object names to include. Matches the
#'   left-hand side of assignments. \code{NULL} (default) returns all names,
#'   including anonymous visible returns.
#'
#' @return An object of class \code{robjgrader_records} (a named list of
#'   record entries).
#' @export
get_records <- function(type = NULL, name = NULL) {
  records <- .recorder_env$records

  if (!is.null(type)) {
    type    <- match.arg(type, .recordable_types, several.ok = TRUE)
    records <- Filter(function(r) r$object_type %in% type, records)
  }

  if (!is.null(name)) {
    records <- Filter(
      function(r) !is.null(r$object_name) && r$object_name %in% name,
      records
    )
  }

  structure(records, class = "robjgrader_records")
}


#' @export
print.robjgrader_records <- function(x, ...) {
  n <- length(x)

  if (n == 0L) {
    cat("No records found.\n")
    return(invisible(x))
  }

  cat(sprintf("robjgrader records: %d object(s)\n\n", n))
  cat(sprintf("  %-4s  %-16s  %-10s  %-22s  %s\n",
              "#", "name", "type", "class", "event"))

  for (i in seq_along(x)) {
    r        <- x[[i]]
    name_str <- if (!is.null(r$object_name)) r$object_name else "<anonymous>"
    cat(sprintf("  %-4d  %-16s  %-10s  %-22s  %s\n",
                r$event_id, name_str, r$object_type,
                r$object_class, r$event_type))
  }

  invisible(x)
}


#' Record all objects produced by sourcing an R script
#'
#' Parses \code{path} with \code{parse()}, evaluates each expression one at a
#' time inside \code{envir}, and passes the result directly to the recorder
#' callback -- bypassing \code{addTaskCallback}, which is never fired during
#' \code{source()}.  This is the reliable way to record objects from a student
#' submission file.
#'
#' @param path          Path to the R script to evaluate.
#' @param envir         Environment in which to evaluate expressions. Default
#'   \code{.GlobalEnv}.
#' @param stop_on_error Logical. If \code{TRUE} the first error in the script
#'   halts evaluation and re-throws.  If \code{FALSE} (default) each failing
#'   expression emits a \code{warning()} and evaluation continues.
#' @param record_df      Record data frames and tibbles. Default \code{TRUE}.
#' @param record_ggplot  Record ggplot objects. Default \code{TRUE}.
#' @param record_model   Record model objects. Default \code{TRUE}.
#' @param record_table   Record table objects. Default \code{TRUE}.
#'
#' @return A \code{robjgrader_records} object, returned invisibly.
#' @export
record_script <- function(
  path,
  envir         = .GlobalEnv,
  stop_on_error = FALSE,
  record_df     = TRUE,
  record_ggplot = TRUE,
  record_model  = TRUE,
  record_table  = TRUE
) {
  if (!file.exists(path))
    stop(sprintf("File not found: '%s'", path))
  if (.recorder_env$active)
    stop("A recording is already active. Call record_stop() first.")

  .recorder_env$active  <- TRUE
  .recorder_env$records <- list()
  .recorder_env$counter <- 0L
  .recorder_env$config  <- list(
    df     = record_df,
    ggplot = record_ggplot,
    model  = record_model,
    table  = record_table
  )
  .recorder_env$envir <- envir

  on.exit({
    .recorder_env$active <- FALSE
    message(sprintf("Recording stopped. %d object(s) captured.",
                    length(.recorder_env$records)))
  }, add = TRUE)

  exprs <- parse(file = path)

  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    ev <- tryCatch(
      {
        vr <- withVisible(eval(expr, envir = envir))
        list(value = vr$value, visible = vr$visible, ok = TRUE)
      },
      error = function(e) {
        if (stop_on_error)
          stop(sprintf("Error in expression %d: %s", i, conditionMessage(e)),
               call. = FALSE)
        warning(sprintf("Skipping expression %d: %s", i, conditionMessage(e)),
                call. = FALSE)
        list(value = NULL, visible = FALSE, ok = FALSE)
      }
    )
    .recorder_callback(expr = expr, value = ev$value,
                       ok = ev$ok, visible = ev$visible)
  }

  invisible(structure(.recorder_env$records, class = "robjgrader_records"))
}


# ── Internal helpers ──────────────────────────────────────────────────────────

.recorder_callback <- function(expr, value, ok, visible) {
  if (!ok) return(TRUE)

  # For explicit print() calls, print.*() returns something other than the
  # original object for all table classes (gt -> shiny.tag, flextable -> NULL,
  # tinytable -> tinytable_grid).  Recover the original object two ways:
  #   (a) argument is a bare name  -> look it up in the recording environment
  #   (b) argument is an inline call -> re-evaluate it in the recording environment
  if (.is_print_call(expr) && length(expr) >= 2L) {
    arg <- expr[[2L]]
    ev  <- .recorder_env$envir %||% globalenv()
    if (is.name(arg)) {
      recovered <- tryCatch(
        get(as.character(arg), envir = ev, inherits = TRUE),
        error = function(e) NULL
      )
      if (!is.null(recovered)) value <- recovered
    } else if (is.call(arg)) {
      recovered <- tryCatch(eval(arg, envir = ev), error = function(e) NULL)
      if (!is.null(recovered)) value <- recovered
    }
  }

  if (is.null(value)) return(TRUE)

  obj_type <- .classify_object(value, .recorder_env$config)
  if (is.null(obj_type)) return(TRUE)

  obj_name <- .extract_assign_name(expr)

  event_type <- if (!is.null(obj_name)) {
    "assignment"
  } else if (visible) {
    "visible_return"
  } else if (.is_print_call(expr)) {
    "print_call"
  } else if (obj_type == "table") {
    # Table-generating functions (e.g. modelsummary) often return their object
    # invisibly. Capture anyway -- invisible table output is almost always
    # intentional student output, unlike invisible dfs or models.
    "invisible_return"
  } else {
    return(TRUE)
  }

  .recorder_env$counter <- .recorder_env$counter + 1L
  .recorder_env$records[[.recorder_env$counter]] <- list(
    event_id     = .recorder_env$counter,
    event_type   = event_type,
    object_name  = obj_name,
    object_type  = obj_type,
    object_class = class(value)[1L],
    expr_text    = deparse(expr, nlines = 1L),
    timestamp    = Sys.time(),
    object       = value
  )

  TRUE
}


.classify_object <- function(value, config) {
  if (config$ggplot && inherits(value, "ggplot"))                             return("ggplot")
  if (config$df     && inherits(value, c("data.frame", "tbl_df", "tbl")))     return("df")
  if (config$model  && inherits(value, c("lm", "glm", "fixest", "lmerMod", "glmerMod"))) return("model")
  if (config$table  && inherits(value, c("gt_tbl", "tinytable", "flextable", "huxtable"))) return("table")
  NULL
}


.extract_assign_name <- function(expr) {
  if (!is.call(expr)) return(NULL)
  fn <- expr[[1L]]
  if (identical(fn, quote(`<-`)) ||
      identical(fn, quote(`=`))  ||
      identical(fn, quote(`<<-`))) {
    lhs <- expr[[2L]]
    if (is.name(lhs)) return(as.character(lhs))
  }
  NULL
}


.is_print_call <- function(expr) {
  if (!is.call(expr)) return(FALSE)
  as.character(expr[[1L]]) %in% c("print", "show")
}
