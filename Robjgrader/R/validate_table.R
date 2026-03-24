# Validator for table objects (gt_tbl, tinytable, flextable, huxtable).
#
# All table classes are first reduced to a canonical representation:
#   nrow       integer -- number of body rows
#   ncol       integer -- number of body columns (excluding the label column)
#   row_labels character vector -- body row labels (first column / stub)
#   col_labels character vector -- column header labels
#
# Groups (for reference comparison):
#   dimensions  body nrow and ncol
#   terms       row labels present (covers both coefficients and GOF rows)
#   models      number of model columns (ncol)
#   labels      column header labels
#
# Individual checks:
#   nrow         integer -- expected number of body rows
#   ncol         integer -- expected number of body columns
#   n_models     integer -- expected number of model columns (ncol)
#   terms        character vector -- labels that must be present as body rows
#   terms_absent character vector -- labels that must NOT be present
#   col_labels   character vector -- expected column header labels (exact match)

.table_default_groups <- c("dimensions", "terms", "models", "gof", "inference")
.table_all_groups     <- c("dimensions", "terms", "models", "gof", "inference", "labels")

# Known GOF statistic label patterns (modelsummary / broom convention).
# A row label matches if it exactly equals one of these, or starts with "FE:".
.gof_exact <- c(
  "Num.Obs.", "N", "Obs.",
  "R2", "R2 Adj.", "R2 Adj", "R2 Within", "R2 Between", "R2 Pseudo",
  "R2 Within Adj.", "Adj. R-Squared", "R-Squared", "R-squared",
  "AIC", "BIC", "Log.Lik.", "RMSE", "Sigma",
  "F", "F-stat", "p",
  "Std.Errors", "Std.Error", "SE type",
  "Num.FE", "Num.Clusters", "Num.Groups",
  "ICC", "Deviance", "DF.residual", "DF Residual", "Within R2"
)

.is_gof_label <- function(x) {
  x %in% .gof_exact | grepl("^FE:", x)
}

.detect_gof_labels <- function(row_labels) {
  row_labels[.is_gof_label(row_labels)]
}

.detect_inference_type <- function(val) {
  if (is.null(val) || !nzchar(trimws(val %||% ""))) return(NULL)
  val <- trimws(val)
  if (startsWith(val, "[")) return("ci")
  if (startsWith(val, "(")) return("se")
  NULL
}

.validate_table <- function(obj, reference, checks, exclude = NULL, name) {
  resolved <- .resolve_groups(checks, exclude, .table_default_groups, .table_all_groups)
  results  <- list()
  cls      <- class(obj)[1L]

  info <- tryCatch(
    .table_canonical(obj),
    error = function(e)
      stop(sprintf("Could not extract table information from '%s' object: %s",
                   cls, conditionMessage(e)))
  )

  # -- Group checks (require reference) ----------------------------------------
  if (length(resolved$groups) > 0L) {
    if (is.null(reference))
      stop("'reference' is required for group-level table checks.")
    if (!inherits(reference, c("gt_tbl", "tinytable", "flextable", "huxtable")))
      stop("'reference' must be a table object (gt_tbl, tinytable, flextable, huxtable).")

    ref_info <- tryCatch(
      .table_canonical(reference),
      error = function(e)
        stop(sprintf("Could not extract table information from reference: %s",
                     conditionMessage(e)))
    )

    for (grp in resolved$groups) {
      cmp <- .compare_table_group(info, ref_info, grp)
      results[[grp]] <- .make_check(grp, cmp$pass, cmp$expected, cmp$observed, cmp$msg)
    }
  }

  # -- Individual checks --------------------------------------------------------
  chk <- resolved$checks

  if (!is.null(chk$nrow)) {
    pass <- !is.na(info$nrow) && info$nrow == chk$nrow
    results[["nrow"]] <- .make_check(
      "nrow", pass, chk$nrow, info$nrow,
      sprintf("nrow: expected %d, found %s", chk$nrow,
              if (is.na(info$nrow)) "<unknown>" else as.character(info$nrow))
    )
  }

  if (!is.null(chk$ncol)) {
    pass <- !is.na(info$ncol) && info$ncol == chk$ncol
    results[["ncol"]] <- .make_check(
      "ncol", pass, chk$ncol, info$ncol,
      sprintf("ncol: expected %d, found %s", chk$ncol,
              if (is.na(info$ncol)) "<unknown>" else as.character(info$ncol))
    )
  }

  if (!is.null(chk$n_models)) {
    pass <- !is.na(info$ncol) && info$ncol == chk$n_models
    results[["n_models"]] <- .make_check(
      "n_models", pass, chk$n_models, info$ncol,
      sprintf("n_models: expected %d, found %s", chk$n_models,
              if (is.na(info$ncol)) "<unknown>" else as.character(info$ncol))
    )
  }

  if (!is.null(chk$terms)) {
    missing <- setdiff(chk$terms, info$row_labels)
    pass    <- length(missing) == 0L
    results[["terms"]] <- .make_check(
      "terms", pass, chk$terms, info$row_labels,
      if (pass) sprintf("required term(s) present: %s", paste(chk$terms, collapse = ", "))
      else      sprintf("missing term(s): %s",          paste(missing,    collapse = ", "))
    )
  }

  if (!is.null(chk$terms_absent)) {
    present <- intersect(chk$terms_absent, info$row_labels)
    pass    <- length(present) == 0L
    results[["terms_absent"]] <- .make_check(
      "terms_absent", pass, chk$terms_absent, info$row_labels,
      if (pass) sprintf("term(s) correctly absent: %s", paste(chk$terms_absent, collapse = ", "))
      else      sprintf("term(s) unexpectedly present: %s", paste(present, collapse = ", "))
    )
  }

  if (!is.null(chk$col_labels)) {
    pass <- identical(info$col_labels, chk$col_labels)
    results[["col_labels"]] <- .make_check(
      "col_labels", pass, chk$col_labels, info$col_labels,
      if (pass) "column labels correct"
      else      sprintf("column label mismatch: expected {%s}, found {%s}",
                        paste(chk$col_labels, collapse = ", "),
                        paste(info$col_labels, collapse = ", "))
    )
  }

  if (!is.null(chk$gof)) {
    missing <- setdiff(chk$gof, info$row_labels)
    pass    <- length(missing) == 0L
    results[["gof"]] <- .make_check(
      "gof", pass, chk$gof, info$row_labels,
      if (pass) sprintf("required GOF row(s) present: %s", paste(chk$gof, collapse = ", "))
      else      sprintf("missing GOF row(s): %s", paste(missing, collapse = ", "))
    )
  }

  if (!is.null(chk$inference)) {
    obs  <- info$inference_type
    pass <- !is.null(obs) && obs == chk$inference
    results[["inference"]] <- .make_check(
      "inference", pass, chk$inference, obs %||% "<undetectable>",
      if (pass) sprintf("inference type correct: '%s'", chk$inference)
      else if (is.null(obs))
        sprintf("inference: expected '%s', could not detect type from table structure",
                chk$inference)
      else
        sprintf("inference: expected '%s', found '%s'", chk$inference, obs)
    )
  }

  .make_result(name, "table", cls, results)
}


# -- Group comparison helpers ---------------------------------------------------

.compare_table_group <- function(info, ref_info, group) {
  switch(group,
    dimensions = {
      pass <- isTRUE(info$nrow == ref_info$nrow) && isTRUE(info$ncol == ref_info$ncol)
      list(
        pass     = pass,
        expected = sprintf("%d x %d", ref_info$nrow, ref_info$ncol),
        observed = sprintf("%d x %d", info$nrow, info$ncol),
        msg      = if (pass)
          sprintf("dimensions correct (%d x %d)", ref_info$nrow, ref_info$ncol)
        else
          sprintf("dimensions differ: expected %d x %d, found %d x %d",
                  ref_info$nrow, ref_info$ncol, info$nrow, info$ncol)
      )
    },
    terms = {
      missing <- setdiff(ref_info$row_labels, info$row_labels)
      extra   <- setdiff(info$row_labels,     ref_info$row_labels)
      pass    <- length(missing) == 0L && length(extra) == 0L
      list(
        pass     = pass,
        expected = ref_info$row_labels,
        observed = info$row_labels,
        msg      = if (pass) "row labels match reference"
                   else sprintf("row label mismatch -- missing: {%s}, extra: {%s}",
                                 paste(missing, collapse = ", "),
                                 paste(extra,   collapse = ", "))
      )
    },
    models = {
      pass <- isTRUE(info$ncol == ref_info$ncol)
      list(
        pass     = pass,
        expected = ref_info$ncol,
        observed = info$ncol,
        msg      = if (pass)
          sprintf("model count matches reference: %d", ref_info$ncol)
        else
          sprintf("model count differs: expected %d, found %d",
                  ref_info$ncol %||% NA, info$ncol %||% NA)
      )
    },
    gof = {
      missing <- setdiff(ref_info$gof_labels, info$gof_labels)
      extra   <- setdiff(info$gof_labels,     ref_info$gof_labels)
      pass    <- length(missing) == 0L && length(extra) == 0L
      list(
        pass     = pass,
        expected = ref_info$gof_labels,
        observed = info$gof_labels,
        msg      = if (pass) {
          if (length(ref_info$gof_labels) == 0L) "no GOF rows detected (as expected)"
          else sprintf("GOF rows match reference: {%s}",
                       paste(ref_info$gof_labels, collapse = ", "))
        } else sprintf("GOF row mismatch -- missing: {%s}, extra: {%s}",
                        paste(missing, collapse = ", "),
                        paste(extra,   collapse = ", "))
      )
    },
    inference = {
      obj_inf <- info$inference_type
      ref_inf <- ref_info$inference_type
      if (is.null(ref_inf)) {
        list(pass = TRUE, expected = "<unknown>", observed = obj_inf %||% "<unknown>",
             msg = "reference has no detectable inference type; check skipped")
      } else {
        pass <- isTRUE(obj_inf == ref_inf)
        list(pass = pass, expected = ref_inf, observed = obj_inf %||% "<undetectable>",
             msg = if (pass) sprintf("inference type matches: '%s'", ref_inf)
                   else      sprintf("inference type: expected '%s', found '%s'",
                                     ref_inf, obj_inf %||% "<undetectable>"))
      }
    },
    labels = {
      pass <- identical(info$col_labels, ref_info$col_labels)
      list(
        pass     = pass,
        expected = ref_info$col_labels,
        observed = info$col_labels,
        msg      = if (pass) "column labels match reference"
                   else sprintf("column label mismatch -- expected {%s}, found {%s}",
                                 paste(ref_info$col_labels, collapse = ", "),
                                 paste(info$col_labels,     collapse = ", "))
      )
    },
    list(pass = NA, expected = NULL, observed = NULL,
         msg = sprintf("group '%s' not implemented for tables", group))
  )
}


# -- Canonical extraction dispatcher -------------------------------------------

.table_canonical <- function(obj) {
  info <- if (inherits(obj, "gt_tbl"))    .canonical_gt(obj)
          else if (inherits(obj, "flextable")) .canonical_flextable(obj)
          else if (inherits(obj, "tinytable")) .canonical_tinytable(obj)
          else if (inherits(obj, "huxtable"))  .canonical_huxtable(obj)
          else stop(sprintf("No table adapter for class '%s'", class(obj)[1L]))

  info$gof_labels <- .detect_gof_labels(info$row_labels)
  if (is.null(info$inference_type)) info$inference_type <- NULL
  info
}


# -- gt backend ----------------------------------------------------------------

.canonical_gt <- function(obj) {
  # Body data: try public export path first, fall back to internal slot.
  body_df <- tryCatch(
    {
      # gt >= 0.9 stores data in internal functions; accessed via :::
      gt:::dt_data_get(obj)
    },
    error = function(e) {
      tryCatch(obj[["_data"]], error = function(e2) NULL)
    }
  )

  if (!is.data.frame(body_df) || nrow(body_df) == 0L) {
    return(list(nrow = NA_integer_, ncol = NA_integer_,
                row_labels = character(0L), col_labels = character(0L)))
  }

  # Row labels: the stub column (first column of the body data frame)
  row_labels <- as.character(body_df[[1L]])

  # Model columns: all body columns except the stub (first) column
  n_model_cols <- ncol(body_df) - 1L

  # Column header labels from the boxhead
  col_labels <- tryCatch({
    bh  <- gt:::dt_boxhead_get(obj)
    lbl <- vapply(bh$column_label,
                  function(x) as.character(x[[1L]]),
                  character(1L))
    lbl[-1L]  # drop stub column label
  }, error = function(e) character(0L))

  inference_type <- tryCatch({
    if (nrow(body_df) >= 2L && ncol(body_df) >= 2L)
      .detect_inference_type(as.character(body_df[2L, 2L]))
    else NULL
  }, error = function(e) NULL)

  list(
    nrow           = nrow(body_df),
    ncol           = n_model_cols,
    row_labels     = row_labels,
    col_labels     = col_labels,
    inference_type = inference_type
  )
}


# -- flextable backend ---------------------------------------------------------

.canonical_flextable <- function(obj) {
  body_df <- tryCatch(obj$body$dataset, error = function(e) NULL)

  if (!is.data.frame(body_df)) {
    return(list(nrow = NA_integer_, ncol = NA_integer_,
                row_labels = character(0L), col_labels = character(0L)))
  }

  row_labels   <- as.character(body_df[[1L]])
  n_model_cols <- ncol(body_df) - 1L

  # Column labels: last row of header dataset (innermost header)
  col_labels <- tryCatch({
    hdr <- obj$header$dataset
    as.character(unlist(hdr[nrow(hdr), -1L, drop = TRUE]))
  }, error = function(e) character(0L))

  inference_type <- tryCatch({
    if (nrow(body_df) >= 2L && ncol(body_df) >= 2L)
      .detect_inference_type(as.character(body_df[2L, 2L]))
    else NULL
  }, error = function(e) NULL)

  list(
    nrow           = nrow(body_df),
    ncol           = n_model_cols,
    row_labels     = row_labels,
    col_labels     = col_labels,
    inference_type = inference_type
  )
}


# -- tinytable backend ---------------------------------------------------------

.canonical_tinytable <- function(obj) {
  # tinytable is an R6/S3 hybrid. The body is a character matrix or data frame
  # in obj$body; dimensions in obj$nrow / obj$ncol.
  body <- tryCatch(obj$body, error = function(e) NULL)

  if (is.null(body)) {
    return(list(nrow = NA_integer_, ncol = NA_integer_,
                row_labels = character(0L), col_labels = character(0L)))
  }

  if (is.data.frame(body) || is.matrix(body)) {
    row_labels   <- as.character(body[, 1L])
    n_model_cols <- ncol(body) - 1L
  } else {
    row_labels   <- character(0L)
    n_model_cols <- NA_integer_
  }

  # Column headers: obj$names or obj$header
  col_labels <- tryCatch({
    hdr <- obj$names %||% obj$header
    if (!is.null(hdr)) as.character(hdr)[-1L] else character(0L)
  }, error = function(e) character(0L))

  inference_type <- tryCatch({
    if (!is.null(body) && (is.data.frame(body) || is.matrix(body)) &&
        nrow(body) >= 2L && ncol(body) >= 2L)
      .detect_inference_type(as.character(body[2L, 2L]))
    else NULL
  }, error = function(e) NULL)

  list(
    nrow           = tryCatch(obj$nrow, error = function(e) NA_integer_),
    ncol           = n_model_cols,
    row_labels     = row_labels,
    col_labels     = col_labels,
    inference_type = inference_type
  )
}


# -- huxtable backend ----------------------------------------------------------

.canonical_huxtable <- function(obj) {
  # huxtable stores content as a matrix; header rows are flagged in header_rows
  body_mat <- tryCatch(as.matrix(obj), error = function(e) NULL)

  if (is.null(body_mat)) {
    return(list(nrow = NA_integer_, ncol = NA_integer_,
                row_labels = character(0L), col_labels = character(0L)))
  }

  # Detect header rows (huxtable stores header_rows attribute)
  n_header <- tryCatch({
    attr(obj, "header_rows") %||% 1L
  }, error = function(e) 1L)

  body_rows  <- seq(n_header + 1L, nrow(body_mat))
  row_labels <- if (length(body_rows) > 0L)
    as.character(body_mat[body_rows, 1L])
  else
    character(0L)

  col_labels <- if (n_header >= 1L)
    as.character(body_mat[n_header, -1L])
  else
    character(0L)

  inference_type <- tryCatch({
    if (length(body_rows) >= 2L && ncol(body_mat) >= 2L)
      .detect_inference_type(as.character(body_mat[body_rows[2L], 2L]))
    else NULL
  }, error = function(e) NULL)

  list(
    nrow           = length(body_rows),
    ncol           = ncol(body_mat) - 1L,
    row_labels     = row_labels,
    col_labels     = col_labels,
    inference_type = inference_type
  )
}
