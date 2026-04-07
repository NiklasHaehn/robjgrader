# Validator for data frames and tibbles.
#
# Groups (for reference comparison):
#   dimensions  nrow and ncol
#   names       column names as a set (order-insensitive)
#   col_order   exact column sequence
#   types       column classes
#   values      data values (always row-order-insensitive unless row_order active)
#   row_order   row ordering (opt-in; requires reference or sort key in checks)
#
# Individual checks:
#   nrow        integer
#   ncol        integer
#   names       character vector -- columns that must be present (subset)
#   col_order   TRUE or character vector -- TRUE: match reference; vector: exact sequence
#   col_types   named list (column -> class string)
#   values      named list (column -> expected vector)
#   row_order   TRUE or character vector -- TRUE: match reference; vector: sort columns

.df_default_groups <- c("dimensions", "names", "types", "values")
.df_all_groups     <- c("dimensions", "names", "col_order", "types", "values", "row_order")

.validate_df <- function(obj, reference, checks, exclude = NULL, name) {
  resolved <- .resolve_groups(checks, exclude, .df_default_groups, .df_all_groups)
  results  <- list()
  cls      <- class(obj)[1L]

  # -- Group checks (require reference) ----------------------------------------
  if (length(resolved$groups) > 0L) {
    if (is.null(reference))
      stop("'reference' is required for group-level df checks.")
    if (!inherits(reference, c("data.frame", "tbl_df", "tbl")))
      stop("'reference' must be a data frame or tibble.")

    for (grp in resolved$groups) {
      cmp <- .compare_df_group(obj, reference, grp)
      results[[grp]] <- .make_check(grp, cmp$pass, cmp$expected, cmp$observed, cmp$msg)
    }
  }

  # -- Individual checks --------------------------------------------------------
  chk <- resolved$checks

  if (!is.null(chk$nrow)) {
    sp  <- .parse_check_spec(chk$nrow); exp <- sp$value
    obs <- nrow(obj)
    results[["nrow"]] <- .make_check(
      "nrow", obs == exp, exp, obs,
      sprintf("nrow: expected %d, found %d", exp, obs), sp$weight
    )
  }

  if (!is.null(chk$ncol)) {
    sp  <- .parse_check_spec(chk$ncol); exp <- sp$value
    obs <- ncol(obj)
    results[["ncol"]] <- .make_check(
      "ncol", obs == exp, exp, obs,
      sprintf("ncol: expected %d, found %d", exp, obs), sp$weight
    )
  }

  if (!is.null(chk$names)) {
    sp      <- .parse_check_spec(chk$names); exp <- sp$value
    missing <- setdiff(exp, names(obj))
    pass    <- length(missing) == 0L
    results[["names"]] <- .make_check(
      "names", pass, exp, names(obj),
      if (pass) sprintf("all required columns present: %s", paste(exp, collapse = ", "))
      else      sprintf("missing columns: %s", paste(missing, collapse = ", ")),
      sp$weight
    )
  }

  if (!is.null(chk$col_order)) {
    sp  <- .parse_check_spec(chk$col_order); val <- sp$value
    if (isTRUE(val)) {
      if (is.null(reference)) stop("col_order = TRUE requires a reference object.")
      exp <- names(reference); obs <- names(obj)
    } else {
      exp <- val; obs <- names(obj)
    }
    pass <- identical(obs, exp)
    results[["col_order"]] <- .make_check(
      "col_order", pass, exp, obs,
      if (pass) "column order correct"
      else      sprintf("column order differs: expected {%s}, found {%s}",
                        paste(exp, collapse = ", "), paste(obs, collapse = ", ")),
      sp$weight
    )
  }

  if (!is.null(chk$col_types)) {
    sp  <- .parse_check_spec(chk$col_types); exp <- sp$value
    missing <- setdiff(names(exp), names(obj))
    if (length(missing) > 0L) {
      results[["col_types"]] <- .make_check(
        "col_types", FALSE, exp, NULL,
        sprintf("columns not found: %s", paste(missing, collapse = ", ")), sp$weight
      )
    } else {
      type_ok    <- vapply(names(exp), function(col) inherits(obj[[col]], exp[[col]]), logical(1L))
      mismatches <- vapply(names(type_ok)[!type_ok], function(col)
        sprintf("%s (expected %s, got %s)", col, exp[[col]], class(obj[[col]])[1L]),
        character(1L))
      results[["col_types"]] <- .make_check(
        "col_types", all(type_ok), exp,
        vapply(names(exp), function(col) class(obj[[col]])[1L], character(1L)),
        if (all(type_ok)) "all column types correct"
        else              sprintf("type mismatch: %s", paste(mismatches, collapse = "; ")),
        sp$weight
      )
    }
  }

  if (!is.null(chk$values)) {
    sp  <- .parse_check_spec(chk$values); exp <- sp$value
    col_checks <- lapply(names(exp), function(col) {
      if (!col %in% names(obj))
        return(list(pass = FALSE, msg = sprintf("column '%s' not found", col)))
      eq <- isTRUE(all.equal(obj[[col]], exp[[col]], tolerance = 1e-6, check.attributes = FALSE))
      list(pass = eq,
           msg  = if (eq) sprintf("'%s' values match", col)
                  else    sprintf("'%s' values differ", col))
    })
    pass <- all(vapply(col_checks, `[[`, logical(1L), "pass"))
    results[["values"]] <- .make_check(
      "values", pass, exp, NULL,
      if (pass) "all specified column values match"
      else paste(vapply(Filter(function(r) !r$pass, col_checks), `[[`, character(1L), "msg"),
                 collapse = "; "),
      sp$weight
    )
  }

  if (!is.null(chk$row_order)) {
    sp  <- .parse_check_spec(chk$row_order); val <- sp$value
    if (isTRUE(val)) {
      if (is.null(reference)) stop("row_order = TRUE requires a reference object.")
      obs_keys <- do.call(paste, as.list(obj))
      ref_keys <- do.call(paste, as.list(reference))
      pass     <- identical(obs_keys, ref_keys)
      results[["row_order"]] <- .make_check(
        "row_order", pass, "<reference order>", NULL,
        if (pass) "row order matches reference"
        else      "row order does not match reference",
        sp$weight
      )
    } else {
      sort_cols <- val
      missing   <- setdiff(sort_cols, names(obj))
      if (length(missing) > 0L) {
        results[["row_order"]] <- .make_check(
          "row_order", FALSE, sort_cols, NULL,
          sprintf("sort columns not found: %s", paste(missing, collapse = ", ")), sp$weight
        )
      } else {
        sorted  <- obj[do.call(order, as.list(obj[sort_cols])), , drop = FALSE]
        pass    <- identical(unname(as.list(obj[sort_cols])), unname(as.list(sorted[sort_cols])))
        results[["row_order"]] <- .make_check(
          "row_order", pass, sort_cols, NULL,
          if (pass) sprintf("rows correctly sorted by: %s", paste(sort_cols, collapse = ", "))
          else      sprintf("rows not sorted by: %s", paste(sort_cols, collapse = ", ")),
          sp$weight
        )
      }
    }
  }

  .make_result(name, "df", cls, results)
}


# -- Group comparison helpers ---------------------------------------------------

.compare_df_group <- function(obj, ref, group) {
  switch(group,
    dimensions = {
      pass <- nrow(obj) == nrow(ref) && ncol(obj) == ncol(ref)
      list(pass = pass,
           expected = sprintf("%d x %d", nrow(ref), ncol(ref)),
           observed = sprintf("%d x %d", nrow(obj), ncol(obj)),
           msg = if (pass) sprintf("dimensions correct (%d x %d)", nrow(ref), ncol(ref))
                 else      sprintf("dimensions differ: expected %d x %d, found %d x %d",
                                   nrow(ref), ncol(ref), nrow(obj), ncol(obj)))
    },
    names = {
      missing <- setdiff(names(ref), names(obj))
      extra   <- setdiff(names(obj), names(ref))
      pass    <- length(missing) == 0L && length(extra) == 0L
      list(pass = pass,
           expected = names(ref), observed = names(obj),
           msg = if (pass) "column names match"
                 else sprintf("column name mismatch -- missing: {%s}, extra: {%s}",
                               paste(missing, collapse = ", "),
                               paste(extra,   collapse = ", ")))
    },
    col_order = {
      pass <- identical(names(obj), names(ref))
      list(pass = pass,
           expected = names(ref), observed = names(obj),
           msg = if (pass) "column order matches reference"
                 else sprintf("column order differs: expected {%s}, found {%s}",
                               paste(names(ref), collapse = ", "),
                               paste(names(obj), collapse = ", ")))
    },
    types = {
      common     <- intersect(names(ref), names(obj))
      type_ok    <- vapply(common, function(col)
        identical(class(obj[[col]])[1L], class(ref[[col]])[1L]), logical(1L))
      pass       <- all(type_ok)
      mismatches <- vapply(common[!type_ok], function(col)
        sprintf("%s (expected %s, got %s)", col,
                class(ref[[col]])[1L], class(obj[[col]])[1L]), character(1L))
      list(pass = pass, expected = NULL, observed = NULL,
           msg = if (pass) "all column types match reference"
                 else sprintf("type mismatch: %s", paste(mismatches, collapse = "; ")))
    },
    values = {
      common  <- intersect(names(ref), names(obj))
      obs_s   <- tryCatch(.sort_df(obj[, common, drop = FALSE]),  error = function(e) obj[, common, drop = FALSE])
      ref_s   <- tryCatch(.sort_df(ref[, common, drop = FALSE]),  error = function(e) ref[, common, drop = FALSE])
      rownames(obs_s) <- rownames(ref_s) <- NULL
      eq      <- isTRUE(all.equal(obs_s, ref_s, tolerance = 1e-6, check.attributes = FALSE))
      list(pass = eq, expected = NULL, observed = NULL,
           msg = if (eq) "values match reference (row-order-insensitive)"
                 else {
                   d <- all.equal(obs_s, ref_s, tolerance = 1e-6, check.attributes = FALSE)
                   sprintf("values differ: %s", d[[1L]])
                 })
    },
    row_order = {
      obs_keys <- do.call(paste, as.list(obj))
      ref_keys <- do.call(paste, as.list(ref))
      pass     <- identical(obs_keys, ref_keys)
      list(pass = pass, expected = NULL, observed = NULL,
           msg = if (pass) "row order matches reference"
                 else      "row order does not match reference")
    },
    list(pass = NA, expected = NULL, observed = NULL,
         msg = sprintf("group '%s' not implemented", group))
  )
}

.sort_df <- function(df) df[do.call(order, as.list(df)), , drop = FALSE]
