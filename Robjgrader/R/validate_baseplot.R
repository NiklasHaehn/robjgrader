# Validator for base R plot objects (robjgrader_baseplot).
#
# Groups (for reference comparison):
#   aesthetics  col, pch, type, lty, lwd, cex, xlab, ylab, main, xlim, ylim
#   variables   x_expr and y_expr
#
# Individual checks:
#   plot_type   character -- "scatter", "line", "bar", "histogram", "boxplot",
#                            "density", "pie", "strip", "dotchart", "curve"
#   aes_x       character -- deparsed x-argument expression
#   aes_y       character -- deparsed y-argument expression
#   col         character -- plot color
#   color       alias for col
#   colour      alias for col
#   pch         integer -- point character
#   type        character -- "p", "l", "b", etc.
#   xlab        character -- x-axis label
#   ylab        character -- y-axis label
#   main        character -- plot title
#   lty         numeric -- line type
#   lwd         numeric -- line width
#   cex         numeric -- character expansion

.baseplot_default_groups <- c("aesthetics", "variables")
.baseplot_all_groups     <- c("aesthetics", "variables")

.validate_baseplot <- function(obj, reference, checks, exclude = NULL, name) {
  resolved <- .resolve_groups(checks, exclude,
                              .baseplot_default_groups, .baseplot_all_groups)
  results  <- list()
  cls      <- if (!is.null(obj$plot_type)) obj$plot_type else "baseplot"

  # -- Group checks (require reference) ----------------------------------------
  if (length(resolved$groups) > 0L) {
    if (is.null(reference))
      stop("'reference' is required for group-level base-R-plot checks.")
    if (!inherits(reference, "robjgrader_baseplot"))
      stop("'reference' must be a robjgrader_baseplot object.")

    for (grp in resolved$groups) {
      cmp <- .compare_baseplot_group(obj, reference, grp)
      results[[grp]] <- .make_check(grp, cmp$pass, cmp$expected, cmp$observed, cmp$msg)
    }
  }

  # -- Individual checks --------------------------------------------------------
  chk <- resolved$checks

  # plot_type
  if (!is.null(chk[["plot_type"]])) {
    sp       <- .parse_check_spec(chk[["plot_type"]]); expected <- sp$value
    observed <- obj$plot_type %||% "<unknown>"
    pass     <- isTRUE(observed == expected)
    results[["plot_type"]] <- .make_check(
      "plot_type", pass, expected, observed,
      if (pass) sprintf("plot_type correct: '%s'", expected)
      else      sprintf("plot_type: expected '%s', found '%s'", expected, observed),
      sp$weight
    )
  }

  # aes_x / aes_y
  for (key in c("aes_x", "aes_y")) {
    if (!is.null(chk[[key]])) {
      sp       <- .parse_check_spec(chk[[key]]); expected <- sp$value
      observed <- if (key == "aes_x") obj$x_expr else obj$y_expr
      pass     <- !is.null(observed) && observed == expected
      results[[key]] <- .make_check(
        key, pass, expected,
        observed %||% "<not provided>",
        if (pass)
          sprintf("%s: '%s' correct", key, expected)
        else if (is.null(observed))
          sprintf("%s: '%s' not found -- no %s argument", key, expected,
                  if (key == "aes_x") "x" else "y")
        else
          sprintf("%s: expected '%s', found '%s'", key, expected, observed),
        sp$weight
      )
    }
  }

  # Scalar aesthetic checks: col/color/colour, pch, type, xlab, ylab, main, lty, lwd, cex
  aes_check_keys <- c("col", "color", "colour", "pch", "type",
                      "xlab", "ylab", "main", "lty", "lwd", "cex")
  for (key in aes_check_keys) {
    if (!is.null(chk[[key]])) {
      sp         <- .parse_check_spec(chk[[key]]); expected <- sp$value
      lookup_key <- if (key %in% c("color", "colour")) "col" else key
      observed   <- obj$aes[[lookup_key]] %||% obj$aes[["color"]] %||% obj$aes[["colour"]]
      if (key == "col") observed <- obj$aes[["col"]] %||% obj$aes[["color"]] %||% obj$aes[["colour"]]
      pass       <- !is.null(observed) && identical(observed, expected)
      results[[key]] <- .make_check(
        key, pass, expected,
        observed %||% "<not set>",
        if (pass)
          sprintf("%s: '%s' correct", key, as.character(expected))
        else if (is.null(observed))
          sprintf("%s: '%s' not set in plot call", key, as.character(expected))
        else
          sprintf("%s: expected '%s', found '%s'", key,
                  as.character(expected), as.character(observed)),
        sp$weight
      )
    }
  }

  .make_result(name, "baseplot", cls, results)
}


# -- Group comparison helpers ---------------------------------------------------

.compare_baseplot_group <- function(obj, ref, group) {
  switch(group,
    aesthetics = {
      aes_keys <- c("col", "pch", "type", "lty", "lwd", "cex",
                    "xlab", "ylab", "main", "xlim", "ylim", "bg", "las",
                    "border", "breaks")
      common <- intersect(names(ref$aes), aes_keys)
      if (length(common) == 0L) {
        return(list(pass = TRUE, expected = list(), observed = list(),
                    msg = "no reference aesthetics to compare"))
      }
      mismatches <- character(0L)
      for (k in common) {
        obs_val <- obj$aes[[k]]
        ref_val <- ref$aes[[k]]
        if (is.null(obs_val) || !identical(obs_val, ref_val)) {
          obs_str <- if (is.null(obs_val)) "<not set>" else as.character(obs_val)
          mismatches <- c(mismatches,
            sprintf("%s: expected '%s', found '%s'",
                    k, as.character(ref_val), obs_str))
        }
      }
      pass <- length(mismatches) == 0L
      list(
        pass     = pass,
        expected = ref$aes[common],
        observed = obj$aes[common],
        msg      = if (pass) "aesthetics match reference"
                   else paste(mismatches, collapse = "; ")
      )
    },
    variables = {
      x_pass <- is.null(ref$x_expr) || isTRUE(obj$x_expr == ref$x_expr)
      y_pass <- is.null(ref$y_expr) || isTRUE(obj$y_expr == ref$y_expr)
      pass   <- x_pass && y_pass
      msgs   <- character(0L)
      if (!x_pass) msgs <- c(msgs, sprintf("x: expected '%s', found '%s'",
                                            ref$x_expr, obj$x_expr %||% "<none>"))
      if (!y_pass) msgs <- c(msgs, sprintf("y: expected '%s', found '%s'",
                                            ref$y_expr, obj$y_expr %||% "<none>"))
      list(
        pass     = pass,
        expected = list(x = ref$x_expr, y = ref$y_expr),
        observed = list(x = obj$x_expr, y = obj$y_expr),
        msg      = if (pass) "plot variables match reference"
                   else paste(msgs, collapse = "; ")
      )
    },
    list(pass = NA, expected = NULL, observed = NULL,
         msg = sprintf("group '%s' not implemented for baseplot", group))
  )
}
