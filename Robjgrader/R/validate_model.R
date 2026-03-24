# Validator for model objects (lm, glm, feols/feglm from fixest).
#
# Groups (for reference comparison):
#   estimator    model class; family and link for GLMs
#   outcome      dependent variable
#   predictors   RHS main-effect variables (presence and count)
#   interactions interaction terms
#   effects      fixed effect variables
#   inference    clustering variables and SE type
#   weights      weighting variable
#   sample       number of observations and model frame
#
# Individual checks:
#   estimator    character -- e.g. "lm", "glm", "feols"
#   family       character -- GLM family, e.g. "binomial"
#   link         character -- GLM link, e.g. "logit"
#   outcome      character -- dependent variable name
#   predictors   character vector -- must appear as main effects on RHS
#   n_predictors integer -- exact number of main-effect predictors
#   predictors_only logical -- if TRUE, no extra predictors allowed
#   interactions character vector -- interaction terms (order-insensitive)
#   n_interactions integer -- exact number of interaction terms
#   fixed_effects character vector -- FE variables (fixest only)
#   n_fixed_effects integer -- exact number of FE dimensions
#   has_fe       logical -- whether any FE are present
#   cluster      character vector -- clustering variables
#   vcov         character -- SE type: "iid", "HC1", "cluster", "twoway"
#   weights      character -- name of weighting variable
#   has_weights  logical -- whether any weights are used
#   nobs         integer -- expected number of observations
#   data         character -- name of data object in model call

.model_default_groups <- c("estimator", "outcome", "predictors", "interactions",
                            "effects", "inference", "weights", "sample", "random_effects")
.model_all_groups     <- .model_default_groups

.validate_model <- function(obj, reference, checks, exclude = NULL, name) {
  resolved  <- .resolve_groups(checks, exclude, .model_default_groups, .model_all_groups)
  results   <- list()
  cls       <- class(obj)[1L]
  is_fixest <- inherits(obj, "fixest")
  is_glm    <- inherits(obj, "glm") && !is_fixest
  is_glmer  <- inherits(obj, "glmerMod")
  is_lmer   <- inherits(obj, "lmerMod") && !is_glmer
  is_lme4   <- is_lmer || is_glmer

  # -- Group checks (require reference) ----------------------------------------
  if (length(resolved$groups) > 0L) {
    if (is.null(reference))
      stop("'reference' is required for group-level model checks.")
    if (!inherits(reference, c("lm", "glm", "fixest", "lmerMod", "glmerMod")))
      stop("'reference' must be a model object (lm, glm, fixest, lmerMod, or glmerMod).")

    for (grp in resolved$groups) {
      cmp <- .compare_model_group(obj, reference, grp)
      results[[grp]] <- .make_check(grp, cmp$pass, cmp$expected, cmp$observed, cmp$msg)
    }
  }

  # -- Individual checks --------------------------------------------------------
  chk <- resolved$checks

  if (!is.null(chk$estimator)) {
    pass <- inherits(obj, chk$estimator)
    results[["estimator"]] <- .make_check(
      "estimator", pass, chk$estimator, cls,
      sprintf("estimator: expected '%s', found '%s'", chk$estimator, cls)
    )
  }

  if (!is.null(chk$family)) {
    obs_fam <- if (is_glm || is_glmer) family(obj)$family else "<not a glm>"
    pass    <- obs_fam == chk$family
    results[["family"]] <- .make_check(
      "family", pass, chk$family, obs_fam,
      sprintf("family: expected '%s', found '%s'", chk$family, obs_fam)
    )
  }

  if (!is.null(chk$link)) {
    obs_link <- if (is_glm || is_glmer) family(obj)$link else "<not a glm>"
    pass     <- obs_link == chk$link
    results[["link"]] <- .make_check(
      "link", pass, chk$link, obs_link,
      sprintf("link: expected '%s', found '%s'", chk$link, obs_link)
    )
  }

  if (!is.null(chk$outcome)) {
    obs_out <- .get_model_outcome(obj)
    pass    <- !is.null(obs_out) && obs_out == chk$outcome
    results[["outcome"]] <- .make_check(
      "outcome", pass, chk$outcome,
      obs_out %||% "<unknown>",
      sprintf("outcome: expected '%s', found '%s'", chk$outcome, obs_out %||% "<unknown>")
    )
  }

  if (!is.null(chk$predictors)) {
    obs     <- .get_model_rhs_vars(obj)
    missing <- setdiff(chk$predictors, obs)
    pass    <- length(missing) == 0L
    results[["predictors"]] <- .make_check(
      "predictors", pass, chk$predictors, obs,
      if (pass) sprintf("all predictors present: %s", paste(chk$predictors, collapse = ", "))
      else      sprintf("missing predictors: %s", paste(missing, collapse = ", "))
    )
  }

  if (!is.null(chk$n_predictors)) {
    obs  <- length(.get_model_rhs_vars(obj))
    pass <- obs == chk$n_predictors
    results[["n_predictors"]] <- .make_check(
      "n_predictors", pass, chk$n_predictors, obs,
      sprintf("n_predictors: expected %d, found %d", chk$n_predictors, obs)
    )
  }

  if (isTRUE(chk$predictors_only)) {
    obs_vars  <- .get_model_rhs_vars(obj)
    exp_vars  <- chk$predictors %||% character(0L)
    extra     <- setdiff(obs_vars, exp_vars)
    pass      <- length(extra) == 0L
    results[["predictors_only"]] <- .make_check(
      "predictors_only", pass, exp_vars, obs_vars,
      if (pass) "no extra predictors found"
      else      sprintf("unexpected predictors: %s", paste(extra, collapse = ", "))
    )
  }

  if (!is.null(chk$interactions)) {
    obs_labels <- .get_model_term_labels(obj)
    obs_ints   <- obs_labels[grepl(":", obs_labels, fixed = TRUE)]
    is_missing <- vapply(chk$interactions, function(int) {
      parts <- sort(strsplit(int, ":", fixed = TRUE)[[1L]])
      !any(vapply(obs_ints, function(obs)
        identical(sort(strsplit(obs, ":", fixed = TRUE)[[1L]]), parts), logical(1L)))
    }, logical(1L))
    pass <- !any(is_missing)
    results[["interactions"]] <- .make_check(
      "interactions", pass, chk$interactions, obs_ints,
      if (pass) sprintf("required interaction(s) present: %s", paste(chk$interactions, collapse = ", "))
      else      sprintf("missing interaction(s): %s", paste(chk$interactions[is_missing], collapse = ", "))
    )
  }

  if (!is.null(chk$n_interactions)) {
    obs_labels <- .get_model_term_labels(obj)
    obs_n      <- sum(grepl(":", obs_labels, fixed = TRUE))
    pass       <- obs_n == chk$n_interactions
    results[["n_interactions"]] <- .make_check(
      "n_interactions", pass, chk$n_interactions, obs_n,
      sprintf("n_interactions: expected %d, found %d", chk$n_interactions, obs_n)
    )
  }

  if (!is.null(chk$fixed_effects)) {
    obs     <- .get_fixed_effects(obj)
    missing <- setdiff(chk$fixed_effects, obs)
    pass    <- length(missing) == 0L
    results[["fixed_effects"]] <- .make_check(
      "fixed_effects", pass, chk$fixed_effects, obs,
      if (pass) sprintf("required fixed effect(s) present: %s", paste(chk$fixed_effects, collapse = ", "))
      else      sprintf("missing fixed effect(s): %s", paste(missing, collapse = ", "))
    )
  }

  if (!is.null(chk$n_fixed_effects)) {
    obs  <- length(.get_fixed_effects(obj))
    pass <- obs == chk$n_fixed_effects
    results[["n_fixed_effects"]] <- .make_check(
      "n_fixed_effects", pass, chk$n_fixed_effects, obs,
      sprintf("n_fixed_effects: expected %d, found %d", chk$n_fixed_effects, obs)
    )
  }

  if (!is.null(chk$has_fe)) {
    obs  <- length(.get_fixed_effects(obj)) > 0L
    pass <- obs == chk$has_fe
    results[["has_fe"]] <- .make_check(
      "has_fe", pass, chk$has_fe, obs,
      sprintf("has_fe: expected %s, found %s", chk$has_fe, obs)
    )
  }

  if (!is.null(chk$cluster)) {
    obs     <- .get_cluster(obj)
    missing <- setdiff(chk$cluster, obs %||% character(0L))
    pass    <- length(missing) == 0L
    results[["cluster"]] <- .make_check(
      "cluster", pass, chk$cluster,
      obs %||% "<none>",
      if (pass) sprintf("clustering correct: %s", paste(chk$cluster, collapse = ", "))
      else if (is.null(obs)) sprintf("cluster: expected '%s', no clustering found",
                                     paste(chk$cluster, collapse = ", "))
      else sprintf("missing cluster variable(s): %s", paste(missing, collapse = ", "))
    )
  }

  if (!is.null(chk$weights)) {
    obs_w <- tryCatch(deparse(obj$call$weights), error = function(e) NULL)
    pass  <- !is.null(obs_w) && obs_w == chk$weights
    results[["weights"]] <- .make_check(
      "weights", pass, chk$weights, obs_w %||% "<none>",
      sprintf("weights: expected '%s', found '%s'", chk$weights, obs_w %||% "<none>")
    )
  }

  if (!is.null(chk$has_weights)) {
    obs_w <- tryCatch(obj$call$weights, error = function(e) NULL)
    obs   <- !is.null(obs_w)
    pass  <- obs == chk$has_weights
    results[["has_weights"]] <- .make_check(
      "has_weights", pass, chk$has_weights, obs,
      sprintf("has_weights: expected %s, found %s", chk$has_weights, obs)
    )
  }

  if (!is.null(chk$nobs)) {
    obs  <- tryCatch(nobs(obj), error = function(e) NA_integer_)
    pass <- !is.na(obs) && obs == chk$nobs
    results[["nobs"]] <- .make_check(
      "nobs", pass, chk$nobs, obs,
      sprintf("nobs: expected %d, found %s", chk$nobs,
              if (is.na(obs)) "<unknown>" else as.character(obs))
    )
  }

  if (!is.null(chk$data)) {
    obs_data <- tryCatch(deparse(obj$call$data), error = function(e) NULL)
    pass     <- !is.null(obs_data) && obs_data == chk$data
    results[["data"]] <- .make_check(
      "data", pass, chk$data, obs_data %||% "<unknown>",
      sprintf("data: expected '%s', found '%s'", chk$data, obs_data %||% "<unknown>")
    )
  }

  if (!is.null(chk$random_effects)) {
    obs     <- .get_random_effects(obj)
    missing <- setdiff(chk$random_effects, obs)
    pass    <- length(missing) == 0L
    results[["random_effects"]] <- .make_check(
      "random_effects", pass, chk$random_effects, obs,
      if (pass) sprintf("required random effect grouping(s) present: %s",
                        paste(chk$random_effects, collapse = ", "))
      else      sprintf("missing random effect grouping(s): %s",
                        paste(missing, collapse = ", "))
    )
  }

  if (!is.null(chk$n_random_effects)) {
    obs  <- length(.get_random_effects(obj))
    pass <- obs == chk$n_random_effects
    results[["n_random_effects"]] <- .make_check(
      "n_random_effects", pass, chk$n_random_effects, obs,
      sprintf("n_random_effects: expected %d, found %d", chk$n_random_effects, obs)
    )
  }

  if (!is.null(chk$has_re)) {
    obs  <- length(.get_random_effects(obj)) > 0L
    pass <- obs == chk$has_re
    results[["has_re"]] <- .make_check(
      "has_re", pass, chk$has_re, obs,
      sprintf("has_re: expected %s, found %s", chk$has_re, obs)
    )
  }

  .make_result(name, "model", cls, results)
}


# -- Group comparison helpers ---------------------------------------------------

.compare_model_group <- function(obj, ref, group) {
  switch(group,
    estimator = {
      obj_cls <- class(obj)[1L]; ref_cls <- class(ref)[1L]
      pass    <- obj_cls == ref_cls
      msg     <- if (pass) sprintf("estimator matches: %s", ref_cls)
                 else      sprintf("estimator: expected '%s', found '%s'", ref_cls, obj_cls)
      if (pass && inherits(ref, c("glm", "glmerMod")) && !inherits(ref, "fixest")) {
        obj_fam <- tryCatch(family(obj)$family, error = function(e) NULL)
        ref_fam <- tryCatch(family(ref)$family, error = function(e) NULL)
        if (!is.null(obj_fam) && !is.null(ref_fam) && obj_fam != ref_fam) {
          pass <- FALSE
          msg  <- sprintf("GLM family: expected '%s', found '%s'", ref_fam, obj_fam)
        }
      }
      list(pass = pass, expected = ref_cls, observed = obj_cls, msg = msg)
    },
    outcome = {
      obj_out <- .get_model_outcome(obj); ref_out <- .get_model_outcome(ref)
      pass    <- isTRUE(obj_out == ref_out)
      list(pass = pass, expected = ref_out, observed = obj_out,
           msg = if (pass) sprintf("outcome matches: %s", ref_out)
                 else      sprintf("outcome: expected '%s', found '%s'",
                                   ref_out %||% "?", obj_out %||% "?"))
    },
    predictors = {
      obj_vars <- sort(.get_model_rhs_vars(obj))
      ref_vars <- sort(.get_model_rhs_vars(ref))
      missing  <- setdiff(ref_vars, obj_vars)
      extra    <- setdiff(obj_vars, ref_vars)
      pass     <- length(missing) == 0L && length(extra) == 0L
      list(pass = pass, expected = ref_vars, observed = obj_vars,
           msg = if (pass) "predictor variables match reference"
                 else sprintf("predictor mismatch -- missing: {%s}, extra: {%s}",
                               paste(missing, collapse = ", "),
                               paste(extra,   collapse = ", ")))
    },
    interactions = {
      obj_terms <- .get_model_term_labels(obj)
      ref_terms <- .get_model_term_labels(ref)
      obj_ints  <- sort(obj_terms[grepl(":", obj_terms, fixed = TRUE)])
      ref_ints  <- sort(ref_terms[grepl(":", ref_terms, fixed = TRUE)])
      pass      <- identical(obj_ints, ref_ints)
      list(pass = pass, expected = ref_ints, observed = obj_ints,
           msg = if (pass) "interaction terms match reference"
                 else sprintf("interaction mismatch -- expected {%s}, found {%s}",
                               paste(ref_ints, collapse = ", "),
                               paste(obj_ints, collapse = ", ")))
    },
    effects = {
      obj_fe  <- sort(.get_fixed_effects(obj))
      ref_fe  <- sort(.get_fixed_effects(ref))
      pass    <- identical(obj_fe, ref_fe)
      list(pass = pass, expected = ref_fe, observed = obj_fe,
           msg = if (pass) "fixed effects match reference"
                 else sprintf("fixed effects mismatch -- expected {%s}, found {%s}",
                               paste(ref_fe, collapse = ", "),
                               paste(obj_fe, collapse = ", ")))
    },
    inference = {
      obj_cl <- sort(.get_cluster(obj) %||% character(0L))
      ref_cl <- sort(.get_cluster(ref) %||% character(0L))
      pass   <- identical(obj_cl, ref_cl)
      list(pass = pass, expected = ref_cl, observed = obj_cl,
           msg = if (pass) "clustering matches reference"
                 else sprintf("clustering mismatch -- expected {%s}, found {%s}",
                               paste(ref_cl %||% "<none>", collapse = ", "),
                               paste(obj_cl %||% "<none>", collapse = ", ")))
    },
    weights = {
      obj_w <- tryCatch(deparse(obj$call$weights), error = function(e) NULL)
      ref_w <- tryCatch(deparse(ref$call$weights), error = function(e) NULL)
      pass  <- identical(obj_w, ref_w)
      list(pass = pass, expected = ref_w %||% "<none>", observed = obj_w %||% "<none>",
           msg = if (pass) "weights match reference"
                 else sprintf("weights mismatch -- expected '%s', found '%s'",
                               ref_w %||% "<none>", obj_w %||% "<none>"))
    },
    sample = {
      obj_n <- tryCatch(nobs(obj), error = function(e) NA_integer_)
      ref_n <- tryCatch(nobs(ref), error = function(e) NA_integer_)
      pass  <- isTRUE(obj_n == ref_n)
      list(pass = pass, expected = ref_n, observed = obj_n,
           msg = if (pass) sprintf("sample size matches: %d observations", ref_n)
                 else      sprintf("sample size: expected %d, found %d",
                                   ref_n %||% NA, obj_n %||% NA))
    },
    random_effects = {
      obj_re <- sort(.get_random_effects(obj))
      ref_re <- sort(.get_random_effects(ref))
      pass   <- identical(obj_re, ref_re)
      list(pass = pass, expected = ref_re, observed = obj_re,
           msg = if (pass) {
             if (length(ref_re) == 0L) "no random effects (as expected)"
             else sprintf("random effects match: %s", paste(ref_re, collapse = ", "))
           } else sprintf("random effects mismatch -- expected {%s}, found {%s}",
                           paste(if (length(ref_re)) ref_re else "<none>", collapse = ", "),
                           paste(if (length(obj_re)) obj_re else "<none>", collapse = ", ")))
    },
    list(pass = NA, expected = NULL, observed = NULL,
         msg = sprintf("group '%s' not implemented", group))
  )
}


# -- Extraction helpers ---------------------------------------------------------

.get_model_outcome <- function(obj) {
  tryCatch({
    f <- if (inherits(obj, "fixest")) obj$fml else formula(obj)
    deparse(f[[2L]])
  }, error = function(e) NULL)
}

.lme4_nobars <- function(f) {
  if (requireNamespace("reformulas", quietly = TRUE))
    reformulas::nobars(f)
  else
    suppressWarnings(lme4::nobars(f))
}

.lme4_findbars <- function(f) {
  if (requireNamespace("reformulas", quietly = TRUE))
    reformulas::findbars(f)
  else
    suppressWarnings(lme4::findbars(f))
}

.get_model_rhs_vars <- function(obj) {
  tryCatch({
    if (inherits(obj, "fixest"))
      return(unique(c(all.vars(obj$fml[[3L]]), .get_fixed_effects(obj))))
    if (inherits(obj, c("lmerMod", "glmerMod")))
      return(all.vars(.lme4_nobars(formula(obj))[[3L]]))
    all.vars(formula(obj)[[3L]])
  }, error = function(e) character(0L))
}

.get_model_term_labels <- function(obj) {
  tryCatch({
    f <- if (inherits(obj, "fixest")) obj$fml
         else if (inherits(obj, c("lmerMod", "glmerMod"))) .lme4_nobars(formula(obj))
         else formula(obj)
    attr(terms(f), "term.labels")
  }, error = function(e) character(0L))
}

.get_random_effects <- function(obj) {
  if (!inherits(obj, c("lmerMod", "glmerMod"))) return(character(0L))
  tryCatch({
    bars <- .lme4_findbars(formula(obj))
    if (is.null(bars) || length(bars) == 0L) return(character(0L))
    vapply(bars, function(bar) {
      vars <- all.vars(bar[[3L]])
      paste(vars, collapse = ":")
    }, character(1L))
  }, error = function(e) character(0L))
}

.get_fixed_effects <- function(obj) {
  if (!inherits(obj, "fixest")) return(character(0L))
  tryCatch({
    fe <- obj$fixef_vars
    if (is.null(fe)) character(0L) else fe
  }, error = function(e) character(0L))
}

.get_cluster <- function(obj) {
  if (!inherits(obj, "fixest")) return(NULL)
  tryCatch({
    cl <- obj$call$cluster
    if (is.null(cl)) return(NULL)
    all.vars(eval(cl))
  }, error = function(e) NULL)
}
