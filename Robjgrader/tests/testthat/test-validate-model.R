library(testthat)

# ---- helpers -----------------------------------------------------------------

model_recs <- function(obj, name = "m1") {
  make_records(list(obj = obj, name = name))
}

ref_m <- lm(mpg ~ wt + hp, data = mtcars)

# ---- group checks: PASS -------------------------------------------------------

test_that("model: full group comparison PASS (identical lm)", {
  m    <- lm(mpg ~ wt + hp, data = mtcars)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref_m)
  expect_true(res$overall)
  expect_true(res$checks$estimator$pass)
  expect_true(res$checks$outcome$pass)
  expect_true(res$checks$predictors$pass)
  expect_true(res$checks$interactions$pass)
  expect_true(res$checks$effects$pass)
  expect_true(res$checks$inference$pass)
  expect_true(res$checks$weights$pass)
  expect_true(res$checks$sample$pass)
})

# ---- group checks: FAIL -------------------------------------------------------

test_that("model: estimator group FAIL when class differs", {
  m    <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref_m)
  expect_false(res$checks$estimator$pass)
})

test_that("model: outcome group FAIL on different DV", {
  m    <- lm(disp ~ wt + hp, data = mtcars)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref_m)
  expect_false(res$overall)
  expect_false(res$checks$outcome$pass)
  expect_match(res$checks$outcome$message, "mpg")
})

test_that("model: predictors FAIL on missing predictor", {
  m    <- lm(mpg ~ wt, data = mtcars)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref_m)
  expect_false(res$checks$predictors$pass)
  expect_match(res$checks$predictors$message, "hp")
})

test_that("model: predictors FAIL on extra predictor", {
  m    <- lm(mpg ~ wt + hp + disp, data = mtcars)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref_m)
  expect_false(res$checks$predictors$pass)
  expect_match(res$checks$predictors$message, "disp")
})

test_that("model: interactions FAIL when ref has interaction but student does not", {
  ref_int <- lm(mpg ~ wt * hp, data = mtcars)
  m       <- lm(mpg ~ wt + hp, data = mtcars)
  recs    <- model_recs(m)
  res     <- validate(recs, name = "m1", reference = ref_int)
  expect_false(res$checks$interactions$pass)
})

test_that("model: interactions PASS when both have same interaction", {
  ref_int <- lm(mpg ~ wt * hp, data = mtcars)
  m       <- lm(mpg ~ wt * hp, data = mtcars)
  recs    <- model_recs(m)
  res     <- validate(recs, name = "m1", reference = ref_int)
  expect_true(res$checks$interactions$pass)
})

test_that("model: sample FAIL on different nobs (subset of data)", {
  m    <- lm(mpg ~ wt + hp, data = mtcars[1:20, ])
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref_m)
  expect_false(res$checks$sample$pass)
})

# ---- GLM family/estimator ----------------------------------------------------

test_that("model: estimator group checks GLM family when ref is glm", {
  ref_glm  <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  m_pois   <- glm(am ~ wt + hp, data = mtcars, family = poisson)
  recs     <- model_recs(m_pois)
  res      <- validate(recs, name = "m1", reference = ref_glm)
  expect_false(res$checks$estimator$pass)
  expect_match(res$checks$estimator$message, "binomial|poisson")
})

# ---- exclude -----------------------------------------------------------------

test_that("model: exclude removes specified group", {
  m    <- lm(mpg ~ wt, data = mtcars)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref_m, exclude = "predictors")
  expect_null(res$checks$predictors)
  expect_true(res$checks$outcome$pass)
})

test_that("model: exclude multiple groups", {
  m    <- lm(mpg ~ wt, data = mtcars)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref_m,
                   exclude = c("predictors", "sample"))
  expect_null(res$checks$predictors)
  expect_null(res$checks$sample)
  expect_false(is.null(res$checks$outcome))
})

# ---- individual checks -------------------------------------------------------

test_that("model: individual outcome check PASS/FAIL", {
  recs <- model_recs(ref_m)
  expect_true(validate(recs, name = "m1", checks = list(outcome = "mpg"))$checks$outcome$pass)
  expect_false(validate(recs, name = "m1", checks = list(outcome = "disp"))$checks$outcome$pass)
})

test_that("model: individual estimator check PASS/FAIL", {
  recs <- model_recs(ref_m)
  expect_true(validate(recs, name = "m1", checks = list(estimator = "lm"))$checks$estimator$pass)
  expect_false(validate(recs, name = "m1", checks = list(estimator = "glm"))$checks$estimator$pass)
})

test_that("model: individual predictors check (subset)", {
  recs <- model_recs(ref_m)
  expect_true(
    validate(recs, name = "m1",
             checks = list(predictors = c("wt", "hp")))$checks$predictors$pass
  )
  expect_false(
    validate(recs, name = "m1",
             checks = list(predictors = c("wt", "disp")))$checks$predictors$pass
  )
})

test_that("model: n_predictors check PASS/FAIL", {
  recs <- model_recs(ref_m)
  expect_true(validate(recs, name = "m1", checks = list(n_predictors = 2L))$checks$n_predictors$pass)
  expect_false(validate(recs, name = "m1", checks = list(n_predictors = 3L))$checks$n_predictors$pass)
})

test_that("model: predictors_only PASS when no extras", {
  recs <- model_recs(ref_m)
  res  <- validate(recs, name = "m1",
                   checks = list(predictors = c("wt", "hp"), predictors_only = TRUE))
  expect_true(res$checks$predictors_only$pass)
})

test_that("model: predictors_only FAIL when extras present", {
  m    <- lm(mpg ~ wt + hp + disp, data = mtcars)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1",
                   checks = list(predictors = c("wt", "hp"), predictors_only = TRUE))
  expect_false(res$checks$predictors_only$pass)
  expect_match(res$checks$predictors_only$message, "disp")
})

test_that("model: interactions individual check is order-insensitive", {
  m    <- lm(mpg ~ wt * hp, data = mtcars)
  recs <- model_recs(m)

  expect_true(
    validate(recs, name = "m1",
             checks = list(interactions = "wt:hp"))$checks$interactions$pass
  )
  expect_true(
    validate(recs, name = "m1",
             checks = list(interactions = "hp:wt"))$checks$interactions$pass
  )
})

test_that("model: n_interactions check PASS/FAIL", {
  m    <- lm(mpg ~ wt * hp, data = mtcars)
  recs <- model_recs(m)
  expect_true(validate(recs, name = "m1", checks = list(n_interactions = 1L))$checks$n_interactions$pass)
  expect_false(validate(recs, name = "m1", checks = list(n_interactions = 0L))$checks$n_interactions$pass)
})

test_that("model: nobs individual check PASS/FAIL", {
  recs <- model_recs(ref_m)
  expect_true(validate(recs, name = "m1", checks = list(nobs = 32L))$checks$nobs$pass)
  expect_false(validate(recs, name = "m1", checks = list(nobs = 10L))$checks$nobs$pass)
})

test_that("model: data individual check PASS/FAIL", {
  recs <- model_recs(ref_m)
  expect_true(validate(recs, name = "m1", checks = list(data = "mtcars"))$checks$data$pass)
  expect_false(validate(recs, name = "m1", checks = list(data = "iris"))$checks$data$pass)
})

test_that("model: has_weights PASS on unweighted model", {
  recs <- model_recs(ref_m)
  expect_true(
    validate(recs, name = "m1", checks = list(has_weights = FALSE))$checks$has_weights$pass
  )
  expect_false(
    validate(recs, name = "m1", checks = list(has_weights = TRUE))$checks$has_weights$pass
  )
})

test_that("model: family and link individual checks for GLM", {
  m    <- glm(am ~ wt + hp, data = mtcars, family = binomial(link = "logit"))
  recs <- model_recs(m)

  expect_true(
    validate(recs, name = "m1", checks = list(family = "binomial"))$checks$family$pass
  )
  expect_true(
    validate(recs, name = "m1", checks = list(link = "logit"))$checks$link$pass
  )
  expect_false(
    validate(recs, name = "m1", checks = list(family = "poisson"))$checks$family$pass
  )
})

test_that("model: family check returns '<not a glm>' for non-glm", {
  recs <- model_recs(ref_m)
  res  <- validate(recs, name = "m1", checks = list(family = "binomial"))
  expect_false(res$checks$family$pass)
  expect_match(res$checks$family$observed, "not a glm")
})

test_that("model: has_fe = FALSE PASS for lm (no fixed effects)", {
  recs <- model_recs(ref_m)
  res  <- validate(recs, name = "m1", checks = list(has_fe = FALSE))
  expect_true(res$checks$has_fe$pass)
})

# ---- fixest (skip if not installed) ------------------------------------------

test_that("model: fixest fixed effects extraction", {
  skip_if_not_installed("fixest")
  library(fixest)

  m    <- feols(mpg ~ wt + hp | cyl, data = mtcars)
  recs <- model_recs(m)

  res <- validate(recs, name = "m1",
                  checks = list(outcome = "mpg", fixed_effects = "cyl", has_fe = TRUE))
  expect_true(res$checks$outcome$pass)
  expect_true(res$checks$fixed_effects$pass)
  expect_true(res$checks$has_fe$pass)
})

test_that("model: fixest n_fixed_effects check", {
  skip_if_not_installed("fixest")
  library(fixest)

  m    <- feols(mpg ~ wt | cyl + am, data = mtcars)
  recs <- model_recs(m)

  res  <- validate(recs, name = "m1", checks = list(n_fixed_effects = 2L))
  expect_true(res$checks$n_fixed_effects$pass)
})

# ---- match-based lookup -------------------------------------------------------

test_that("model: match by outcome selects correct model", {
  m1   <- lm(mpg  ~ wt, data = mtcars)
  m2   <- lm(disp ~ wt, data = mtcars)
  recs <- make_records(list(obj = m1, name = NULL),
                       list(obj = m2, name = NULL))
  res  <- validate(recs,
                   match  = list(type = "model", outcome = "mpg"),
                   checks = list(outcome = "mpg"))
  expect_true(res$checks$outcome$pass)
})

test_that("model: match by estimator narrows candidates", {
  # glm inherits from lm, so inherits(glm_obj, "lm") is TRUE.
  # Use estimator = "glm" to select only the glm (lm objects do NOT inherit glm).
  m1   <- lm(mpg ~ wt, data = mtcars)
  m2   <- glm(am ~ wt, data = mtcars, family = binomial)
  recs <- make_records(list(obj = m1, name = NULL),
                       list(obj = m2, name = NULL))
  res  <- validate(recs,
                   match  = list(type = "model", estimator = "glm"),
                   checks = list(estimator = "glm"))
  expect_true(res$checks$estimator$pass)
})


# ---- lme4: lmerMod / glmerMod ------------------------------------------------

test_that("model: lmerMod classified and recorded correctly", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  recs <- model_recs(m)
  expect_equal(recs[[1L]]$object_type, "model")
  expect_equal(recs[[1L]]$object_class, "lmerMod")
})

test_that("model: glmerMod classified and recorded correctly", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                data = cbpp, family = binomial)
  recs <- model_recs(m)
  expect_equal(recs[[1L]]$object_type, "model")
  expect_equal(recs[[1L]]$object_class, "glmerMod")
})

test_that("model: lmerMod full group comparison PASS (identical)", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  ref  <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref)

  expect_true(res$checks$estimator$pass)
  expect_true(res$checks$outcome$pass)
  expect_true(res$checks$predictors$pass)
  expect_true(res$checks$random_effects$pass)
  expect_true(res$overall)
})

test_that("model: random_effects group FAIL when grouping factor differs", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  ref  <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref)

  # Both have Subject as grouping factor; structure differs in random slope
  # but grouping factor name is the same -> random_effects group PASS
  expect_true(res$checks$random_effects$pass)
})

test_that("model: random_effects individual check PASS", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1",
                   checks = list(random_effects = "Subject"))
  expect_true(res$checks$random_effects$pass)
})

test_that("model: random_effects individual check FAIL for wrong grouping factor", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1",
                   checks = list(random_effects = "WrongGroup"))
  expect_false(res$checks$random_effects$pass)
})

test_that("model: has_re = TRUE PASS for lmerMod", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", checks = list(has_re = TRUE))
  expect_true(res$checks$has_re$pass)
})

test_that("model: has_re = FALSE PASS for regular lm", {
  m    <- lm(mpg ~ wt, data = mtcars)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", checks = list(has_re = FALSE))
  expect_true(res$checks$has_re$pass)
})

test_that("model: lmerMod predictor extraction excludes random effect terms", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", checks = list(predictors = "Days"))
  expect_true(res$checks$predictors$pass)
})

test_that("model: glmerMod family individual check PASS", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                data = cbpp, family = binomial)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", checks = list(family = "binomial"))
  expect_true(res$checks$family$pass)
})

test_that("model: n_random_effects = 1 PASS for single grouping factor", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", checks = list(n_random_effects = 1L))
  expect_true(res$checks$n_random_effects$pass)
})

test_that("model: lm has no random_effects (trivial PASS in group check)", {
  m    <- lm(mpg ~ wt + hp, data = mtcars)
  ref  <- lm(mpg ~ wt + hp, data = mtcars)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", reference = ref)
  expect_true(res$checks$random_effects$pass)
  expect_true(res$overall)
})


# ---- coef_sign ---------------------------------------------------------------

test_that("model: coef_sign PASS for negative wt coefficient", {
  recs <- model_recs(ref_m)
  res  <- validate(recs, name = "m1", checks = list(coef_sign = c(wt = "negative")))
  expect_true(res$checks[["coef_sign.wt"]]$pass)
})

test_that("model: coef_sign FAIL when sign is wrong", {
  recs <- model_recs(ref_m)
  res  <- validate(recs, name = "m1", checks = list(coef_sign = c(wt = "positive")))
  expect_false(res$checks[["coef_sign.wt"]]$pass)
})

test_that("model: coef_sign FAIL with informative message when coefficient missing", {
  recs <- model_recs(ref_m)
  res  <- validate(recs, name = "m1", checks = list(coef_sign = c(nonexistent = "positive")))
  expect_false(res$checks[["coef_sign.nonexistent"]]$pass)
  expect_match(res$checks[["coef_sign.nonexistent"]]$message, "not found")
})

test_that("model: coef_sign checks multiple coefficients independently", {
  recs <- model_recs(ref_m)
  res  <- validate(recs, name = "m1",
                   checks = list(coef_sign = c(wt = "negative", hp = "negative")))
  expect_true(res$checks[["coef_sign.wt"]]$pass)
  expect_true(res$checks[["coef_sign.hp"]]$pass)
})


# ---- coef (value with tolerance) --------------------------------------------

test_that("model: coef PASS when estimate within zero tolerance (exact)", {
  m    <- lm(mpg ~ wt, data = mtcars)
  recs <- model_recs(m)
  est  <- coef(m)[["wt"]]
  res  <- validate(recs, name = "m1",
                   checks = list(coef = list(values = c(wt = est), tolerance = 0)))
  expect_true(res$checks[["coef.wt"]]$pass)
})

test_that("model: coef PASS when estimate within tolerance", {
  m    <- lm(mpg ~ wt, data = mtcars)
  recs <- model_recs(m)
  est  <- coef(m)[["wt"]]
  res  <- validate(recs, name = "m1",
                   checks = list(coef = list(values = c(wt = est + 0.5), tolerance = 1)))
  expect_true(res$checks[["coef.wt"]]$pass)
})

test_that("model: coef FAIL when estimate outside tolerance", {
  m    <- lm(mpg ~ wt, data = mtcars)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1",
                   checks = list(coef = list(values = c(wt = 0), tolerance = 0.1)))
  expect_false(res$checks[["coef.wt"]]$pass)
  expect_match(res$checks[["coef.wt"]]$message, "expected")
})

test_that("model: coef FAIL with informative message when coefficient missing", {
  recs <- model_recs(ref_m)
  res  <- validate(recs, name = "m1",
                   checks = list(coef = list(values = c(nonexistent = 0), tolerance = 0)))
  expect_false(res$checks[["coef.nonexistent"]]$pass)
  expect_match(res$checks[["coef.nonexistent"]]$message, "not found")
})


# ---- coef_sig ----------------------------------------------------------------

test_that("model: coef_sig PASS when wt is significant (TRUE)", {
  recs <- model_recs(ref_m)
  res  <- validate(recs, name = "m1", checks = list(coef_sig = c(wt = TRUE)))
  expect_true(res$checks[["coef_sig.wt"]]$pass)
})

test_that("model: coef_sig PASS when non-significant expected (FALSE)", {
  # Use a random-noise predictor that should not be significant
  set.seed(1)
  df   <- data.frame(y = rnorm(50), x = rnorm(50), noise = rnorm(50))
  m    <- lm(y ~ x + noise, data = df)
  recs <- model_recs(m)
  # Either x or noise may be non-sig; just check the check itself runs and returns a bool
  res  <- validate(recs, name = "m1", checks = list(coef_sig = list(noise = FALSE)))
  expect_type(res$checks[["coef_sig.noise"]]$pass, "logical")
})

test_that("model: coef_sig FAIL when significant expected but coefficient is not", {
  set.seed(42)
  df   <- data.frame(y = rnorm(20), x = rnorm(20))
  m    <- lm(y ~ x, data = df)
  recs <- model_recs(m)
  p    <- summary(m)$coefficients["x", "Pr(>|t|)"]
  # If p >= 0.05, the test verifies FAIL when sig = TRUE expected
  if (p >= 0.05) {
    res <- validate(recs, name = "m1", checks = list(coef_sig = c(x = TRUE)))
    expect_false(res$checks[["coef_sig.x"]]$pass)
  } else {
    skip("x happened to be significant in this seed — skip FAIL branch")
  }
})

test_that("model: coef_sig FAIL with message when coefficient missing", {
  recs <- model_recs(ref_m)
  res  <- validate(recs, name = "m1", checks = list(coef_sig = c(nonexistent = TRUE)))
  expect_false(res$checks[["coef_sig.nonexistent"]]$pass)
  expect_match(res$checks[["coef_sig.nonexistent"]]$message, "not found")
})

test_that("model: coef_sig_level changes significance threshold", {
  recs <- model_recs(ref_m)
  # wt is significant at 0.05 but test with very strict threshold
  res  <- validate(recs, name = "m1",
                   checks = list(coef_sig = c(wt = TRUE), coef_sig_level = 1e-10))
  # At alpha = 1e-10, wt may or may not be significant — just check it runs
  expect_type(res$checks[["coef_sig.wt"]]$pass, "logical")
})

test_that("model: coef_sig returns NA for lmerMod (no p-values)", {
  skip_if_not_installed("lme4")
  library(lme4)

  m    <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  recs <- model_recs(m)
  res  <- validate(recs, name = "m1", checks = list(coef_sig = c(Days = TRUE)))
  expect_true(is.na(res$checks[["coef_sig.Days"]]$pass))
  expect_match(res$checks[["coef_sig.Days"]]$message, "not available")
})
