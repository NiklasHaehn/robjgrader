library(Robjgrader)

# ==============================================================================
# PS-02 Autograder — Democracy and Development
#
# Students submit:
#   ps02.R          — R script with all analytical objects
#   ps02_text.md    — written answers for Q5 and Q6 in a single file,
#                     structured with Markdown headings:
#
#                       # Q5: OLS Interpretation
#                       [answer]
#
#                       # Q6: Fixed Effects
#                       [answer]
#
# Questions graded:
#   Q1. Load and clean the V-Dem dataset (data frame)
#   Q2. Plot the relationship between GDP and democracy (ggplot)
#   Q3. Run an OLS regression of democracy on GDP and population (lm)
#   Q4. Run a fixed-effects regression with country FE (fixest)
#   Q5. Produce a regression table comparing both models (gt / modelsummary)
#   Q6. Written interpretation of the OLS GDP coefficient
#   Q7. Written reflection on what changes in the FE model and why
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Record student R submission
# ------------------------------------------------------------------------------

records <- source_student_file()


# ------------------------------------------------------------------------------
# 2. Validate R objects
# ------------------------------------------------------------------------------

# Q1 — Cleaned data frame
# Provide a reference to drive automatic type-based matching
ref_df <- data.frame(
  country_name  = character(),
  year          = integer(),
  v2x_polyarchy = numeric(),
  gdp_pc        = numeric(),
  population    = numeric()
)

res_q1 <- validate(
  records,
  reference = ref_df,
  checks    = list(
    nrow     = list(min = 1000),
    required = c("country_name", "year", "v2x_polyarchy", "gdp_pc", "population")
  )
)


# Q2 — Scatter plot (GDP per capita vs. polyarchy score)
# Match by required aesthetics; check geom and axis label
res_q2 <- validate(
  records,
  match  = list(
    type  = "ggplot",
    aes_x = "gdp_pc",
    aes_y = "v2x_polyarchy"
  ),
  checks = list(
    geoms    = list(required = "GeomPoint"),
    mappings = list(required = list(x = "gdp_pc", y = "v2x_polyarchy")),
    labels   = list(required = list(x = "GDP per capita"))
  )
)


# Q3 — OLS regression
res_q3 <- validate(
  records,
  match  = list(
    type      = "model",
    outcome   = "v2x_polyarchy",
    estimator = "lm"
  ),
  checks = list(
    terms = list(required = c("gdp_pc", "population")),
    gof   = list(nobs = list(min = 500))
  )
)


# Q4 — Fixed-effects regression (fixest) with country FE
res_q4 <- validate(
  records,
  match  = list(
    type          = "model",
    outcome       = "v2x_polyarchy",
    estimator     = "fixest",
    fixed_effects = "country_name"
  ),
  checks = list(
    terms = list(required = c("gdp_pc", "population"))
  )
)


# Q5 — Regression table with both models
res_q5 <- validate(
  records,
  match  = list(type = "table"),
  checks = list(
    n_models = list(min = 2),
    terms    = list(required = c("gdp_pc", "population"))
  )
)


# ------------------------------------------------------------------------------
# 3. Validate written answers (combined Markdown file)
#
# Expected student file structure (ps02_text.md):
#
#   # Q6: OLS Interpretation
#   A one-unit increase in log GDP per capita is associated with...
#
#   # Q7: Fixed Effects
#   After adding country fixed effects, the coefficient on GDP...
# ------------------------------------------------------------------------------

text <- find_student_text(pattern = "text|written|ps02")

# Q6 — Match section by name (partial, case-insensitive)
res_q6 <- validate_text(
  text       = text,
  section    = "Q6",
  n_sections = 2,
  question   = paste(
    "Interpret the coefficient on log(gdp_pc) from your OLS regression.",
    "What does the coefficient tell us about the relationship between",
    "GDP per capita and democracy? Is the effect statistically significant?"
  ),
  rubric = c(
    direction    = "Correctly identifies the sign of the coefficient.",
    magnitude    = "Interprets the substantive size (e.g. a 1-unit increase
                   in log GDP is associated with X).",
    significance = "States whether the effect is statistically significant
                   and what that implies.",
    caveats      = "Acknowledges at least one limitation (omitted variable
                   bias, reverse causality, or selection)."
  ),
  reference = "solution_q6.txt",
  feedback  = TRUE,
  name      = "Q6: OLS Interpretation"
)

# Q7 — Match section by position (second heading)
res_q7 <- validate_text(
  text       = text,
  section    = 2L,
  n_sections = 2,
  question   = paste(
    "Compare your OLS and fixed-effects estimates of the GDP coefficient.",
    "What changes, and why? What does the direction of change tell us about",
    "the relationship between GDP and omitted country-level factors?"
  ),
  rubric = c(
    direction_change = "Correctly describes whether the coefficient increases
                       or decreases when moving to FE.",
    mechanism        = "Provides a substantive explanation for the change
                       (e.g. richer countries are also more democratic for
                       historical reasons not captured in OLS).",
    interpretation   = "Draws a conclusion about the bias direction in the
                       OLS estimate."
  ),
  feedback = TRUE,
  name     = "Q7: Fixed Effects Reflection"
)


# ------------------------------------------------------------------------------
# 4. Run autograder and write results.json
# ------------------------------------------------------------------------------

test_cases <- list(
  list(
    name      = "Q1: Data cleaning",
    result    = res_q1,
    max_score = 10
  ),
  list(
    name      = "Q2: Scatter plot",
    result    = res_q2,
    max_score = 15
  ),
  list(
    name      = "Q3: OLS regression",
    result    = res_q3,
    max_score = 20
  ),
  list(
    name      = "Q4: Fixed-effects regression",
    result    = res_q4,
    max_score = 20
  ),
  list(
    name      = "Q5: Regression table",
    result    = res_q5,
    max_score = 15
  ),
  list(
    name       = "Q6: OLS interpretation",
    result     = res_q6,
    max_score  = 10,
    visibility = "after_published"
  ),
  list(
    name       = "Q7: Fixed effects reflection",
    result     = res_q7,
    max_score  = 10,
    visibility = "after_published"
  )
)

run_autograder(test_cases)
