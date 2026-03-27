library(Robjgrader)

# ==============================================================================
# PS-02 Autograder — Democracy and Development
#
# Students are asked to:
#   Q1. Load and clean the V-Dem dataset (data frame)
#   Q2. Plot the relationship between GDP and democracy (ggplot)
#   Q3. Run an OLS regression of democracy on GDP and population (lm)
#   Q4. Run a fixed-effects regression with country FE (fixest)
#   Q5. Produce a regression table comparing both models (gt/modelsummary)
#   Q6. Submit a short written interpretation of the GDP coefficient (text file)
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Record student submission
# ------------------------------------------------------------------------------

records <- source_student_file()


# ------------------------------------------------------------------------------
# 2. Validate objects
# ------------------------------------------------------------------------------

# Q1 — Cleaned data frame
# Match by reference: extract structural criteria automatically
ref_df <- data.frame(
  country_name = character(),
  year         = integer(),
  v2x_polyarchy = numeric(),
  gdp_pc       = numeric(),
  population   = numeric()
)

res_q1 <- validate(
  records,
  reference = ref_df,
  checks    = list(
    nrow     = list(min = 1000),
    required = c("country_name", "year", "v2x_polyarchy", "gdp_pc", "population")
  )
)


# Q2 — Scatter plot (GDP per capita vs. V-Dem polyarchy score)
# Match by aesthetics; check geom and axis labels
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


# Q3 — OLS regression: v2x_polyarchy ~ log(gdp_pc) + log(population)
# Match by outcome and estimator; check terms and N
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


# Q4 — Fixed-effects regression (fixest): same outcome, country FE
res_q4 <- validate(
  records,
  match  = list(
    type         = "model",
    outcome      = "v2x_polyarchy",
    estimator    = "fixest",
    fixed_effects = "country_name"
  ),
  checks = list(
    terms = list(required = c("gdp_pc", "population"))
  )
)


# Q5 — Regression table with both models
# Match by type; check model count and required terms
res_q5 <- validate(
  records,
  match  = list(type = "table"),
  checks = list(
    n_models = list(min = 2),
    terms    = list(required = c("gdp_pc", "population"))
  )
)


# Q6 — Written interpretation of the GDP coefficient
# Auto-discover the student's text file; grade with LLM
answer <- find_student_text(pattern = "interpretation|q6|question")

res_q6 <- validate_text(
  text      = answer,
  question  = paste(
    "Interpret the coefficient on log(gdp_pc) from your OLS regression.",
    "What does the coefficient tell us about the relationship between",
    "GDP per capita and democracy? Is the effect statistically significant?"
  ),
  rubric = c(
    direction    = "Correctly identifies the sign of the coefficient and
                   what it implies for the GDP-democracy relationship.",
    magnitude    = "Interprets the substantive size of the effect
                   (e.g., a 1-unit increase in log GDP is associated with X).",
    significance = "States whether the coefficient is statistically significant
                   and what that means for the conclusion.",
    caveats      = "Acknowledges at least one limitation (e.g., omitted variable
                   bias, reverse causality, or selection)."
  ),
  reference = "solution_q6.txt",    # model answer on file; remove if not used
  feedback  = TRUE,
  name      = "Q6: Interpretation"
)


# ------------------------------------------------------------------------------
# 3. Run autograder and write results.json
# ------------------------------------------------------------------------------

test_cases <- list(
  list(
    name       = "Q1: Data cleaning",
    result     = res_q1,
    max_score  = 15
  ),
  list(
    name       = "Q2: Scatter plot",
    result     = res_q2,
    max_score  = 15
  ),
  list(
    name       = "Q3: OLS regression",
    result     = res_q3,
    max_score  = 20
  ),
  list(
    name       = "Q4: Fixed-effects regression",
    result     = res_q4,
    max_score  = 20
  ),
  list(
    name       = "Q5: Regression table",
    result     = res_q5,
    max_score  = 15
  ),
  list(
    name       = "Q6: Written interpretation",
    result     = res_q6,
    max_score  = 15,
    visibility = "after_published"   # hide LLM score until grades are released
  )
)

run_autograder(test_cases)
