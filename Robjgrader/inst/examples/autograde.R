# autograde.R — example autograder script
#
# Demonstrates the full Robjgrader workflow:
#   1. LLM configuration (for validate_text)
#   2. Sourcing the student file and recording objects
#   3. Exclusive object matching across multiple validate() calls
#   4. Coefficient sign, value, and significance checks
#   5. Text grading with section selection and a Markdown reference
#   6. Graceful not-found handling (no stop() on missing objects)
#   7. Exporting results to Gradescope JSON
#
# Run from the submission directory (Gradescope places all files there):
#   Rscript autograde.R

library(Robjgrader)


# 1. Configure LLM (Groq default; reads GROQ_API_KEY from environment) ---------
robjgrader_set_llm(provider = "groq")


# 2. Source student file -------------------------------------------------------
#    record_script() is called internally; the returned records carry a
#    .used_ids attribute so validate() can implement exclusive matching.
records <- source_student_file()


# 3. Grade a data frame -------------------------------------------------------
#    Looks for a df named "gapminder_2007".  If the student used a different
#    name, match = list(type = "df") falls back to the last recorded df.

ref_gapminder <- readRDS("solutions/gapminder_2007.rds")

res_df <- validate(
  records,
  name      = "gapminder_2007",
  reference = ref_gapminder,
  checks    = list(nrow = 142L, ncol = 6L)
)


# 4. Grade two regression models (exclusive matching) -------------------------
#    Two validate() calls both target lm objects.  Exclusive matching ensures
#    each call receives a different student model; if fewer than two lm models
#    were recorded, the second call returns a graceful not-found result.

ref_ols <- lm(lifeExp ~ log(gdpPercap) + pop, data = ref_gapminder)

res_m1 <- validate(
  records,
  match     = list(type = "model", outcome = "lifeExp", estimator = "lm"),
  reference = ref_ols,
  checks    = list(
    outcome      = "lifeExp",
    predictors   = "log(gdpPercap)",
    coef_sign    = c("log(gdpPercap)" = "positive"),
    coef_sig     = c("log(gdpPercap)" = TRUE),
    coef_sig_level = 0.05
  )
)

res_m2 <- validate(
  records,
  match  = list(type = "model", outcome = "lifeExp", estimator = "lm"),
  checks = list(
    outcome          = "lifeExp",
    predictors       = c("log(gdpPercap)", "continent"),
    predictors_only  = TRUE,
    coef_sign        = c("log(gdpPercap)" = "positive"),
    coef             = list(
      values    = c("(Intercept)" = 40),
      tolerance = 10
    )
  )
)


# 5. Grade a ggplot -----------------------------------------------------------
res_plot <- validate(
  records,
  match  = list(type = "ggplot", aes_x = "gdpPercap", aes_y = "lifeExp"),
  checks = list(geom = "point")
)


# 6. Grade a written answer (validate_text) -----------------------------------
#    Uses match_section to find the interpretation section regardless of
#    whether the student titled it "Q3", "Question 3", or "Interpretation".
#    A specific section from the solutions Markdown is used as a reference.

student_text <- find_student_text()

res_text <- validate_text(
  text          = student_text,
  match_section = list(heading = "q3|interpret|gdp", min_words = 30L),
  question      = "Interpret the coefficient on log(gdpPercap).",
  rubric        = c(
    direction   = "Identifies that higher GDP is associated with longer life expectancy.",
    magnitude   = "Comments on the magnitude or practical significance.",
    causality   = "Avoids causal language or acknowledges the observational nature."
  ),
  reference     = list(path = "solutions/solutions.md", section = "Q3"),
  feedback      = TRUE,
  name          = "q3_interpretation"
)


# 7. Collect and export results -----------------------------------------------
results <- list(res_df, res_m1, res_m2, res_plot, res_text)
points  <- c(5, 10, 10, 5, 10)

run_autograder(results, points = points)
