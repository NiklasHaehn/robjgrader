# autograde.R -- Problem Set XX: [Title]
#
# Workflow:
#   source_student_file()  →  validate() per criterion  →  run_autograder()
#
# Local use:
#   setwd("<path-to-submission-folder>")
#   source("autograde.R")
#
# Gradescope: runs automatically; ASSIGNMENT_TITLE env var triggers container paths.


# ==============================================================================
# Setup
# ==============================================================================

library(Robjgrader)

# === CUSTOMIZE: load packages the PS requires =================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(modelsummary)
  library(gt)
})


# ==============================================================================
# 1. Source student submission
# ==============================================================================

records <- source_student_file(
  autograder_name = "autograde.R",
  record_types    = c("df", "ggplot", "model", "table")
)


# ==============================================================================
# 2. Reference objects
#
#    Build the expected correct output here -- not from the student file.
#    The reference is used by validate() for group-level comparison.
#
# === CUSTOMIZE: replace the data loading and transformations below ============
# ==============================================================================

# --- Data (Task 1) ------------------------------------------------------------
# Replace read_csv(...) with the actual data the PS provides.
# This example filters mtcars to automatic transmission cars.

ps_data <- mtcars                                  # replace with read_csv(...)

ref_df <- ps_data |>
  filter(am == 0) |>
  select(mpg, wt, hp, cyl)

# --- Plot (Task 2) ------------------------------------------------------------
ref_plot <- ggplot(ref_df, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(x = "Weight (1000 lbs)", y = "Miles per Gallon", color = "Cylinders")

# --- Model (Task 3) -----------------------------------------------------------
ref_model <- lm(mpg ~ wt + hp, data = ref_df)

# --- Table (Task 4) -----------------------------------------------------------
ref_table <- modelsummary(
  list("OLS" = ref_model),
  output  = "gt",
  gof_map = c("nobs", "r.squared", "r.squared.adj")
)


# ==============================================================================
# 3. Test functions
#
#    One function per graded criterion.
#    Must return "SUCCESS" on pass, or a descriptive message string on fail.
#    result_to_outcome() converts a robjgrader_result to that format.
# ==============================================================================

# Submission ran without error (always the first test case)
test_submission <- function() ag_submission_test()

# --- Task 1: data wrangling ---------------------------------------------------
test_df <- function() {
  result <- validate(
    records,
    name      = "df",          # exact variable name the student must use
    reference = ref_df,
    checks    = c("dimensions", "names", "values"),
    exclude   = "row_order"    # row order not required unless PS says so
  )
  result_to_outcome(result)
}

# --- Task 2: plot -------------------------------------------------------------
test_plot <- function() {
  result <- validate(
    records,
    # Use name = "p" if the PS requires a specific variable name.
    # Use match = list(...) to find the plot by content when naming is free.
    match     = list(type = "ggplot", aes_x = "wt", aes_y = "mpg"),
    reference = ref_plot,
    exclude   = c("theme", "data")  # theme and data rarely graded
  )
  result_to_outcome(result)
}

# --- Task 3: model ------------------------------------------------------------
test_model <- function() {
  result <- validate(
    records,
    name   = "m1",
    checks = list(
      estimator  = "lm",
      outcome    = "mpg",
      predictors = c("wt", "hp"),
      nobs       = nrow(ref_df)
    )
  )
  result_to_outcome(result)
}

# --- Task 4: regression table -------------------------------------------------
test_table <- function() {
  result <- validate(
    records,
    match  = list(type = "table"),
    checks = list(
      terms     = c("wt", "hp"),
      gof       = c("Num.Obs.", "R2"),
      n_models  = 1L
    )
  )
  result_to_outcome(result)
}


# ==============================================================================
# 4. Test cases
#
#    name       Human-readable label shown to students on Gradescope.
#    fun        Test function defined above (unquoted function object).
#    args       Named list passed to fun via do.call(); usually list().
#    expect     Expected return value; "SUCCESS" for all validate-based tests.
#    visibility "visible" shows result to student; "hidden" reveals after deadline.
#    weight     Points for this criterion.
#
# === CUSTOMIZE: adjust names, weights, and visibility =========================
# ==============================================================================

test_cases <- list(
  list(
    name       = "Submission runs without error",
    fun        = test_submission,
    args       = list(),
    expect     = "SUCCESS",
    visibility = "visible",
    weight     = 1
  ),
  list(
    name       = "Task 1: data wrangling correct",
    fun        = test_df,
    args       = list(),
    expect     = "SUCCESS",
    visibility = "visible",
    weight     = 2
  ),
  list(
    name       = "Task 2: scatter plot with correct aesthetics",
    fun        = test_plot,
    args       = list(),
    expect     = "SUCCESS",
    visibility = "visible",
    weight     = 2
  ),
  list(
    name       = "Task 3: OLS regression correctly specified",
    fun        = test_model,
    args       = list(),
    expect     = "SUCCESS",
    visibility = "visible",
    weight     = 2
  ),
  list(
    name       = "Task 4: regression table with correct terms and GOF",
    fun        = test_table,
    args       = list(),
    expect     = "SUCCESS",
    visibility = "hidden",   # hidden: student sees score, not feedback
    weight     = 3
  )
)


# ==============================================================================
# 5. Run
# ==============================================================================

run_autograder(test_cases)
