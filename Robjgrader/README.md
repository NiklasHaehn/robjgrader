Automated Grading of R Submissions for Political Science Methods Courses
================

<!-- badges: start -->
[![pkgdown](https://github.com/NiklasHaehn/robjgrader/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/NiklasHaehn/robjgrader/actions/workflows/pkgdown.yml)
<!-- badges: end -->

### Purpose

Automated grading of student R scripts requires two things: capturing
what a student's script produces, and checking whether those objects
meet substantive expectations. Existing tools handle one or the other —
but not in a way designed for the heterogeneous output of a political
science methods course, where a single problem set might ask for a
cleaned data frame, a `ggplot2` visualization, an OLS or fixed-effects
model, and a formatted regression table.

**Robjgrader** provides a unified pipeline for this task. It records
every analytical object a student's script assigns or prints, and
exposes a flexible validation interface that checks objects by name,
type, or structural similarity against a reference solution — without
requiring students to follow a fixed naming convention.

### Installation

```r
# remotes::install_github("NiklasHaehn/robjgrader")
library(Robjgrader)
```

### Workflow

A typical autograder script has three stages:

```r
library(Robjgrader)

# 1. Record all objects produced by the student's script
records <- source_student_file("autograde.R")

# 2. Validate individual objects
res_df <- validate(records, "clean_data",
  checks = list(nrow = 1000, required = c("year", "country", "gdp_pc"))
)

res_model <- validate(records, "ols_model",
  checks = list(estimator = "lm", required = c("gdp_pc", "polity2"))
)

# 3. Run all test cases and write Gradescope-compatible JSON
test_cases <- list(
  list(name = "clean_data", result = res_df,    max_score = 20),
  list(name = "ols_model",  result = res_model, max_score = 30)
)

run_autograder(test_cases)
```

`run_autograder()` writes results to `/autograder/results/results.json`
(Gradescope format) and prints a summary to the console.

### Key Functions

| Function | Description |
|:---|:---|
| `record_script()` | Parse and evaluate a student script expression by expression, capturing all assigned and printed objects |
| `source_student_file()` | Locate and record a student submission file automatically, excluding the calling script |
| `get_records()` | Retrieve and filter recorded objects by type |
| `grab()` | Wrap a single object from the global environment into a one-element records list |
| `validate()` | Validate a recorded object by name, match criteria, or reference object |
| `run_autograder()` | Execute a list of test cases and produce Gradescope-compatible output |
| `result_to_outcome()` | Convert a validation result to a `"SUCCESS"` string or formatted failure message |

### Supported Object Types

- **Data frames** — dimensions, column names, column types
- **ggplot2 plots** — geoms, aesthetic mappings, facets, labels, scales
- **Models** — `lm`, `glm`, `fixest` (with fixed effects and clustering), `lmer`/`glmer`
- **Regression tables** — `gt`, `flextable`, `tinytable`, `huxtable`; checks for model count, terms, GOF rows, and column labels

### Gradescope Integration

The package is designed for deployment on
[Gradescope](https://www.gradescope.com/) via a Docker-based autograder.
`run_autograder()` writes its output directly to the path Gradescope
expects. Partial credit, per-test score weights, and descriptive failure
messages are all supported.
