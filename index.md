# Automated Grading of R Submissions for Political Science Methods Courses

### Purpose

Automated grading of student R scripts requires two things: capturing
what a student’s script produces, and checking whether those objects
meet substantive expectations. Existing tools handle one or the other —
but not in a way designed for the heterogeneous output of a political
science methods course, where a single problem set might ask for a
cleaned data frame, a `ggplot2` visualization, an OLS or fixed-effects
model, and a formatted regression table.

**Robjgrader** provides a unified pipeline for this task. It records
every analytical object a student’s script assigns or prints, and
exposes a flexible validation interface that checks objects by name,
type, or structural similarity against a reference solution — without
requiring students to follow a fixed naming convention. Written answers
can be graded via an LLM backend (Groq or any OpenAI-compatible
endpoint), with optional per-student feedback that is constrained to be
constructive, precise, and written in plain English.

### Installation

``` r
# remotes::install_github("NiklasHaehn/robjgrader")
library(Robjgrader)
```

### Workflow

A typical autograder script has three stages:

``` r
library(Robjgrader)

# 1. Record all objects produced by the student's script
records <- source_student_file()

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

[`run_autograder()`](https://niklashaehn.github.io/robjgrader/reference/run_autograder.md)
writes results to `/autograder/results/results.json` (Gradescope format)
and prints a summary to the console.

### Grading Written Answers

Problem sets that include written interpretation questions can be graded
via
[`validate_text()`](https://niklashaehn.github.io/robjgrader/reference/validate_text.md),
which sends the student’s answer to an OpenAI-compatible LLM endpoint
(default: Groq, `temperature = 0` for deterministic results). Pass a
question and rubric and the full grading prompt is built automatically.

Students can submit all written answers in a single file. Use
[`split_student_text()`](https://niklashaehn.github.io/robjgrader/reference/split_student_text.md)
to extract individual sections before grading. Sections are detected
automatically: Markdown files are split on `#` headings; plain-text and
PDF files are split on labelled prefixes (`Q1:`, `Question 1.`, `1.`,
`1)`) with a double-blank-line fallback.

``` r
# Student submits a single Markdown file:
#
#   # Q3: Interpretation
#   A 1-unit increase in log GDP per capita is associated with...
#
#   # Q4: Fixed Effects
#   After adding country fixed effects, the coefficient...

text <- find_student_text()            # auto-discovers the text file

res_q3 <- validate_text(
  text       = text,
  section    = "Q3",                   # matched by heading name
  n_sections = 2,
  question   = "Interpret the coefficient on gdp_pc in your regression.",
  rubric     = c(
    direction    = "Correctly identifies the sign of the coefficient.",
    magnitude    = "Interprets the substantive size of the effect.",
    significance = "Mentions statistical significance and its implications."
  ),
  reference  = "solution_q3.txt",      # optional model answer for comparison
  feedback   = TRUE                    # include per-student written feedback
)

res_q4 <- validate_text(
  text       = text,
  section    = 2L,                     # matched by position
  n_sections = 2,
  question   = "What changes in the fixed-effects model and why?",
  rubric     = c(direction_change = "...", mechanism = "...")
)
```

When `feedback = TRUE`, the LLM returns a short comment (max 3 sentences
/ 200 words) included in the Gradescope output for incorrect answers.
Feedback is constrained to be constructive, precise, and in plain
English — no greetings, sign-offs, or filler phrases.

Text results integrate directly into
[`run_autograder()`](https://niklashaehn.github.io/robjgrader/reference/run_autograder.md)
alongside object-based results. Partial credit is supported: the LLM
returns a normalized score (0–1) which is multiplied by `max_score`:

``` r
test_cases <- list(
  list(name = "OLS model",      result = res_model, max_score = 30),
  list(name = "Interpretation", result = res_q3,    max_score = 20,
       visibility = "after_published")
)

run_autograder(test_cases)
```

### Key Functions

**Recording**

| Function                                                                                             | Description                                                                                              |
|:-----------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------|
| [`record_script()`](https://niklashaehn.github.io/robjgrader/reference/record_script.md)             | Parse and evaluate a student script expression by expression, capturing all assigned and printed objects |
| [`source_student_file()`](https://niklashaehn.github.io/robjgrader/reference/source_student_file.md) | Locate and record a student R submission automatically, excluding the calling script                     |
| [`get_records()`](https://niklashaehn.github.io/robjgrader/reference/get_records.md)                 | Retrieve and filter recorded objects by type                                                             |
| [`grab()`](https://niklashaehn.github.io/robjgrader/reference/grab.md)                               | Wrap a single object from the global environment into a one-element records list                         |

**Validation**

| Function                                                                                           | Description                                                                                                             |
|:---------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------------------------------------------------------|
| [`validate()`](https://niklashaehn.github.io/robjgrader/reference/validate.md)                     | Validate a recorded object by name, match criteria, or reference object                                                 |
| [`validate_text()`](https://niklashaehn.github.io/robjgrader/reference/validate_text.md)           | Grade a written answer via an LLM, with optional rubric, reference answer, section extraction, and per-student feedback |
| [`split_student_text()`](https://niklashaehn.github.io/robjgrader/reference/split_student_text.md) | Split a multi-question text file into named sections (Markdown headings or numbered prefixes)                           |
| [`find_student_text()`](https://niklashaehn.github.io/robjgrader/reference/find_student_text.md)   | Auto-discover a student text submission file in the working directory                                                   |
| [`read_student_text()`](https://niklashaehn.github.io/robjgrader/reference/read_student_text.md)   | Read a `.txt`, `.md`, or `.pdf` file into a character string                                                            |

**Running**

| Function                                                                                         | Description                                                                      |
|:-------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------|
| [`run_autograder()`](https://niklashaehn.github.io/robjgrader/reference/run_autograder.md)       | Execute a list of test cases and produce Gradescope-compatible output            |
| [`result_to_outcome()`](https://niklashaehn.github.io/robjgrader/reference/result_to_outcome.md) | Convert a validation result to a `"SUCCESS"` string or formatted failure message |

### Supported Object Types

- **Data frames** — dimensions, column names, column types
- **ggplot2 plots** — geoms, aesthetic mappings, facets, labels, scales
- **Models** — `lm`, `glm`, `fixest` (with fixed effects and
  clustering), `lmer`/`glmer`
- **Regression tables** — `gt`, `flextable`, `tinytable`, `huxtable`;
  checks for model count, terms, GOF rows, and column labels

### Gradescope Integration

The package is designed for deployment on
[Gradescope](https://www.gradescope.com/) via a Docker-based autograder.
[`run_autograder()`](https://niklashaehn.github.io/robjgrader/reference/run_autograder.md)
writes its output directly to the path Gradescope expects. Partial
credit, per-test score weights, and descriptive failure messages are all
supported.
