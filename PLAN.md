# Robjgrader — Project Plan

## Package Identity

**Robjgrader** is a runtime object recorder and semantic validator for teaching and reproducibility in R.

It captures analytical objects produced during an interactive R session (or a sourced script), and validates them against instructor-defined expectations — without requiring any changes to the student's original code.

---

## Core Design Principle

Grade the **objects students produce**, not the code they write.

This tolerates alternative but correct implementations and evaluates semantic correctness rather than syntactic equivalence.

---

## Architecture

The package has two main layers.

### Layer 1: Recorder

Attaches a task callback to the R session that intercepts top-level object events.

**Public API:**

| Function | Description |
|---|---|
| `record_start(...)` | Start recording; configure which types to capture |
| `record_stop()` | Stop recording; print capture summary |
| `get_records(type, name)` | Retrieve captured objects, optionally filtered |

**Captured event types:**

| Type | Description |
|---|---|
| `assignment` | Top-level assignment (`x <- ...`) |
| `visible_return` | Top-level expression with visible return value |
| `print_call` | Explicit `print(obj)` call for a supported class |
| `recorded_plot` | Fallback base-graphics capture via `recordPlot()` — Level 2 |

**Capture mechanism:**

`addTaskCallback()` runs after each top-level R expression and receives `expr`, `value`, `ok`, and `visible`. Object type is classified from `value`; assignment name is parsed from `expr`.

**`record_start()` parameters:**

| Parameter | Default | Description |
|---|---|---|
| `record_df` | `TRUE` | Capture `data.frame`, `tbl_df`, `tbl` |
| `record_ggplot` | `TRUE` | Capture `ggplot` objects |
| `record_model` | `TRUE` | Capture `lm`, `glm`, `fixest` |
| `record_table` | `TRUE` | Capture `gt_tbl`, `tinytable`, `flextable`, `huxtable` |

**Storage:** In-memory (list inside `.recorder_env`). Each record is a named list:

```
event_id, event_type, object_name, object_type, object_class, timestamp, object
```

**`get_records()` returns:** A `robjgrader_records` S3 object (list of records) with a print method showing a formatted event table.

---

### Layer 2: Validators

A single `validate()` function dispatches to class-specific internal validators.

**Public API:**

```r
validate(records, name = NULL, match = list(), reference = NULL,
         checks = list(), exclude = NULL, position = "last")
```

| Argument | Description |
|---|---|
| `records` | A `robjgrader_records` object from `get_records()` |
| `name` | Character. Variable name of the recorded object. Primary identification method. |
| `match` | Named list of coarse matching criteria used to identify the object when `name` is absent or ambiguous. See matching section below. |
| `reference` | Optional reference object for semantic comparison |
| `checks` | Group names (character) and/or individual property checks (named list); see below |
| `exclude` | Character vector of group names to skip |
| `position` | Positional fallback when matching yields > 1 candidate: `"last"` (default), `"first"`, or integer index among filtered candidates |

`name` and `match` are mutually exclusive. If both are omitted, an error is raised.

**Default behaviour:** when `reference` is provided and `checks` and `exclude` are both empty, all check groups are run automatically. No `checks` specification needed for a full comparison.

---

### Object Matching

The problem: student scripts may produce objects in different orders, use different variable names, or produce anonymous visible-return objects (e.g. a ggplot printed without assignment). Name-based lookup is therefore not always sufficient.

**Two identification paths:**

1. **`name`** — direct lookup by variable name. Fast and unambiguous. Preferred when the instructor can rely on students naming their objects consistently.

2. **`match`** — progressive semantic matching. The system narrows down recorded objects of the right type using increasingly specific criteria until exactly one candidate remains. Designed for anonymous objects or scripts where naming cannot be assumed.

**Matching algorithm:**

```
1. Filter all records by object type (e.g. "ggplot")
   → n candidates

2. Apply match criteria one by one, from most to least
   discriminating (see default ordering below).
   After each criterion:
     - If candidates == 1  → stop, match found
     - If candidates == 0  → backtrack (undo this criterion), continue
     - If candidates >  1  → apply next criterion

3. After all criteria applied and candidates > 1:
   → positional fallback: select by `position` among remaining candidates
   → emit a warning listing the ambiguous candidates

4. If candidates == 0 after type filter:
   → error: no recorded object of this type exists
```

**Default criterion ordering by discriminating power:**

*ggplot:* `aes_x` → `aes_y` → `geom` → `aes_color` / `aes_fill` → `facet_var` → `expr_contains`

*model:* `outcome` → `estimator` → `fixed_effects` → `predictors` → `cluster` → `nobs` → `expr_contains`

*df:* `names` → `nrow` → `ncol` → `col_types` → `expr_contains`

**`expr_contains`** matches on a substring of the source expression that produced the object (recorded in the event log). Useful for disambiguating by data source without knowing the full object properties:

```r
# Two ggplots recorded; one built from mtcars, one from gapminder
match = list(type = "ggplot", aes_x = "year", expr_contains = "gapminder")
```

**Examples:**

```r
# Name-based (primary)
validate(records, name = "p1", reference = ref)

# Match-based: unique ggplot with these aesthetics
validate(records,
  match = list(type = "ggplot", aes_x = "wt", aes_y = "mpg"),
  reference = ref)

# Match-based: OLS model with mpg as outcome
validate(records,
  match = list(type = "model", outcome = "mpg", estimator = "lm"),
  checks = list(predictors = c("wt", "hp")))

# Positional fallback: second ggplot after filtering by geom
validate(records,
  match = list(type = "ggplot", geom = "point"),
  position = 2L,
  reference = ref)

# Disambiguate two plots using expression text
validate(records,
  match = list(type = "ggplot", expr_contains = "gapminder"),
  reference = ref)
```

**Out of scope (Level 1):** full code-graph / dependency analysis (tracking which objects were derived from which). R's pervasive non-standard evaluation makes static dependency tracing fragile. Noted as a potential Level 3 extension.

**`checks` accepts two kinds of entries:**

1. **Group names** (character strings) — runs the entire group against `reference`.
   Requires `reference`.
   Example: `checks = c("aesthetics", "geoms")`

2. **Individual properties** (named list entries) — checks a specific expected value.
   Does not require `reference`.
   Example: `checks = list(aes_x = "wt", geom = "point", nobs = 100L)`

Both can be combined: `checks = list("aesthetics", geom = "point")`.

**`exclude`** removes group(s) from an otherwise full or group-level comparison. Individual property checks in `checks` are never affected by `exclude`.
Example: `exclude = c("theme")` — compare everything except the theme group.

**Common patterns:**

```r
# Full comparison (all groups)
validate(records, "p1", reference = ref)

# Full comparison, skip theme
validate(records, "p1", reference = ref, exclude = "theme")

# Only aesthetics and geoms
validate(records, "p1", reference = ref, checks = c("aesthetics", "geoms"))

# Specific property checks only (no reference required)
validate(records, "p1", checks = list(aes_x = "wt", geom = "point"))

# Reference comparison + extra individual check
validate(records, "p1", reference = ref, exclude = "theme",
         checks = list(aes_x = "wt"))
```

**Returns:** A `robjgrader_result` S3 object with overall pass/fail, per-check results (expected, observed, message), and a formatted print method.

---

## Validators: Check Groups and Individual Checks

### `validate()` on `df` objects

**Check groups:**

Row and column ordering are **opt-in** — they are not part of the default full comparison because most teaching scenarios evaluate whether students produced the correct data, not whether they sorted it. Include `"row_order"` or `"col_order"` explicitly via `checks` when ordering matters.

| Group | Default | Covers |
|---|---|---|
| `"dimensions"` | ✓ | row count and column count |
| `"names"` | ✓ | column names present (set comparison, not sequence) |
| `"col_order"` | — | exact sequence of columns |
| `"types"` | ✓ | column classes |
| `"values"` | ✓ | data values (always row-order-insensitive unless `"row_order"` also present) |
| `"row_order"` | — | row ordering relative to reference or an explicit sort key |

Default full comparison (`reference` provided, no `checks`, no `exclude`) runs: `dimensions`, `names`, `types`, `values`.

**Individual checks:**

| Check | Type | Description |
|---|---|---|
| `nrow` | integer | Expected row count |
| `ncol` | integer | Expected column count |
| `names` | character vector | Column names that must be present (subset; does not require completeness) |
| `col_order` | `TRUE` or character vector | `TRUE`: column sequence must match reference; character vector: exact expected column sequence |
| `col_types` | named list | Column → expected class string, e.g. `list(year = "integer", gdp = "numeric")` |
| `values` | named list | Column → expected value vector; numeric tolerance applied |
| `row_order` | `TRUE` or character vector | `TRUE`: row order must match reference; character vector: rows must be sorted ascending by these columns |

**Examples:**

```r
# Full content comparison, ignore order (default)
validate(records, "df1", reference = ref)

# Full comparison + check that rows are sorted by year
validate(records, "df1", reference = ref, checks = "row_order")

# Check row order by explicit sort key (no reference needed)
validate(records, "df1", checks = list(row_order = c("country", "year")))

# Check dimensions and column names only
validate(records, "df1", reference = ref, checks = c("dimensions", "names"))

# Full comparison but skip value check (structure only)
validate(records, "df1", reference = ref, exclude = "values")

# Specific column type and row count without reference
validate(records, "df1", checks = list(
  nrow     = 50L,
  col_types = list(year = "integer", gdp = "numeric")
))
```

### `validate()` on `ggplot` objects

Default reference comparison: all groups below. Each group can be excluded individually via `exclude`.

**Check groups:**

| Group | Covers |
|---|---|
| `"aesthetics"` | all global and layer-level aesthetic mappings (x, y, color, fill, size, shape, alpha, group, linetype) |
| `"geoms"` | geom types present in all layers |
| `"stats"` | stat transformations for each layer |
| `"facets"` | faceting type and variables |
| `"scales"` | axis and colour scale settings |
| `"theme"` | theme elements (commonly excluded in teaching) |
| `"data"` | dataset used in the plot |

**Individual checks:**

| Check | Type | Description |
|---|---|---|
| `aes_x`, `aes_y`, `aes_color`, `aes_fill`, `aes_size`, `aes_shape`, `aes_alpha`, `aes_group`, `aes_linetype` | character | Expected aesthetic expression (e.g. `"factor(cyl)"`) |
| `geom` | character vector | Geom type(s) that must be present; accepts short form (`"point"`) or class name (`"GeomPoint"`) |
| `facet_var` | character vector | Faceting variable(s) that must be present |
| `facet_type` | character | `"wrap"` or `"grid"` |
| `stat` | character vector | Stat type(s) that must be present |

Notes:
- Aesthetics are checked in both global (`plot$mapping`) and per-layer mappings.
- `aes_color` and `aes_colour` are treated as equivalent.
- `facet_var` works for both `facet_wrap()` and `facet_grid()`.

### `validate()` on `model` objects

**Target classes:** `lm`, `glm`, `feols`/`feglm` (fixest). Random-effects models (`lmer`, `glmer`) are Level 2.

All groups are included in the default full comparison when `reference` is provided.

**Check groups:**

| Group | Default | Covers |
|---|---|---|
| `"estimator"` | ✓ | model class; family and link function for GLMs |
| `"outcome"` | ✓ | dependent variable |
| `"predictors"` | ✓ | RHS main-effect variables (presence and count) |
| `"interactions"` | ✓ | interaction terms present in the formula |
| `"effects"` | ✓ | fixed effects variables; whether any FE are present |
| `"inference"` | ✓ | clustering variables; SE / vcov type |
| `"weights"` | ✓ | weighting variable; whether weights are used at all |
| `"sample"` | ✓ | number of observations; model frame (data used) |

**Individual checks:**

*Estimator:*

| Check | Type | Description |
|---|---|---|
| `estimator` | character | Expected model class, e.g. `"lm"`, `"glm"`, `"feols"` |
| `family` | character | GLM family, e.g. `"binomial"`, `"poisson"` |
| `link` | character | GLM link function, e.g. `"logit"`, `"log"` |

*Outcome:*

| Check | Type | Description |
|---|---|---|
| `outcome` | character | Expected dependent variable name |

*Predictors:*

| Check | Type | Description |
|---|---|---|
| `predictors` | character vector | Variables that must appear as main effects on the RHS |
| `n_predictors` | integer | Exact number of main-effect predictors |
| `predictors_only` | logical | If `TRUE`, no predictors beyond those listed in `predictors` are allowed |

*Interactions:*

| Check | Type | Description |
|---|---|---|
| `interactions` | character vector | Interaction terms that must be present; order-insensitive (`"wt:hp"` matches `"hp:wt"`) |
| `n_interactions` | integer | Exact number of interaction terms |

*Effects:*

| Check | Type | Description |
|---|---|---|
| `fixed_effects` | character vector | FE variables that must be present |
| `n_fixed_effects` | integer | Exact number of FE dimensions |
| `has_fe` | logical | Whether any fixed effects are present at all |

*Inference:*

| Check | Type | Description |
|---|---|---|
| `cluster` | character vector | Clustering variable(s) that must be used |
| `vcov` | character | SE type: `"iid"`, `"HC1"`, `"HC2"`, `"HC3"`, `"cluster"`, `"twoway"` |

*Weights:*

| Check | Type | Description |
|---|---|---|
| `weights` | character | Name of the weighting variable |
| `has_weights` | logical | Whether any weights are used at all |

*Sample:*

| Check | Type | Description |
|---|---|---|
| `nobs` | integer | Expected number of observations |
| `data` | character | Name of the data object the model was fitted on |

**Notes on extraction:**

- For `feols`/`feglm`: fixed effects are in `obj$fixef_vars`; clustering from `obj$call$cluster` or `obj$call$vcov`.
- For `glm`: family and link via `family(obj)$family` and `family(obj)$link`.
- Sample check via reference uses `model.frame()` comparison (catches subsetting and filtering, not just `nobs`).
- `data` check compares the name of the data argument in the model call (`obj$call$data`) against the expected string.

**Examples:**

```r
# Full comparison against reference model
validate(records, "m1", reference = ref_model)

# Check estimator, outcome and clustering only
validate(records, "m1", reference = ref_model,
         checks = c("estimator", "outcome", "inference"))

# Check specific properties without reference
validate(records, "m1", checks = list(
  estimator    = "feols",
  outcome      = "log_wage",
  predictors   = c("education", "experience"),
  interactions = "education:experience",
  fixed_effects = c("industry", "year"),
  cluster      = "firm_id"
))

# Full comparison but skip sample check
validate(records, "m1", reference = ref_model, exclude = "sample")

# Verify that exactly the listed predictors are used (no extras)
validate(records, "m1", checks = list(
  predictors      = c("wt", "hp"),
  predictors_only = TRUE,
  n_interactions  = 0L
))
```

### `validate()` on `table` objects

**Target classes:** `gt_tbl`, `tinytable`, `flextable`, `huxtable`.

The primary teaching use case is regression tables (produced via modelsummary or similar). Checks focus on what models are shown, which terms and GOF rows appear, and what type of uncertainty is reported. Formatting details (labels, notes) are opt-in.

**Implementation note:** each table class stores content differently. Extracting cell values, row labels, and column headers requires a backend-specific adapter per class. This must be implemented before any group-level validation can run.

**Check groups:**

| Group | Default | Covers |
|---|---|---|
| `"dimensions"` | ✓ | number of body rows and body columns |
| `"models"` | ✓ | number of model columns in a regression table |
| `"terms"` | ✓ | coefficient/variable row labels present in the table body |
| `"gof"` | ✓ | goodness-of-fit row labels present (R², N, etc.) |
| `"inference"` | ✓ | type of uncertainty shown (SE, CI, p-value, t-stat) |
| `"labels"` | — | column headers and coefficient display labels |
| `"notes"` | — | table footnotes and source notes |

Default full comparison (`reference` provided, no `checks`, no `exclude`) runs: `dimensions`, `models`, `terms`, `gof`, `inference`.

`"labels"` and `"notes"` are opt-in because they reflect presentation choices rather than substantive content, and are rarely the focus of a grading rubric.

**Individual checks:**

| Check | Type | Description |
|---|---|---|
| `nrow` | integer | Expected number of body rows |
| `ncol` | integer | Expected number of body columns |
| `n_models` | integer | Expected number of model columns |
| `terms` | character vector | Coefficient/variable labels that must be present as row labels |
| `terms_absent` | character vector | Labels that must *not* appear (e.g. omitted variables) |
| `gof` | character vector | GOF statistic labels that must be present (e.g. `c("R2", "Num.Obs.")`) |
| `inference` | character | Uncertainty type: `"se"`, `"ci"`, `"p"`, or `"tstat"` |
| `col_labels` | character vector | Expected column header labels |

**Examples:**

```r
# Full comparison against reference table
validate(records, "tbl1", reference = ref_table)

# Full comparison, skip label check
validate(records, "tbl1", reference = ref_table, exclude = "labels")

# Check that specific terms and GOF rows are present (no reference needed)
validate(records, "tbl1", checks = list(
  terms = c("wt", "hp", "wt:hp"),
  gof   = c("R2", "Num.Obs."),
  inference = "se"
))

# Check number of models and that certain terms are not shown
validate(records, "tbl1", checks = list(
  n_models     = 3L,
  terms_absent = c("(Intercept)")
))
```

---

## Implementation Roadmap

### Level 1 — Recorder (complete)

- [x] `record_start()` / `record_stop()` via `addTaskCallback()`
- [x] Assignment, visible-return, and print-call event capture
- [x] Print-call recovery for table classes via environment lookup
- [x] Type filtering via `record_start()` parameters
- [x] `expr_text` stored per record for `expr_contains` matching
- [x] `robjgrader_records` S3 class with print method
- [x] `get_records(type, name)` with filtering
- [x] `record_script(path)` -- parse → eval loop bypassing `addTaskCallback`; handles errors per-expression with `stop_on_error` toggle; evaluates into configurable `envir`

### Level 1 — Validator infrastructure (complete)

- [x] `validate()` dispatcher with `name` / `match` / `reference` / `checks` / `exclude` / `position`
- [x] Progressive semantic matching with backtrack and positional fallback
- [x] `expr_contains` matching via stored expression text
- [x] `robjgrader_result` S3 class with print method
- [x] `.resolve_groups()` helper: separates group names from individual checks, applies defaults and exclude

### Level 1 — Validators (complete)

- [x] `validate()` on `df`: individual checks (nrow, ncol, names, col_types, values, row_order)
- [x] `validate()` on `df`: group-level checks (dimensions, names, col_order, types, values, row_order) with reference and exclude
- [x] `validate()` on `ggplot`: individual checks (aes_*, geom, facet_var)
- [x] `validate()` on `ggplot`: group-level checks (aesthetics, geoms, facets, stats, theme, data) with reference and exclude
- [x] `validate()` on `model`: individual checks (outcome, predictors, interactions, fixed_effects, cluster, family, nobs, ...)
- [x] `validate()` on `model`: group-level checks (estimator, outcome, predictors, interactions, effects, inference, weights, sample) with reference and exclude
- [x] `validate()` on `table`: backend adapters for gt, flextable, tinytable, huxtable + group checks (dimensions, terms, models, labels)

### Level 1 — Package infrastructure (complete)

- [x] DESCRIPTION with Imports (rlang) and Suggests (ggplot2, fixest, gt, tinytable, flextable, huxtable, testthat)
- [x] NAMESPACE with exports, S3 methods, and importFrom(stats, ...)
- [x] `LazyData` field removed
- [x] roxygen2 documentation generated for all exported functions
- [x] `devtools::load_all()` runs without errors or warnings
- [x] `devtools::check()` passes with 0 errors, 0 warnings (3 unavoidable Notes)

### Level 1 — Tests (complete)

- [x] `testthat` tests for recorder: internal helpers, callback simulation, get_records() filtering (28 tests)
- [x] `testthat` tests for `validate()` on df: group checks, individual checks, exclude, lookup (30 tests)
- [x] `testthat` tests for `validate()` on ggplot: group checks, individual checks, match-based lookup (27 tests)
- [x] `testthat` tests for `validate()` on model: group checks, individual checks, GLM, fixest, match-based lookup (30 tests)
- [x] `testthat` tests for `validate()` on table: gt and flextable backends, group checks, individual checks (14 tests)
- [x] `testthat` tests for `record_script()`: file errors, type filtering, error recovery, envir isolation, `source_student_file()` integration (33 tests)

### Level 2 — Next

- [x] `record_script(path)` for batch grading (parse → eval without modifying script)

#### P1 — Table capture without assignment ✓ DONE

Table-generating functions often return their object **invisibly**, and students rarely assign them to a variable. This causes silent data loss in `record_script()`.

Two distinct failure modes, both rooted in `.recorder_callback()`:

**Failure mode A — invisible return, no assignment:**

```r
modelsummary(list(m1, m2), output = "gt")  # returns gt_tbl invisibly
```

`withVisible(eval(expr))` returns `visible = FALSE`. The callback logic drops any object where `visible = FALSE` and the expression is neither an assignment nor a `print()` call:

```r
} else {
  return(TRUE)  # silently dropped ← BUG for table classes
}
```

**Fix:** After `obj_type` is resolved, if it is `"table"` and `visible = FALSE` and no `obj_name`, capture anyway as a new `event_type = "invisible_return"`. Do NOT generalise to other types — invisible dfs and models are rarely intentional outputs.

**Failure mode B — `print()` with inline expression:**

```r
print(gt(df))        # expr[[2L]] is a call, not a name
gt(df) |> print()    # same
```

The existing recovery logic:

```r
if (.is_print_call(expr) && is.name(expr[[2L]])) {
  recovered <- get(as.character(expr[[2L]]), envir = globalenv())
}
```

only runs when the print argument is a bare name (`print(tbl)`). For inline expressions, `expr[[2L]]` is a call and the guard `is.name(expr[[2L]])` fails, so `value` stays as whatever `print.gt_tbl()` / `print.flextable()` returns (e.g. `shiny.tag`, `NULL`) — none of which are a table class → dropped.

**Fix:** Extend the recovery branch: when `is.call(expr[[2L]])`, evaluate `expr[[2L]]` in `envir` to recover the table object before classifying.

**Affected classes by mode:**

| Class | `print()` return value | Invisible from call? | Mode A risk | Mode B risk |
|---|---|---|---|---|
| `gt_tbl` | `shiny.tag` | sometimes (modelsummary) | yes | yes |
| `tinytable` | `tinytable_grid` | yes (by default) | yes | yes |
| `flextable` | `NULL` | sometimes | yes | yes |
| `huxtable` | not tested | unknown | likely | likely |

**Scope of change:** `recorder.R` only — `.recorder_callback()`, plus `.recorder_env$envir` field set in `record_start()` and `record_script()`. No changes needed to validators or `record_script()` eval loop.

**Tests:** 6 new unit tests in `test-recorder.R` (direct callback simulation, no package needed); 5 new end-to-end tests in `test-record-script.R` (including 3 `skip_if_not_installed("gt")`). 284 total, 0 failures.

---

#### P2 — Autograder template ✓ DONE

- [x] `autograder/autograde_template.R`: annotated template covering df, ggplot, model, and table checks; `# === CUSTOMIZE ===` markers for PS-specific data and reference objects; covers `source_student_file()` → `validate()` → `run_autograder()` full workflow

#### P3 — Validator gaps ✓ DONE

- [x] `validate_table`: `gof` group (GOF-label heuristic + reference comparison) and `inference` group (SE/CI detection from body cell patterns) implemented; both in default groups
- [x] `validate_plot`: `facet_type` individual check (`"wrap"` / `"grid"`)
- [x] `validate_plot`: `stat` individual check with `.normalize_stat()` helper

#### P4 — Infrastructure

- [x] Random-effects model support (`lmerMod`, `glmerMod`) via lme4/reformulas: `random_effects`, `n_random_effects`, `has_re` checks; `random_effects` group in defaults; `nobars`/`findbars` via reformulas fallback
- [ ] Disk-backed storage backend via `saveRDS()` — not needed for Gradescope use case
- [ ] Summary report output (markdown / HTML) — not needed currently

### Out of scope

- Assignments inside nested function bodies
- Universal tracing of non-standard evaluation paths
- Provenance across `source()`d child scripts or parallel workers
- Code-graph / dependency analysis across objects

---

## Tests Before Deploy

Manual checks that must be run and confirmed before the corresponding code is considered ready. Each item should be verified interactively in a clean R session.

### Recorder

| # | Test | What to verify |
|---|---|---|
| R1 | `ggplot(mtcars, aes(x = wt, y = mpg))` at top level | Captured as `visible_return`; `object_name = NULL`; object is a valid `ggplot` |
| R2 | `ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()` across two lines | Treated as single expression; captured once, not twice |
| R3 | `p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()` | Captured as `assignment`; `object_name = "p"` |
| R4 | `p` (bare name at top level after R3) | Captured as second event (`visible_return`); same object, different event type |
| R5 | `mtcars \|> gt()` | Captured as `visible_return`; `object_type = "table"` |
| R6 | `gt(mtcars)` | Same as R5 |
| R7 | `print(gt(mtcars))` | Captured as `print_call`; check that `value` is the `gt_tbl`, not `NULL` |
| R8 | Assign, then overwrite: `x <- lm(mpg ~ wt, mtcars)` then `x <- lm(mpg ~ hp, mtcars)` | Both versions recorded; `get_records(name = "x")` returns two entries |
| R9 | `record_start()` called while already active | Warning issued; no duplicate callback registered |
| R10 | `record_start(record_df = FALSE)` then create a data frame | Data frame not captured; other types still captured |

### Print-method return values (table classes)

| Class | Package | `print()` returns | `visible` | Task callback sees for `data \|> Class()` | Status |
|---|---|---|---|---|---|
| `gt_tbl` | gt | `shiny.tag` (rendered HTML) | FALSE | `gt_tbl` (expression return) | PASS for assignment/visible_return; FAIL for `print_call` |
| `tt` | tinytable | `tinytable_grid` (not original) | FALSE | `tt` object (expression return) | PASS for assignment/visible_return; FAIL for `print_call` |
| `flextable` | flextable | `NULL` | FALSE | `flextable` object (expression return) | PASS for assignment/visible_return; FAIL for `print_call` |
| `huxtable` | huxtable | not tested (not installed) | — | — | pending |

**Key architectural finding:** The task callback `value` is the return value of the **expression**, not of `print()`. This distinction matters:

- `data |> gt()` → expression returns `gt_tbl` visibly → callback sees `gt_tbl` ✓
- `tbl` (bare name) → expression evaluates to `gt_tbl` → callback sees `gt_tbl` ✓
- `print(tbl)` → expression IS the `print()` call → callback sees whatever `print.gt_tbl()` returns (`shiny.tag`) ✗

**Implementation consequence:** `print_call` event capture for table objects **cannot rely on `value`**. When `expr` is a `print()` call, the recorder must parse the argument from `expr` and retrieve the object directly from the calling environment:

```r
# Inside the task callback, when event_type == "print_call":
arg_name <- as.character(expr[[2]])          # e.g. "tbl"
obj      <- get(arg_name, envir = parent.env) # look up original object
```

This is already different from ggplot, where `print.ggplot()` returns the plot invisibly and `value` is the correct object. Table classes require environment-based retrieval for print-call interception.

### ggplot internal structure

These verify that the extraction helpers used in `validate_plot` work correctly across ggplot2 versions.

| # | Test | Result | Notes |
|---|---|---|---|
| G1 | Global aes: `ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl)))` | PASS | `names(p$mapping)` = `x, y, colour` (British spelling); `rlang::as_label()` returns `"wt"`, `"mpg"`, `"factor(cyl)"` correctly |
| G2 | Layer aes: `ggplot(mtcars) + geom_point(aes(x = wt, y = mpg))` | PASS | Global `p$mapping` is empty; aesthetics in `p$layers[[1]]$mapping`; `class(layer$geom)[1]` = `"GeomPoint"` |
| G3 | `facet_wrap(~ cyl)` | PASS | `class(p$facet)[1]` = `"FacetWrap"`; `names(p$facet$params$facets)` = `"cyl"`; `rlang::as_label()` not needed — `names()` sufficient |
| G4 | `facet_grid(cyl ~ am)` | PASS | `class(p$facet)[1]` = `"FacetGrid"`; `names(p$facet$params$rows)` = `"cyl"`; `names(p$facet$params$cols)` = `"am"` |

**Implementation notes from G1–G4:**
- Always normalise `"color"` → `"colour"` before looking up in `p$mapping`.
- Facet variable names are available via `names()` on `$params$facets` / `$params$rows` / `$params$cols` — no `rlang` needed for facets.
- `names(p$mapping)` returns `""` (empty string, length 1) when no global aesthetics are set; guard against this.

### fixest internal structure

| # | Test | Result | Notes |
|---|---|---|---|
| F1 | `feols(mpg ~ wt + hp \| cyl, mtcars)` | PASS | `obj$fml` = `mpg ~ wt + hp` (FE stripped); `obj$fixef_vars` = `"cyl"`; outcome via `obj$fml[[2]]` |
| F2 | `feols(mpg ~ wt, cluster = ~ cyl, mtcars)` | PASS (workaround) | `obj$se_type` is `NULL` — unusable. Use `attr(obj$cov.scaled, "type")` instead. Returns `"Clustered (cyl)"` or `"Clustered (cyl & am)"` for two-way; empty string for no clustering. Requires regex parsing to extract variable names. |
| F3 | `feols(mpg ~ wt \| cyl + am, mtcars)` | PASS | `obj$fixef_vars` = `c("cyl", "am")` |

**Implementation notes from F1–F3:**
- **Do not use `obj$se_type`** — it is `NULL` regardless of clustering. The only reliable source is `attr(obj$cov.scaled, "type")`.
- Parse clustering variables from the type string: `"Clustered (cyl)"` → `"cyl"`; `"Clustered (cyl & am)"` → `c("cyl", "am")`. A simple regex is sufficient.
- `obj$fml` contains the linear part only (no FE). RHS variables via `all.vars(obj$fml[[3]])`.

### Matching

| # | Test | What to verify |
|---|---|---|
| M1 | Two ggplots recorded; `match = list(type = "ggplot", aes_x = "wt")` | Returns only the plot with `x = wt` |
| M2 | Two ggplots with same `aes_x`; `match` after progressive narrowing still yields 2 | Warning issued; `position = "last"` selects the second |
| M3 | `expr_contains = "gapminder"` with two plots from different datasets | Correct plot identified via expression text |
| M4 | `match` criterion reduces candidates to 0; backtrack occurs | Falls back to previous candidate set; warning issued |

---

## Key Files

```
Robjgrader/
  R/
    recorder.R         # record_start/stop, record_script(), task callback, .recorder_env
    autograder.R       # source_student_file(), run_autograder(), grab(), result_to_outcome()
    validate.R         # validate() dispatcher, robjgrader_result S3 class
    validate_df.R      # .validate_df()
    validate_plot.R    # .validate_plot(), ggplot helpers
    validate_model.R   # .validate_model(), model helpers
    validate_table.R   # .validate_table(), backend adapters (gt, flextable, tinytable, huxtable)
  tests/
    testthat/
      helper.R                  # make_records() test helper
      test-recorder.R           # 28 tests
      test-record-script.R      # 33 tests
      test-validate-df.R        # 30 tests
      test-validate-plot.R      # 27 tests
      test-validate-model.R     # 30 tests
      test-validate-table.R     # 14 tests
  man/
  DESCRIPTION          # Imports: rlang, jsonlite; Suggests: ggplot2, fixest, gt, ...
  NAMESPACE
autograder/
  runner.R             # standalone runner (pre-package reference copy)
  extract_code.R       # source-code inspection helpers (extract_cleaned_chunks etc.)
PLAN.md
```

---

## Open Questions

1. ~~**Interactive vs. script mode**~~ — resolved: `record_script()` handles batch grading via parse → eval; `record_start()`/`record_stop()` remain for interactive use.
2. ~~**`source()` calls inside student scripts**~~ — resolved: `record_script()` evaluates top-level expressions directly without `source()`, so nested `source()` calls are still not intercepted but are out of scope.
3. **`recordedplot` fallback** — useful for base graphics but captures only a rendered bitmap, not an analytical specification. Should be clearly labelled as degraded when implemented.
4. **Package name** — `Robjgrader` is a working name. Alternatives to consider: `objcheck`, `recval`, `scriptval`.
5. **`modelsummary` integration** — modelsummary produces intermediate list objects before rendering; decide whether to intercept the list or the rendered table output.
