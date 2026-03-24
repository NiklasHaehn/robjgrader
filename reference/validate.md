# Validate a recorded object

Retrieves an object from a `robjgrader_records` result and validates it
against a reference object, a set of named checks, or both.

## Usage

``` r
validate(
  records,
  name = NULL,
  match = list(),
  reference = NULL,
  checks = list(),
  exclude = NULL,
  position = "last"
)
```

## Arguments

- records:

  A `robjgrader_records` object from
  [`get_records()`](https://niklashaehn.github.io/robjgrader/reference/get_records.md).

- name:

  Character. Variable name of the recorded object. Primary
  identification method.

- match:

  Named list of coarse matching criteria used when `name` is absent or
  unknown (e.g. student chose a different variable name, or the object
  was never assigned). Must include `type`; all other keys are optional.
  Criteria are applied in order of discriminating power (most to least)
  until one candidate remains; a criterion that would eliminate all
  remaining candidates is skipped.

  **Universal (all types):**

  `type`

  :   Required. One of `"ggplot"`, `"model"`, `"df"`, `"table"`.

  `expr_contains`

  :   Character. Fixed substring matched against the source expression
      text (e.g. `"filter(year == 2007)"`).

  **ggplot:**

  `aes_x`, `aes_y`, `aes_color`, `aes_colour`, `aes_fill`, `aes_size`, `aes_shape`, `aes_alpha`, `aes_group`, `aes_linetype`

  :   Character. Expected variable name mapped to that aesthetic.

  `geom`

  :   Character. Geom class name, either short (`"point"`) or full
      (`"GeomPoint"`). Matches if any layer uses that geom.

  `facet_var`

  :   Character vector. Variable name(s) that must appear in the facet
      specification.

  **model:**

  `outcome`

  :   Character. Name of the response variable (LHS).

  `estimator`

  :   Character. Class the model must inherit from, e.g. `"lm"`,
      `"glm"`, `"fixest"`.

  `predictors`

  :   Character vector. RHS variable names that must all be present in
      the model formula.

  `fixed_effects`

  :   Character vector. Fixed-effect variable names (relevant for fixest
      models).

  `cluster`

  :   Character vector. Clustering variable names.

  `nobs`

  :   Integer. Exact number of observations.

  **df:**

  `names`

  :   Character vector. Column names that must all be present.

  `nrow`

  :   Integer. Exact number of rows.

  `ncol`

  :   Integer. Exact number of columns.

  **table:**

  `expr_contains`

  :   Only `expr_contains` is effective for tables; structural criteria
      are not yet supported.

- reference:

  Optional reference object. Serves two roles: (1) when neither `name`
  nor `match` is given, the reference is used to locate the
  best-matching candidate automatically by extracting structural
  criteria (type, aesthetics, model outcome, column names, etc.) and
  running them through the normal match logic; (2) when a candidate has
  been identified, the reference is used for group-level semantic
  comparison.

- checks:

  Named list of type-specific property checks, or a character vector of
  group names to check against `reference`.

- exclude:

  Character vector of group names to skip when `reference` is provided.
  Individual `checks` are unaffected.

- position:

  Positional fallback when matching yields \> 1 candidate: `"last"`
  (default), `"first"`, or an integer index among the filtered
  candidates.

## Value

A `robjgrader_result` object.
