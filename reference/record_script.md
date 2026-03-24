# Record all objects produced by sourcing an R script

Parses `path` with [`parse()`](https://rdrr.io/r/base/parse.html),
evaluates each expression one at a time inside `envir`, and passes the
result directly to the recorder callback – bypassing `addTaskCallback`,
which is never fired during
[`source()`](https://rdrr.io/r/base/source.html). This is the reliable
way to record objects from a student submission file.

## Usage

``` r
record_script(
  path,
  envir = .GlobalEnv,
  stop_on_error = FALSE,
  record_df = TRUE,
  record_ggplot = TRUE,
  record_model = TRUE,
  record_table = TRUE
)
```

## Arguments

- path:

  Path to the R script to evaluate.

- envir:

  Environment in which to evaluate expressions. Default `.GlobalEnv`.

- stop_on_error:

  Logical. If `TRUE` the first error in the script halts evaluation and
  re-throws. If `FALSE` (default) each failing expression emits a
  [`warning()`](https://rdrr.io/r/base/warning.html) and evaluation
  continues.

- record_df:

  Record data frames and tibbles. Default `TRUE`.

- record_ggplot:

  Record ggplot objects. Default `TRUE`.

- record_model:

  Record model objects. Default `TRUE`.

- record_table:

  Record table objects. Default `TRUE`.

## Value

A `robjgrader_records` object, returned invisibly.
