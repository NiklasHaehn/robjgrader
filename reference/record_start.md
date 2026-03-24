# Start recording analytical objects

Attaches a task callback that intercepts top-level assignments, visible
returns, and explicit print() calls for supported object classes.

## Usage

``` r
record_start(
  record_df = TRUE,
  record_ggplot = TRUE,
  record_model = TRUE,
  record_table = TRUE
)
```

## Arguments

- record_df:

  Record data frames and tibbles. Default TRUE.

- record_ggplot:

  Record ggplot objects. Default TRUE.

- record_model:

  Record model objects (lm, glm, fixest). Default TRUE.

- record_table:

  Record table objects (gt, tinytable, flextable, huxtable). Default
  TRUE.

## Value

Invisibly NULL. Called for its side effect.
