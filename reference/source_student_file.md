# Source a student submission and return recorded objects

Sources a student R file from the current working directory and builds a
`robjgrader_records` object from all new named objects of a recordable
type that appear in `.GlobalEnv` after sourcing.

## Usage

``` r
source_student_file(
  autograder_name = NULL,
  record_types = c("df", "ggplot", "model", "table")
)
```

## Arguments

- autograder_name:

  Character or `NULL`. Additional filename(s) to exclude beyond the
  auto-detected calling file. Default `NULL`.

- record_types:

  Character vector of object types to capture. Any subset of
  `c("df", "ggplot", "model", "table")`.

## Value

A `robjgrader_records` object with a `"student_file"` attribute,
returned invisibly.

## Details

The calling script is automatically excluded from the candidate list —
no manual `autograder_name` argument is needed in the typical case.
Detection works both when the autograder is run via
`Rscript autograde.R` (reads `--file=` from
[`commandArgs()`](https://rdrr.io/r/base/commandArgs.html)) and when it
is loaded interactively via `source("autograde.R")` (walks
[`sys.calls()`](https://rdrr.io/r/base/sys.parent.html)). Pass
`autograder_name` to add further exclusions.

The student file path is stored as `attr(records, "student_file")` and
is available to code-inspection functions such as those in
`extract_code.R`.
