# Run autograder test cases and write Gradescope-compatible JSON

Iterates over `test_cases`, calls each test function, compares the
return value to the expected value, accumulates scores, and writes the
results to a JSON file that Gradescope can parse. Also prints a summary
to the console when `verbose = TRUE`.

## Usage

``` r
run_autograder(test_cases, json_path = .autograder_json_path(), verbose = TRUE)
```

## Arguments

- test_cases:

  A list of test-case lists. Each entry must contain:

  `name`

  :   Human-readable criterion label shown to students.

  `fun`

  :   The test function: a function object or a character string naming
      a function visible in the calling environment.

  `args`

  :   A list of arguments passed to `fun` via `do.call`.

  `expect`

  :   Expected return value; compared via `.ag_to_string()`.

  `visibility`

  :   `"visible"` (default) or `"hidden"`.

  `weight`

  :   Point value for this criterion.

- json_path:

  Output path for the JSON results file. Defaults to
  `"/autograder/results/results.json"` inside a Gradescope container and
  `"results.json"` otherwise (detected via the `ASSIGNMENT_TITLE`
  environment variable).

- verbose:

  Logical. Print per-test summaries to the console. Default `TRUE`.

## Value

The results list, invisibly.
