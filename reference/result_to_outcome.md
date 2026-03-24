# Convert a validate() result to an autograder outcome string

Maps a `robjgrader_result` to `"SUCCESS"` when all checks pass, or to a
concatenated string of all failing check messages otherwise.

## Usage

``` r
result_to_outcome(result)
```

## Arguments

- result:

  A `robjgrader_result` from
  [`validate()`](https://niklashaehn.github.io/robjgrader/reference/validate.md).

## Value

`"SUCCESS"` or a newline-separated string of all failing messages.
