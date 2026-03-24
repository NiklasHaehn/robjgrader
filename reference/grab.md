# Wrap a named global object into a robjgrader_records list

Convenience helper: fetches `name` from `envir` and returns a
one-element `robjgrader_records` object ready to pass to
[`validate()`](https://niklashaehn.github.io/robjgrader/reference/validate.md).
Useful when a test function needs to validate a single known object
without holding the full records from
[`source_student_file()`](https://niklashaehn.github.io/robjgrader/reference/source_student_file.md).

## Usage

``` r
grab(name, envir = .GlobalEnv)
```

## Arguments

- name:

  Character. Name of the object to fetch.

- envir:

  Environment to look in. Default `.GlobalEnv`.

## Value

A `robjgrader_records` object with one entry.
