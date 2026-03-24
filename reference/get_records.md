# Retrieve recorded objects

Returns a list of recorded events. Each element contains event metadata
and the captured object under `$object`.

## Usage

``` r
get_records(type = NULL, name = NULL)
```

## Arguments

- type:

  Character vector of object types to include. One or more of `"df"`,
  `"ggplot"`, `"model"`, `"table"`. `NULL` (default) returns all types.

- name:

  Character vector of object names to include. Matches the left-hand
  side of assignments. `NULL` (default) returns all names, including
  anonymous visible returns.

## Value

An object of class `robjgrader_records` (a named list of record
entries).
