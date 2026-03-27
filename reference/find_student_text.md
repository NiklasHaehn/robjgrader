# Find and read a student text submission automatically

Scans the current working directory for text files (`.txt`, `.md`,
`.pdf`), excludes the calling autograder script, and returns the content
of the first matching file as a character string. Mirrors the
auto-discovery logic of
[`source_student_file()`](https://niklashaehn.github.io/robjgrader/reference/source_student_file.md).

## Usage

``` r
find_student_text(pattern = NULL, autograder_name = NULL)
```

## Arguments

- pattern:

  Optional regex to filter filenames (e.g. `"interpretation"`). When
  `NULL`, all supported text formats are considered.

- autograder_name:

  Character or `NULL`. Additional filename(s) to exclude beyond the
  auto-detected calling script.

## Value

A single character string with the file contents.
