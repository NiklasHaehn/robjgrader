# Split a student text submission into sections

Splits a multi-question text submission into individual sections so that
each question can be graded independently via
[`validate_text()`](https://niklashaehn.github.io/robjgrader/reference/validate_text.md).
Section boundaries are detected automatically based on the content
format:

## Usage

``` r
split_student_text(text, n_sections = NULL, format = "auto")
```

## Arguments

- text:

  Character. Full text from
  [`read_student_text()`](https://niklashaehn.github.io/robjgrader/reference/read_student_text.md)
  or
  [`find_student_text()`](https://niklashaehn.github.io/robjgrader/reference/find_student_text.md).

- n_sections:

  Integer or `NULL`. Expected number of sections. Used as a limit when
  falling back to blank-line splitting.

- format:

  One of `"auto"` (default), `"markdown"`, or `"plain"`. `"auto"`
  detects the format from the content.

## Value

A named character vector. Names are the detected section headings (e.g.
`"Q1: Interpretation"`); values are the section bodies. Unnamed sections
are labelled `"Section 1"`, `"Section 2"`, etc.

## Details

- Markdown (`.md`):

  Lines starting with one to three `#` characters (`# Q1`, `## Part 2`,
  etc.).

- Plain text / PDF (`.txt`, `.pdf`):

  Lines matching common question prefixes: `Q1:`, `Question 1.`, `1.`,
  `1)`, `Part 1`, `Section 1`. When no prefix pattern is found, the text
  is split on double blank lines.
