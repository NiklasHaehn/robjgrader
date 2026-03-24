# Code-extraction helpers for source-code-based autograder checks.
#
# Source this file when your test functions need to inspect the student's
# source code directly (as opposed to grading the produced objects).
#
# Typical usage:
#   chunks  <- extract_cleaned_chunks(student_file)
#   ggplots <- extract_all_ggplots(student_file)
#   if (length(ggplots) == 0) return("No ggplot found.")

suppressPackageStartupMessages({
  library(stringr)
  library(purrr)
  library(dplyr)
  library(readr)
  library(tibble)
})


# ---- Internal parsing pipeline -----------------------------------------------

# Splits a raw code string (from read_file) into trimmed, non-empty lines.
.split_into_lines <- function(code_string) {
  str_split(code_string, "\n")[[1L]] |>
    map_chr(str_trim) |>
    discard(~ .x == "")
}

# Removes everything after a `#` comment marker on each line.
.delete_comments <- function(lines) {
  lines |>
    str_remove_all("#.*") |>
    map_chr(str_trim) |>
    discard(~ .x == "")
}

# Strips double-quote characters (useful before pattern matching on string args).
.strip_quotes <- function(lines) {
  str_remove_all(lines, '"')
}

# Collapses continuation lines (pipes, commas, open brackets) into single
# logical expressions, making multi-line calls searchable as one string.
.collapse_continuations <- function(lines) {
  depth_par <- cumsum(str_count(lines, fixed("(")) - str_count(lines, fixed(")")))
  depth_brc <- cumsum(str_count(lines, fixed("{")) - str_count(lines, fixed("}")))

  cont <- str_ends(lines, "%>%|\\|>|,|\\+|\\(|\\{") |
    (depth_par > 0) | (depth_brc > 0)
  grp  <- cumsum(!lag(cont, default = FALSE))

  tibble(line = lines, grp = grp) |>
    group_by(grp) |>
    summarise(collapsed = str_c(line, collapse = " "), .groups = "drop") |>
    pull(collapsed)
}


# ---- Public API --------------------------------------------------------------

# Returns a character vector of cleaned, collapsed logical expressions from an
# R script file. Removes comments and collapses multi-line calls.
extract_cleaned_chunks <- function(R_script_path) {
  read_file(R_script_path) |>
    .split_into_lines() |>
    .delete_comments() |>
    .strip_quotes() |>
    .collapse_continuations()
}

# Returns all expressions that contain a ggplot() call.
extract_all_ggplots <- function(R_script_path) {
  extract_cleaned_chunks(R_script_path) |>
    str_subset("ggplot\\(")
}

# Returns all expressions that contain a modelsummary() call.
extract_all_modelsummaries <- function(R_script_path) {
  extract_cleaned_chunks(R_script_path) |>
    str_subset("modelsummary\\(")
}

# Returns all expressions that contain a gt() call.
extract_all_gt <- function(R_script_path) {
  extract_cleaned_chunks(R_script_path) |>
    str_subset("\\bgt\\(")
}
