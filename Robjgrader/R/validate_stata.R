#' Read a Stata .do file as a character string
#'
#' Reads a \code{.do} file from disk and returns its content as a single
#' character string with lines separated by \code{"\n"}.
#'
#' @param path Character. Path to the \code{.do} file.
#'
#' @return A character string.
#' @export
read_do_file <- function(path) {
  if (!file.exists(path))
    stop(sprintf("File not found: '%s'", path))
  paste(readLines(path, warn = FALSE), collapse = "\n")
}


#' Split Stata code into typed sections
#'
#' Classifies each line of Stata code into one of seven section types based on
#' command patterns, and returns a named list of character vectors — one per
#' section type.
#'
#' @details
#' Lines that match no known command pattern are placed in the
#' \code{"other"} section.  Comments (\code{*} and \code{//}) are excluded from
#' all section assignments.
#'
#' Section types: \code{"packages"}, \code{"setup"}, \code{"data_load"},
#' \code{"data_mgmt"}, \code{"estimation"}, \code{"tables"}, \code{"figures"},
#' \code{"other"}.
#'
#' @param code Character string. Stata source code (as returned by
#'   \code{read_do_file()}).
#'
#' @return A named list of character vectors, one element per section type.
#' @export
split_do_file <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]

  .stata_section_patterns <- list(
    packages   = "^\\s*(ssc\\s+install|net\\s+install|ado\\s+)",
    setup      = "^\\s*(set\\s+|clear\\s*$|version\\s+)",
    data_load  = "^\\s*(use\\s+|import\\s+|insheet\\s+|infile\\s+|load\\s+)",
    data_mgmt  = paste0(
      "^\\s*(keep\\s+|drop\\s+|rename\\s+|gen(erate)?\\s+|replace\\s+|",
      "merge\\s+|reshape\\s+|label\\s+|encode\\s+|decode\\s+|xtset\\s+|",
      "tsset\\s+|sort\\s+|collapse\\s+|duplicates\\s+|destring\\s+|",
      "tostring\\s+|egen\\s+)"
    ),
    estimation = paste0(
      "^\\s*(reg(ress)?\\s+|logit\\s+|probit\\s+|xtreg\\s+|ivregress\\s+|",
      "reghdfe\\s+|areg\\s+|poisson\\s+|oprobit\\s+|ologit\\s+|tobit\\s+|",
      "heckman\\s+|ivreg2\\s+|ppmlhdfe\\s+|clogit\\s+|nbreg\\s+|zinb\\s+|",
      "zip\\s+|xtlogit\\s+|xtprobit\\s+|xtpoisson\\s+|mixed\\s+|melogit\\s+)"
    ),
    tables     = paste0(
      "^\\s*(esttab\\s+|outreg2?\\s+|tabulate\\s+|tab\\s+|estout\\s+|",
      "putexcel\\s+|matrix\\s+list|eststo\\s+|estimates\\s+store|",
      "table\\s+|tabstat\\s+|sutex\\s+|asdoc\\s+)"
    ),
    figures    = paste0(
      "^\\s*(graph\\s+|scatter\\s+|line\\s+|histogram\\s+|twoway\\s+|",
      "binscatter\\s+|coefplot\\s+|kdensity\\s+|ciplot\\s+|rddplot\\s+)"
    )
  )

  comment_re <- "^\\s*(\\*|//)"

  sections <- lapply(names(.stata_section_patterns), function(x) character(0L))
  names(sections) <- names(.stata_section_patterns)
  sections[["other"]] <- character(0L)

  for (line in lines) {
    if (grepl(comment_re, line, perl = TRUE)) next
    if (!nzchar(trimws(line)))                 next

    matched <- FALSE
    for (stype in names(.stata_section_patterns)) {
      if (grepl(.stata_section_patterns[[stype]], line, perl = TRUE,
                ignore.case = TRUE)) {
        sections[[stype]] <- c(sections[[stype]], line)
        matched <- TRUE
        break
      }
    }
    if (!matched) sections[["other"]] <- c(sections[["other"]], line)
  }

  sections
}


#' Validate a Stata .do file
#'
#' Checks a Stata code string (or file path) against a set of regex-based
#' criteria.  Optionally restricts checks to a specific code section as
#' returned by \code{split_do_file()}.
#'
#' @details
#' \strong{checks} is a named list supporting the following keys:
#' \describe{
#'   \item{\code{contains}}{Character vector. Each pattern must match at least
#'     one line in the target code (regex, case-sensitive by default).}
#'   \item{\code{not_contains}}{Character vector. Each pattern must NOT match
#'     any line in the target code.}
#'   \item{\code{command}}{Character vector. Each string must appear as a
#'     Stata command at the start of a line (word-boundary matched,
#'     case-insensitive).}
#'   \item{\code{variable}}{Character vector. Each string must appear somewhere
#'     in the target code (word-boundary match).}
#'   \item{\code{n_commands}}{A list with \code{type} (section name),
#'     \code{expected} (integer), and optional \code{tolerance} (integer,
#'     default 0).  Checks the number of lines in the given section.}
#' }
#'
#' @param code     Character string or file path. Stata code to validate.
#' @param section  Character or \code{NULL}. Which section to run checks
#'   against: one of \code{"all"} (default), \code{"packages"},
#'   \code{"setup"}, \code{"data_load"}, \code{"data_mgmt"},
#'   \code{"estimation"}, \code{"tables"}, \code{"figures"}, \code{"other"}.
#'   Ignored when \code{"all"}.
#' @param reference Character string, file path, or \code{NULL}. Optional
#'   reference code; currently used for \code{n_commands}-style comparison
#'   if no explicit \code{checks} are provided.
#' @param ref_section Character or \code{NULL}. Section to extract from
#'   \code{reference} (same options as \code{section}).
#' @param checks   Named list of check specs.  See Details.
#' @param name     Character. Label for this result object.
#'   Default \code{"stata_code"}.
#'
#' @return A \code{robjgrader_result}.
#' @export
validate_do <- function(
  code,
  section     = NULL,
  reference   = NULL,
  ref_section = NULL,
  checks      = list(),
  name        = "stata_code"
) {
  # -- 1. Read code if path given -----------------------------------------------
  if (length(code) == 1L && file.exists(code))
    code <- read_do_file(code)

  # -- 2. Extract target section -------------------------------------------------
  sections <- split_do_file(code)
  target   <- if (!is.null(section) && section != "all" &&
                  section %in% names(sections)) {
    paste(sections[[section]], collapse = "\n")
  } else {
    code
  }

  # -- 3. Reference code (optional) ---------------------------------------------
  ref_code <- NULL
  if (!is.null(reference)) {
    if (length(reference) == 1L && file.exists(reference))
      reference <- read_do_file(reference)
    ref_sections <- split_do_file(reference)
    ref_code <- if (!is.null(ref_section) && ref_section != "all" &&
                    ref_section %in% names(ref_sections)) {
      paste(ref_sections[[ref_section]], collapse = "\n")
    } else {
      reference
    }
  }

  # -- 4. Run checks ------------------------------------------------------------
  results <- list()
  chk     <- if (is.list(checks)) checks else list()

  # contains
  for (pat in chk[["contains"]] %||% character(0L)) {
    key  <- paste0("contains.", gsub("[^a-zA-Z0-9]", "_", pat))
    pass <- grepl(pat, target, perl = TRUE)
    results[[key]] <- .make_check(
      key, pass, pat, target,
      if (pass) sprintf("Pattern '%s' found.", pat)
      else      sprintf("Pattern '%s' not found in code.", pat)
    )
  }

  # not_contains
  for (pat in chk[["not_contains"]] %||% character(0L)) {
    key  <- paste0("not_contains.", gsub("[^a-zA-Z0-9]", "_", pat))
    pass <- !grepl(pat, target, perl = TRUE)
    results[[key]] <- .make_check(
      key, pass, sprintf("no match for '%s'", pat), target,
      if (pass) sprintf("Forbidden pattern '%s' not found (good).", pat)
      else      sprintf("Forbidden pattern '%s' found in code.", pat)
    )
  }

  # command — word-boundary match at line start, case-insensitive
  for (cmd in chk[["command"]] %||% character(0L)) {
    key  <- paste0("command.", cmd)
    pat  <- sprintf("(?i)^\\s*%s\\b", cmd)
    pass <- grepl(pat, target, perl = TRUE)
    results[[key]] <- .make_check(
      key, pass, cmd, target,
      if (pass) sprintf("Command '%s' found.", cmd)
      else      sprintf("Command '%s' not found in code.", cmd)
    )
  }

  # variable — word boundary anywhere in code
  for (var in chk[["variable"]] %||% character(0L)) {
    key  <- paste0("variable.", var)
    pat  <- sprintf("\\b%s\\b", var)
    pass <- grepl(pat, target, perl = TRUE)
    results[[key]] <- .make_check(
      key, pass, var, target,
      if (pass) sprintf("Variable '%s' found in code.", var)
      else      sprintf("Variable '%s' not found in code.", var)
    )
  }

  # n_commands — count lines in a section
  nc_spec <- chk[["n_commands"]]
  if (!is.null(nc_spec)) {
    sec_type  <- nc_spec[["type"]]   %||% "estimation"
    expected  <- nc_spec[["expected"]]
    tolerance <- nc_spec[["tolerance"]] %||% 0L
    observed  <- length(sections[[sec_type]] %||% character(0L))
    pass      <- !is.null(expected) && abs(observed - expected) <= tolerance
    key       <- paste0("n_commands.", sec_type)
    results[[key]] <- .make_check(
      key, pass, expected, observed,
      if (is.null(expected))
        sprintf("n_commands: section '%s' has %d lines", sec_type, observed)
      else if (pass)
        sprintf("n_commands (%s): %d (expected %d ± %d)",
                sec_type, observed, expected, tolerance)
      else
        sprintf("n_commands (%s): expected %d ± %d, found %d",
                sec_type, expected, tolerance, observed)
    )
  }

  .make_result(name, "stata", "character", results)
}
