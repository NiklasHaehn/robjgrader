# Validate a student text answer using an LLM

Sends a student's written answer to an OpenAI-compatible LLM endpoint
for automated grading. Supports two modes:

## Usage

``` r
validate_text(
  text,
  section = NULL,
  n_sections = NULL,
  prompt = NULL,
  question = NULL,
  rubric = NULL,
  reference = NULL,
  name = "text",
  feedback = FALSE,
  model = "llama-3.3-70b-versatile",
  base_url = "https://api.groq.com/openai/v1",
  api_key = Sys.getenv("GROQ_API_KEY"),
  max_retry = 3L
)
```

## Arguments

- text:

  Character. The student's answer, typically from
  [`read_student_text()`](https://niklashaehn.github.io/robjgrader/reference/read_student_text.md).

- section:

  Character or integer. When the submission contains multiple questions,
  identifies which section to grade. A character value is matched
  against section headings (case-insensitive, partial match allowed); an
  integer selects by position. When `NULL` (default), the full text is
  graded without splitting.

- n_sections:

  Integer or `NULL`. Total number of questions in the submission. Used
  as a hint by
  [`split_student_text()`](https://niklashaehn.github.io/robjgrader/reference/split_student_text.md)
  when no heading patterns are found (limits blank-line splitting to the
  first `n_sections` chunks).

- prompt:

  Character. Full system prompt (Mode A). Mutually exclusive with
  `question` and `rubric`.

- question:

  Character. The assignment question (Mode B).

- rubric:

  Named character vector or named list. Each element is a grading
  criterion; its name is the criterion label and its value is the
  description of what a passing answer must demonstrate.

- reference:

  Character or file path. An optional model answer used as a grading
  standard. If a valid file path is supplied, the file is read via
  [`read_student_text()`](https://niklashaehn.github.io/robjgrader/reference/read_student_text.md).
  When provided, the LLM compares the student answer against the
  reference rather than grading against abstract criteria alone. Only
  used in Mode B (`question`/`rubric`); ignored in Mode A.

- name:

  Character. Label for this result, shown in console output and
  Gradescope.

- feedback:

  Logical. If `TRUE`, the LLM is asked to provide written feedback
  stored in `result$feedback` and included in the Gradescope output when
  the answer is not fully correct. The feedback is constrained by prompt
  instructions: it must be constructive and precise, written in plain
  English, at most 3 sentences and 200 words, free of greetings or
  sign-offs, and must start directly with the substantive comment. No
  reference to automated grading or language models is permitted.

- model:

  Character. Model identifier passed to the API.

- base_url:

  Character. Base URL of the OpenAI-compatible API endpoint. Defaults to
  Groq (`"https://api.groq.com/openai/v1"`).

- api_key:

  Character. API key. Defaults to the `GROQ_API_KEY` environment
  variable.

- max_retry:

  Integer. Maximum regrading attempts on invalid responses. Default
  `3L`.

## Value

A `robjgrader_result` with fields `score` (normalized 0–1, used for
partial credit in
[`run_autograder()`](https://niklashaehn.github.io/robjgrader/reference/run_autograder.md))
and, when `feedback = TRUE`, `feedback` (character or `NULL`).

## Details

- Mode A – full prompt:

  Pass a complete `prompt` that you have written yourself. The prompt
  must instruct the model to return JSON matching the schema described
  below.

- Mode B – structured prompt:

  Pass `question` and `rubric`; a complete grading prompt is built
  automatically.

The LLM is expected to return a JSON object with the following fields:

    {
      "pass":     <boolean>,
      "score":    <number 0-1>,
      "criteria": [
        { "name": "...", "pass": <boolean>, "message": "..." }
      ],
      "feedback": "..."   // only when feedback = TRUE
    }

If the response does not conform to this schema, the LLM is asked to
retry up to `max_retry` times before returning a result with
`overall = NA`.
