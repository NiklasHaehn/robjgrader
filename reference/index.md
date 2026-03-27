# Package index

## Recording

Capture objects from R scripts or interactive sessions.

- [`record_script()`](https://niklashaehn.github.io/robjgrader/reference/record_script.md)
  : Record all objects produced by sourcing an R script
- [`record_start()`](https://niklashaehn.github.io/robjgrader/reference/record_start.md)
  : Start recording analytical objects
- [`record_stop()`](https://niklashaehn.github.io/robjgrader/reference/record_stop.md)
  : Stop recording and summarise captured objects
- [`get_records()`](https://niklashaehn.github.io/robjgrader/reference/get_records.md)
  : Retrieve recorded objects
- [`source_student_file()`](https://niklashaehn.github.io/robjgrader/reference/source_student_file.md)
  : Source a student submission and return recorded objects
- [`grab()`](https://niklashaehn.github.io/robjgrader/reference/grab.md)
  : Wrap a named global object into a robjgrader_records list

## Validation

Validate recorded objects against expectations.

- [`validate()`](https://niklashaehn.github.io/robjgrader/reference/validate.md)
  : Validate a recorded object
- [`validate_text()`](https://niklashaehn.github.io/robjgrader/reference/validate_text.md)
  : Validate a student text answer using an LLM
- [`find_student_text()`](https://niklashaehn.github.io/robjgrader/reference/find_student_text.md)
  : Find and read a student text submission automatically
- [`read_student_text()`](https://niklashaehn.github.io/robjgrader/reference/read_student_text.md)
  : Read a student text submission

## Autograder

Run test cases and produce Gradescope-compatible output.

- [`run_autograder()`](https://niklashaehn.github.io/robjgrader/reference/run_autograder.md)
  : Run autograder test cases and write Gradescope-compatible JSON
- [`ag_submission_test()`](https://niklashaehn.github.io/robjgrader/reference/ag_submission_test.md)
  : Submission test: always returns "SUCCESS"
- [`result_to_outcome()`](https://niklashaehn.github.io/robjgrader/reference/result_to_outcome.md)
  : Convert a validate() result to an autograder outcome string
