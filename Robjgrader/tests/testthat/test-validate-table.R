library(testthat)

# ---- helpers -----------------------------------------------------------------

table_recs <- function(obj, name = "tbl1") {
  make_records(list(obj = obj, name = name))
}

# ---- gt backend --------------------------------------------------------------

test_that("table (gt): canonical extraction returns expected structure", {
  skip_if_not_installed("gt")
  library(gt)

  tbl  <- gt(head(mtcars[, c("mpg", "cyl", "hp")], 5))
  info <- .table_canonical(tbl)

  expect_type(info$nrow,       "integer")
  expect_type(info$ncol,       "integer")
  expect_type(info$row_labels, "character")
  expect_type(info$col_labels, "character")
  expect_equal(info$nrow, 5L)
})

test_that("table (gt): nrow individual check PASS/FAIL", {
  skip_if_not_installed("gt")
  library(gt)

  tbl  <- gt(head(mtcars[, c("mpg", "cyl", "hp")], 5))
  recs <- table_recs(tbl)

  expect_true(validate(recs, name = "tbl1", checks = list(nrow = 5L))$checks$nrow$pass)
  expect_false(validate(recs, name = "tbl1", checks = list(nrow = 3L))$checks$nrow$pass)
})

test_that("table (gt): terms individual check PASS/FAIL", {
  skip_if_not_installed("gt")
  library(gt)

  df   <- data.frame(term = c("wt", "hp", "R2"), value = c("0.5", "0.3", "0.8"))
  tbl  <- gt(df)
  recs <- table_recs(tbl)

  expect_true(
    validate(recs, name = "tbl1",
             checks = list(terms = c("wt", "hp")))$checks$terms$pass
  )
  expect_false(
    validate(recs, name = "tbl1",
             checks = list(terms = "missing_term"))$checks$terms$pass
  )
})

test_that("table (gt): terms_absent check PASS/FAIL", {
  skip_if_not_installed("gt")
  library(gt)

  df   <- data.frame(term = c("wt", "hp"), value = c("0.5", "0.3"))
  tbl  <- gt(df)
  recs <- table_recs(tbl)

  expect_true(
    validate(recs, name = "tbl1",
             checks = list(terms_absent = "(Intercept)"))$checks$terms_absent$pass
  )
  expect_false(
    validate(recs, name = "tbl1",
             checks = list(terms_absent = "wt"))$checks$terms_absent$pass
  )
})

test_that("table (gt): full group comparison PASS (identical tables)", {
  skip_if_not_installed("gt")
  library(gt)

  df   <- data.frame(term  = c("wt", "hp", "Num.Obs."),
                     m1    = c("-0.37", "0.12", "32"),
                     stringsAsFactors = FALSE)
  ref  <- gt(df)
  tbl  <- gt(df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", reference = ref)

  expect_true(res$overall)
  expect_true(res$checks$dimensions$pass)
  expect_true(res$checks$terms$pass)
  expect_true(res$checks$models$pass)
})

test_that("table (gt): dimensions FAIL when nrow differs", {
  skip_if_not_installed("gt")
  library(gt)

  ref_df <- data.frame(term = c("wt", "hp", "Num.Obs."),
                       m1   = c("-0.37", "0.12", "32"),
                       stringsAsFactors = FALSE)
  obj_df <- head(ref_df, 2)
  ref  <- gt(ref_df)
  tbl  <- gt(obj_df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", reference = ref)

  expect_false(res$checks$dimensions$pass)
})

test_that("table (gt): terms FAIL when row label missing", {
  skip_if_not_installed("gt")
  library(gt)

  ref_df <- data.frame(term = c("wt", "hp", "Num.Obs."),
                       m1   = c("-0.37", "0.12", "32"),
                       stringsAsFactors = FALSE)
  obj_df <- data.frame(term = c("wt", "Num.Obs."),
                       m1   = c("-0.37", "32"),
                       stringsAsFactors = FALSE)
  ref  <- gt(ref_df)
  tbl  <- gt(obj_df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", reference = ref)

  expect_false(res$checks$terms$pass)
  expect_match(res$checks$terms$message, "hp")
})

test_that("table (gt): exclude removes group from check", {
  skip_if_not_installed("gt")
  library(gt)

  ref_df <- data.frame(term = c("wt", "hp"), m1 = c("0.5", "0.3"),
                       stringsAsFactors = FALSE)
  obj_df <- data.frame(term = c("wt"),       m1 = c("0.5"),
                       stringsAsFactors = FALSE)
  ref  <- gt(ref_df)
  tbl  <- gt(obj_df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", reference = ref, exclude = "terms")

  expect_null(res$checks$terms)
  expect_false(is.null(res$checks$dimensions))
})

# ---- flextable backend -------------------------------------------------------

test_that("table (flextable): canonical extraction returns expected structure", {
  skip_if_not_installed("flextable")
  library(flextable)

  df   <- data.frame(term = c("wt", "hp"), value = c("0.5", "0.3"),
                     stringsAsFactors = FALSE)
  tbl  <- flextable(df)
  info <- .table_canonical(tbl)

  expect_type(info$nrow,       "integer")
  expect_type(info$ncol,       "integer")
  expect_type(info$row_labels, "character")
  expect_equal(info$nrow, 2L)
  expect_true("wt" %in% info$row_labels)
})

test_that("table (flextable): nrow individual check PASS", {
  skip_if_not_installed("flextable")
  library(flextable)

  df   <- data.frame(term = c("wt", "hp"), value = c("0.5", "0.3"),
                     stringsAsFactors = FALSE)
  tbl  <- flextable(df)
  recs <- table_recs(tbl)

  expect_true(validate(recs, name = "tbl1", checks = list(nrow = 2L))$checks$nrow$pass)
})

test_that("table (flextable): terms check PASS", {
  skip_if_not_installed("flextable")
  library(flextable)

  df   <- data.frame(term = c("wt", "hp", "Num.Obs."), value = c("0.5", "0.3", "32"),
                     stringsAsFactors = FALSE)
  tbl  <- flextable(df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", checks = list(terms = c("wt", "hp")))

  expect_true(res$checks$terms$pass)
})

# ---- error handling ----------------------------------------------------------

test_that("validate_table() requires reference for group checks", {
  skip_if_not_installed("gt")
  library(gt)

  tbl  <- gt(data.frame(term = "wt", m1 = "0.5", stringsAsFactors = FALSE))
  recs <- table_recs(tbl)

  expect_error(
    validate(recs, name = "tbl1", checks = "dimensions"),
    "'reference' is required"
  )
})


# ---- gof individual check ----------------------------------------------------

test_that("table (gt): gof individual check PASS when GOF rows present", {
  skip_if_not_installed("gt")
  library(gt)

  df   <- data.frame(term  = c("wt", "hp", "Num.Obs.", "R2"),
                     m1    = c("-0.37", "0.12", "32", "0.83"),
                     stringsAsFactors = FALSE)
  tbl  <- gt(df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", checks = list(gof = c("Num.Obs.", "R2")))
  expect_true(res$checks$gof$pass)
})

test_that("table (gt): gof individual check FAIL when GOF row missing", {
  skip_if_not_installed("gt")
  library(gt)

  df   <- data.frame(term = c("wt", "hp"), m1 = c("-0.37", "0.12"),
                     stringsAsFactors = FALSE)
  tbl  <- gt(df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", checks = list(gof = "Num.Obs."))
  expect_false(res$checks$gof$pass)
  expect_match(res$checks$gof$message, "Num\\.Obs\\.")
})

# ---- gof group check ---------------------------------------------------------

test_that("table (gt): gof group PASS (identical GOF rows in ref and student)", {
  skip_if_not_installed("gt")
  library(gt)

  df   <- data.frame(term = c("wt", "hp", "Num.Obs.", "R2"),
                     m1   = c("-0.37", "0.12", "32", "0.83"),
                     stringsAsFactors = FALSE)
  ref  <- gt(df); tbl <- gt(df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", reference = ref, checks = "gof")
  expect_true(res$checks$gof$pass)
})

test_that("table (gt): gof group FAIL when student missing a GOF row", {
  skip_if_not_installed("gt")
  library(gt)

  ref_df <- data.frame(term = c("wt", "Num.Obs.", "R2"),
                       m1   = c("-0.37", "32", "0.83"), stringsAsFactors = FALSE)
  obj_df <- data.frame(term = c("wt", "Num.Obs."),
                       m1   = c("-0.37", "32"), stringsAsFactors = FALSE)
  ref  <- gt(ref_df); tbl <- gt(obj_df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", reference = ref, checks = "gof")
  expect_false(res$checks$gof$pass)
  expect_match(res$checks$gof$message, "R2")
})

# ---- gof group in full default comparison ------------------------------------

test_that("table (gt): full comparison still PASS for identical table (gof + inference in defaults)", {
  skip_if_not_installed("gt")
  library(gt)

  df   <- data.frame(term  = c("wt", "hp", "Num.Obs."),
                     m1    = c("-0.37", "0.12", "32"),
                     stringsAsFactors = FALSE)
  ref  <- gt(df); tbl <- gt(df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", reference = ref)
  expect_true(res$overall)
  expect_true(res$checks$gof$pass)
})

# ---- inference individual check ----------------------------------------------

test_that("table (gt): inference 'se' detected from (value) rows", {
  skip_if_not_installed("gt")
  library(gt)

  df   <- data.frame(term = c("wt", "", "Num.Obs."),
                     m1   = c("-0.37", "(0.05)", "32"),
                     stringsAsFactors = FALSE)
  tbl  <- gt(df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", checks = list(inference = "se"))
  expect_true(res$checks$inference$pass)
})

test_that("table (gt): inference 'ci' detected from [value] rows", {
  skip_if_not_installed("gt")
  library(gt)

  df   <- data.frame(term = c("wt", "", "Num.Obs."),
                     m1   = c("-0.37", "[−0.5, −0.2]", "32"),
                     stringsAsFactors = FALSE)
  tbl  <- gt(df)
  recs <- table_recs(tbl)
  res  <- validate(recs, name = "tbl1", checks = list(inference = "ci"))
  expect_true(res$checks$inference$pass)
})

test_that("table: gof_labels detects standard GOF stat names", {
  skip_if_not_installed("gt")
  library(gt)

  df   <- data.frame(term = c("wt", "hp", "Num.Obs.", "R2", "AIC"),
                     m1   = c("-0.37", "0.12", "32", "0.83", "124"),
                     stringsAsFactors = FALSE)
  tbl  <- gt(df)
  info <- .table_canonical(tbl)
  expect_true("Num.Obs." %in% info$gof_labels)
  expect_true("R2"       %in% info$gof_labels)
  expect_true("AIC"      %in% info$gof_labels)
  expect_false("wt"      %in% info$gof_labels)
  expect_false("hp"      %in% info$gof_labels)
})
