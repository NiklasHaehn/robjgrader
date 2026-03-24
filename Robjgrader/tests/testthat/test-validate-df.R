library(testthat)

ref_df <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)

# ---- helpers -----------------------------------------------------------------

df_recs <- function(obj, name = "df1") {
  make_records(list(obj = obj, name = name))
}

# ---- group checks: PASS cases ------------------------------------------------

test_that("df: full group comparison PASS (identical data frame)", {
  recs <- df_recs(ref_df)
  res  <- validate(recs, name = "df1", reference = ref_df)
  expect_true(res$overall)
  expect_true(res$checks$dimensions$pass)
  expect_true(res$checks$names$pass)
  expect_true(res$checks$types$pass)
  expect_true(res$checks$values$pass)
})

test_that("df: values check is row-order insensitive by default", {
  shuffled <- ref_df[rev(seq_len(nrow(ref_df))), ]
  rownames(shuffled) <- NULL
  recs <- df_recs(shuffled)
  res  <- validate(recs, name = "df1", reference = ref_df)
  expect_true(res$checks$values$pass)
})

# ---- group checks: FAIL cases ------------------------------------------------

test_that("df: dimensions FAIL when nrow differs", {
  recs <- df_recs(head(ref_df, 3))
  res  <- validate(recs, name = "df1", reference = ref_df)
  expect_false(res$overall)
  expect_false(res$checks$dimensions$pass)
  expect_match(res$checks$dimensions$message, "5")
  expect_match(res$checks$dimensions$message, "3")
})

test_that("df: dimensions FAIL when ncol differs", {
  recs <- df_recs(ref_df[, "x", drop = FALSE])
  res  <- validate(recs, name = "df1", reference = ref_df)
  expect_false(res$checks$dimensions$pass)
})

test_that("df: names FAIL on missing column", {
  recs <- df_recs(ref_df[, "x", drop = FALSE])
  res  <- validate(recs, name = "df1", reference = ref_df)
  expect_false(res$checks$names$pass)
  expect_match(res$checks$names$message, "y")
})

test_that("df: names FAIL on extra column", {
  extra <- cbind(ref_df, z = 1:5)
  recs  <- df_recs(extra)
  res   <- validate(recs, name = "df1", reference = ref_df)
  expect_false(res$checks$names$pass)
})

test_that("df: types FAIL when column class differs", {
  bad        <- ref_df
  bad$x      <- as.character(bad$x)
  recs       <- df_recs(bad)
  res        <- validate(recs, name = "df1", reference = ref_df)
  expect_false(res$checks$types$pass)
  expect_match(res$checks$types$message, "x")
})

test_that("df: values FAIL on differing cell content", {
  bad   <- ref_df
  bad$x <- 6:10
  recs  <- df_recs(bad)
  res   <- validate(recs, name = "df1", reference = ref_df)
  expect_false(res$checks$values$pass)
})

# ---- opt-in groups -----------------------------------------------------------

test_that("df: row_order FAIL when rows are reversed", {
  shuffled     <- ref_df[rev(seq_len(nrow(ref_df))), ]
  rownames(shuffled) <- NULL
  recs <- df_recs(shuffled)
  res  <- validate(recs, name = "df1", reference = ref_df,
                   checks = c("dimensions", "names", "types", "values", "row_order"))
  expect_false(res$checks$row_order$pass)
})

test_that("df: row_order PASS when rows match reference order", {
  recs <- df_recs(ref_df)
  res  <- validate(recs, name = "df1", reference = ref_df,
                   checks = c("values", "row_order"))
  expect_true(res$checks$row_order$pass)
})

test_that("df: col_order FAIL when columns reordered", {
  reordered <- ref_df[, rev(names(ref_df))]
  recs      <- df_recs(reordered)
  res       <- validate(recs, name = "df1", reference = ref_df,
                        checks = c("dimensions", "col_order"))
  expect_false(res$checks$col_order$pass)
})

# ---- exclude -----------------------------------------------------------------

test_that("df: exclude removes specified group from checks", {
  bad  <- head(ref_df, 3)
  recs <- df_recs(bad)
  res  <- validate(recs, name = "df1", reference = ref_df, exclude = "dimensions")
  expect_null(res$checks$dimensions)
  expect_true(res$checks$names$pass)
})

test_that("df: exclude multiple groups", {
  bad  <- head(ref_df[, "x", drop = FALSE], 3)
  recs <- df_recs(bad)
  res  <- validate(recs, name = "df1", reference = ref_df,
                   exclude = c("dimensions", "names"))
  expect_null(res$checks$dimensions)
  expect_null(res$checks$names)
})

# ---- individual checks -------------------------------------------------------

test_that("df: nrow individual check PASS/FAIL", {
  recs <- df_recs(ref_df)
  expect_true(validate(recs, name = "df1", checks = list(nrow = 5L))$checks$nrow$pass)
  expect_false(validate(recs, name = "df1", checks = list(nrow = 99L))$checks$nrow$pass)
})

test_that("df: ncol individual check PASS/FAIL", {
  recs <- df_recs(ref_df)
  expect_true(validate(recs, name = "df1", checks = list(ncol = 2L))$checks$ncol$pass)
  expect_false(validate(recs, name = "df1", checks = list(ncol = 5L))$checks$ncol$pass)
})

test_that("df: names individual check is a subset check", {
  recs <- df_recs(ref_df)
  expect_true(validate(recs, name = "df1", checks = list(names = "x"))$checks$names$pass)
  expect_true(validate(recs, name = "df1", checks = list(names = c("x", "y")))$checks$names$pass)
  expect_false(validate(recs, name = "df1", checks = list(names = c("x", "z")))$checks$names$pass)
})

test_that("df: col_types individual check PASS/FAIL", {
  recs <- df_recs(ref_df)
  expect_true(
    validate(recs, name = "df1",
             checks = list(col_types = list(x = "integer")))$checks$col_types$pass
  )
  expect_false(
    validate(recs, name = "df1",
             checks = list(col_types = list(x = "numeric")))$checks$col_types$pass
  )
})

test_that("df: col_types FAIL gracefully for missing column", {
  recs <- df_recs(ref_df)
  res  <- validate(recs, name = "df1",
                   checks = list(col_types = list(nonexistent = "integer")))
  expect_false(res$checks$col_types$pass)
})

test_that("df: values individual check on specific column", {
  recs <- df_recs(ref_df)
  expect_true(
    validate(recs, name = "df1",
             checks = list(values = list(x = 1:5)))$checks$values$pass
  )
  expect_false(
    validate(recs, name = "df1",
             checks = list(values = list(x = 6:10)))$checks$values$pass
  )
})

test_that("df: row_order individual check by sort column", {
  sorted <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
  recs   <- df_recs(sorted)
  expect_true(
    validate(recs, name = "df1", checks = list(row_order = "x"))$checks$row_order$pass
  )

  unsorted <- data.frame(x = c(3,1,4,1,5), y = letters[1:5], stringsAsFactors = FALSE)
  recs2    <- df_recs(unsorted)
  expect_false(
    validate(recs2, name = "df1", checks = list(row_order = "x"))$checks$row_order$pass
  )
})

# ---- mixed group + individual ------------------------------------------------

test_that("df: individual checks suppress default groups", {
  recs <- df_recs(ref_df)
  res  <- validate(recs, name = "df1", checks = list(nrow = 5L))
  expect_null(res$checks$dimensions)
  expect_null(res$checks$names)
  expect_null(res$checks$values)
  expect_false(is.null(res$checks$nrow))
})

# ---- lookup ------------------------------------------------------------------

test_that("validate() errors on unknown name", {
  recs <- df_recs(ref_df, name = "df1")
  expect_error(validate(recs, name = "no_such_name"), "no_such_name")
})

test_that("validate() selects last record by default (position = 'last')", {
  df_v2 <- data.frame(x = 6:10, y = letters[1:5], stringsAsFactors = FALSE)
  recs  <- make_records(list(obj = ref_df, name = "df1"),
                        list(obj = df_v2,  name = "df1"))
  res   <- validate(recs, name = "df1", checks = list(nrow = 5L))
  expect_true(res$checks$nrow$pass)
  expect_equal(res$checks$nrow$observed, 5L)
})

test_that("validate() respects position = 'first'", {
  df_v2 <- data.frame(x = 6:10, y = letters[1:5], stringsAsFactors = FALSE)
  recs  <- make_records(list(obj = ref_df, name = "df1"),
                        list(obj = df_v2,  name = "df1"))
  res   <- validate(recs, name = "df1", checks = list(nrow = 5L), position = "first")
  expect_true(res$checks$nrow$pass)
})
