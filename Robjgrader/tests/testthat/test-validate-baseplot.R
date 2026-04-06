library(testthat)

# ---- helpers -----------------------------------------------------------------

bp_recs <- function(obj, name = "p1") {
  make_records(list(obj = obj, name = name))
}

make_bp <- function(
  plot_type = "scatter",
  x_expr    = "mtcars$wt",
  y_expr    = "mtcars$mpg",
  aes       = list()
) {
  structure(
    list(plot_type = plot_type, x_expr = x_expr, y_expr = y_expr, aes = aes),
    class = "robjgrader_baseplot"
  )
}

ref_bp <- make_bp(
  plot_type = "scatter",
  x_expr    = "mtcars$wt",
  y_expr    = "mtcars$mpg",
  aes       = list(col = "red", pch = 16L)
)

# ==============================================================================
# Individual checks
# ==============================================================================

test_that("baseplot: plot_type check PASS", {
  recs <- bp_recs(make_bp(plot_type = "scatter"))
  res  <- validate(recs, name = "p1", checks = list(plot_type = "scatter"))
  expect_true(res$overall)
  expect_true(res$checks$plot_type$pass)
})

test_that("baseplot: plot_type check FAIL", {
  recs <- bp_recs(make_bp(plot_type = "line"))
  res  <- validate(recs, name = "p1", checks = list(plot_type = "scatter"))
  expect_false(res$overall)
  expect_match(res$checks$plot_type$message, "scatter")
  expect_match(res$checks$plot_type$message, "line")
})

test_that("baseplot: aes_x check PASS", {
  recs <- bp_recs(make_bp(x_expr = "mtcars$wt"))
  res  <- validate(recs, name = "p1", checks = list(aes_x = "mtcars$wt"))
  expect_true(res$checks$aes_x$pass)
})

test_that("baseplot: aes_x check FAIL on wrong expression", {
  recs <- bp_recs(make_bp(x_expr = "mtcars$hp"))
  res  <- validate(recs, name = "p1", checks = list(aes_x = "mtcars$wt"))
  expect_false(res$checks$aes_x$pass)
})

test_that("baseplot: aes_x check FAIL when no x_expr", {
  recs <- bp_recs(make_bp(x_expr = NULL))
  res  <- validate(recs, name = "p1", checks = list(aes_x = "mtcars$wt"))
  expect_false(res$checks$aes_x$pass)
  expect_match(res$checks$aes_x$message, "no x argument")
})

test_that("baseplot: aes_y check PASS", {
  recs <- bp_recs(make_bp(y_expr = "mtcars$mpg"))
  res  <- validate(recs, name = "p1", checks = list(aes_y = "mtcars$mpg"))
  expect_true(res$checks$aes_y$pass)
})

test_that("baseplot: col check PASS", {
  recs <- bp_recs(make_bp(aes = list(col = "red")))
  res  <- validate(recs, name = "p1", checks = list(col = "red"))
  expect_true(res$checks$col$pass)
})

test_that("baseplot: col check FAIL", {
  recs <- bp_recs(make_bp(aes = list(col = "blue")))
  res  <- validate(recs, name = "p1", checks = list(col = "red"))
  expect_false(res$checks$col$pass)
  expect_match(res$checks$col$message, "red")
})

test_that("baseplot: color alias resolves to col", {
  recs <- bp_recs(make_bp(aes = list(col = "green")))
  res  <- validate(recs, name = "p1", checks = list(color = "green"))
  expect_true(res$checks$color$pass)
})

test_that("baseplot: col check FAIL when unset", {
  recs <- bp_recs(make_bp(aes = list()))
  res  <- validate(recs, name = "p1", checks = list(col = "red"))
  expect_false(res$checks$col$pass)
  expect_match(res$checks$col$message, "not set")
})

test_that("baseplot: pch check PASS", {
  recs <- bp_recs(make_bp(aes = list(pch = 16L)))
  res  <- validate(recs, name = "p1", checks = list(pch = 16L))
  expect_true(res$checks$pch$pass)
})

test_that("baseplot: xlab / ylab checks PASS", {
  recs <- bp_recs(make_bp(aes = list(xlab = "Weight", ylab = "MPG")))
  res  <- validate(recs, name = "p1",
                   checks = list(xlab = "Weight", ylab = "MPG"))
  expect_true(res$checks$xlab$pass)
  expect_true(res$checks$ylab$pass)
})

test_that("baseplot: main check PASS", {
  recs <- bp_recs(make_bp(aes = list(main = "My Plot")))
  res  <- validate(recs, name = "p1", checks = list(main = "My Plot"))
  expect_true(res$checks$main$pass)
})

test_that("baseplot: multiple checks, mixed pass/fail", {
  recs <- bp_recs(make_bp(plot_type = "scatter", x_expr = "mtcars$wt",
                           aes = list(col = "blue")))
  res  <- validate(recs, name = "p1",
                   checks = list(plot_type = "scatter", col = "red"))
  expect_false(res$overall)
  expect_true(res$checks$plot_type$pass)
  expect_false(res$checks$col$pass)
})

# ==============================================================================
# Group checks: aesthetics
# ==============================================================================

test_that("baseplot: aesthetics group PASS when aes match reference", {
  bp   <- make_bp(aes = list(col = "red", pch = 16L))
  recs <- bp_recs(bp)
  res  <- validate(recs, name = "p1", reference = ref_bp)
  expect_true(res$checks$aesthetics$pass)
})

test_that("baseplot: aesthetics group FAIL on colour mismatch", {
  bp   <- make_bp(aes = list(col = "blue", pch = 16L))
  recs <- bp_recs(bp)
  res  <- validate(recs, name = "p1", reference = ref_bp)
  expect_false(res$checks$aesthetics$pass)
  expect_match(res$checks$aesthetics$message, "col")
})

test_that("baseplot: aesthetics group FAIL on missing aesthetic", {
  bp   <- make_bp(aes = list(col = "red"))
  recs <- bp_recs(bp)
  res  <- validate(recs, name = "p1", reference = ref_bp)
  expect_false(res$checks$aesthetics$pass)
})

test_that("baseplot: aesthetics group PASS when reference has no aes keys", {
  ref_empty <- make_bp(aes = list())
  bp        <- make_bp(aes = list(col = "red"))
  recs      <- bp_recs(bp)
  res       <- validate(recs, name = "p1", reference = ref_empty)
  expect_true(res$checks$aesthetics$pass)
})

# ==============================================================================
# Group checks: variables
# ==============================================================================

test_that("baseplot: variables group PASS when x/y match reference", {
  bp   <- make_bp(x_expr = "mtcars$wt", y_expr = "mtcars$mpg")
  recs <- bp_recs(bp)
  res  <- validate(recs, name = "p1", reference = ref_bp)
  expect_true(res$checks$variables$pass)
})

test_that("baseplot: variables group FAIL on wrong x", {
  bp   <- make_bp(x_expr = "mtcars$hp", y_expr = "mtcars$mpg")
  recs <- bp_recs(bp)
  res  <- validate(recs, name = "p1", reference = ref_bp)
  expect_false(res$checks$variables$pass)
  expect_match(res$checks$variables$message, "mtcars\\$wt")
})

test_that("baseplot: variables group FAIL on wrong y", {
  bp   <- make_bp(x_expr = "mtcars$wt", y_expr = "mtcars$cyl")
  recs <- bp_recs(bp)
  res  <- validate(recs, name = "p1", reference = ref_bp)
  expect_false(res$checks$variables$pass)
  expect_match(res$checks$variables$message, "mtcars\\$mpg")
})

test_that("baseplot: variables group PASS when reference has no y (histogram)", {
  ref_hist <- make_bp(plot_type = "histogram", x_expr = "mtcars$wt", y_expr = NULL)
  bp       <- make_bp(plot_type = "histogram", x_expr = "mtcars$wt", y_expr = NULL)
  recs     <- bp_recs(bp)
  res      <- validate(recs, name = "p1", reference = ref_hist)
  expect_true(res$checks$variables$pass)
})

# ==============================================================================
# Matching via match = list(type = "baseplot", ...)
# ==============================================================================

test_that("baseplot: match by type finds baseplot record", {
  bp   <- make_bp(plot_type = "scatter")
  recs <- bp_recs(bp)
  res  <- validate(recs, match = list(type = "baseplot"),
                   checks = list(plot_type = "scatter"))
  expect_true(res$overall)
})

test_that("baseplot: match by plot_type discriminates", {
  bp1  <- make_bp(plot_type = "scatter")
  bp2  <- make_bp(plot_type = "histogram", x_expr = "x", y_expr = NULL)
  recs <- make_records(list(obj = bp1, name = "p1"),
                       list(obj = bp2, name = "p2"))
  res  <- validate(recs, match = list(type = "baseplot", plot_type = "histogram"),
                   checks = list(plot_type = "histogram"))
  expect_equal(res$object_name, "p2")
})

test_that("baseplot: exclusive matching prevents reuse", {
  bp   <- make_bp(plot_type = "scatter")
  recs <- make_records(list(obj = bp, name = "p1"), exclusive = TRUE)
  res1 <- validate(recs, match = list(type = "baseplot"),
                   checks = list(plot_type = "scatter"))
  res2 <- validate(recs, match = list(type = "baseplot"),
                   checks = list(plot_type = "scatter"))
  expect_true(res1$overall)
  expect_false(res2$overall)
})

# ==============================================================================
# Matching via reference (robjgrader_baseplot)
# ==============================================================================

test_that("baseplot: lookup by reference works", {
  bp   <- make_bp(plot_type = "scatter", x_expr = "mtcars$wt",
                   y_expr = "mtcars$mpg", aes = list(col = "red", pch = 16L))
  recs <- bp_recs(bp)
  res  <- validate(recs, reference = ref_bp)
  # aesthetics + variables groups both pass (ref has col, pch, x, y)
  expect_true(res$checks$aesthetics$pass)
  expect_true(res$checks$variables$pass)
})

# ==============================================================================
# .make_baseplot_descriptor() — expression parsing
# ==============================================================================

test_that(".make_baseplot_descriptor: scatter from plot()", {
  expr <- quote(plot(mtcars$wt, mtcars$mpg, col = "red", pch = 16))
  bp   <- .make_baseplot_descriptor(expr, globalenv())
  expect_equal(bp$plot_type, "scatter")
  expect_equal(bp$x_expr, "mtcars$wt")
  expect_equal(bp$y_expr, "mtcars$mpg")
  expect_equal(bp$aes$col, "red")
  expect_equal(bp$aes$pch, 16)
})

test_that(".make_baseplot_descriptor: line type from plot(type='l')", {
  expr <- quote(plot(x, y, type = "l"))
  bp   <- .make_baseplot_descriptor(expr, list2env(list(x = 1:5, y = 1:5)))
  expect_equal(bp$plot_type, "line")
})

test_that(".make_baseplot_descriptor: histogram from hist()", {
  expr <- quote(hist(mtcars$wt, breaks = 10))
  bp   <- .make_baseplot_descriptor(expr, globalenv())
  expect_equal(bp$plot_type, "histogram")
  expect_equal(bp$x_expr, "mtcars$wt")
  expect_null(bp$y_expr)
})

test_that(".make_baseplot_descriptor: density via plot(density(x))", {
  expr <- quote(plot(density(mtcars$wt)))
  bp   <- .make_baseplot_descriptor(expr, globalenv())
  expect_equal(bp$plot_type, "density")
})

test_that(".make_baseplot_descriptor: barplot captures x_expr", {
  expr <- quote(barplot(table(mtcars$cyl), col = "steelblue"))
  bp   <- .make_baseplot_descriptor(expr, globalenv())
  expect_equal(bp$plot_type, "bar")
  expect_equal(bp$aes$col, "steelblue")
})

test_that(".is_base_plot_call: TRUE for known functions", {
  expect_true(.is_base_plot_call(quote(plot(x, y))))
  expect_true(.is_base_plot_call(quote(hist(x))))
  expect_true(.is_base_plot_call(quote(barplot(h))))
  expect_true(.is_base_plot_call(quote(boxplot(x ~ g))))
})

test_that(".is_base_plot_call: FALSE for non-plot calls", {
  expect_false(.is_base_plot_call(quote(lm(y ~ x))))
  expect_false(.is_base_plot_call(quote(ggplot(data))))
  expect_false(.is_base_plot_call(as.name("x")))
})
