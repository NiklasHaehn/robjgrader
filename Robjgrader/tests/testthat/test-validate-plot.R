library(testthat)
skip_if_not_installed("ggplot2")
library(ggplot2)

# ---- helpers -----------------------------------------------------------------

plot_recs <- function(obj, name = "p1") {
  make_records(list(obj = obj, name = name))
}

ref_p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

# ---- group checks: PASS -------------------------------------------------------

test_that("plot: full group comparison PASS (identical plot)", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", reference = ref_p)
  expect_true(res$overall)
  expect_true(res$checks$aesthetics$pass)
  expect_true(res$checks$geoms$pass)
  expect_true(res$checks$facets$pass)
})

test_that("plot: layer aes treated same as global aes for aesthetics group", {
  # Student puts aes() in geom_point() instead of ggplot() -- should still PASS
  p    <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg))
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", reference = ref_p)
  expect_true(res$checks$aesthetics$pass)
})

test_that("plot: facets PASS when neither ref nor student has facets", {
  recs <- plot_recs(ref_p)
  res  <- validate(recs, name = "p1", reference = ref_p)
  expect_true(res$checks$facets$pass)
})

test_that("plot: facets PASS with matching facet_wrap", {
  ref_f <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~cyl)
  p     <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~cyl)
  recs  <- plot_recs(p)
  res   <- validate(recs, name = "p1", reference = ref_f)
  expect_true(res$checks$facets$pass)
})

# ---- group checks: FAIL -------------------------------------------------------

test_that("plot: aesthetics FAIL on different x mapping", {
  p    <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", reference = ref_p)
  expect_false(res$checks$aesthetics$pass)
})

test_that("plot: aesthetics FAIL on extra mapping", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) + geom_point()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", reference = ref_p)
  expect_false(res$checks$aesthetics$pass)
})

test_that("plot: geoms FAIL on wrong geom type", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_line()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", reference = ref_p)
  expect_false(res$checks$geoms$pass)
})

test_that("plot: geoms FAIL on extra geom layer", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + geom_smooth()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", reference = ref_p)
  expect_false(res$checks$geoms$pass)
})

test_that("plot: facets FAIL when student has facets but ref does not", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~cyl)
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", reference = ref_p)
  expect_false(res$checks$facets$pass)
})

test_that("plot: facets FAIL when ref has facets but student does not", {
  ref_f <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~cyl)
  recs  <- plot_recs(ref_p)
  res   <- validate(recs, name = "p1", reference = ref_f)
  expect_false(res$checks$facets$pass)
})

test_that("plot: facets FAIL on wrong facet variable", {
  ref_f <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~cyl)
  p     <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~am)
  recs  <- plot_recs(p)
  res   <- validate(recs, name = "p1", reference = ref_f)
  expect_false(res$checks$facets$pass)
})

# ---- exclude -----------------------------------------------------------------

test_that("plot: exclude = 'aesthetics' removes aesthetics check", {
  p    <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", reference = ref_p, exclude = "aesthetics")
  expect_null(res$checks$aesthetics)
  expect_true(res$checks$geoms$pass)
})

test_that("plot: exclude multiple groups", {
  p    <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_line()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", reference = ref_p,
                   exclude = c("aesthetics", "geoms"))
  expect_null(res$checks$aesthetics)
  expect_null(res$checks$geoms)
  expect_false(is.null(res$checks$facets))
})

# ---- individual checks -------------------------------------------------------

test_that("plot: aes_x and aes_y individual checks PASS/FAIL", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  recs <- plot_recs(p)

  expect_true(validate(recs, name = "p1", checks = list(aes_x = "wt"))$checks$aes_x$pass)
  expect_true(validate(recs, name = "p1", checks = list(aes_y = "mpg"))$checks$aes_y$pass)
  expect_false(validate(recs, name = "p1", checks = list(aes_x = "hp"))$checks$aes_x$pass)
})

test_that("plot: aes_x from layer mapping PASS", {
  p    <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg))
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(aes_x = "wt", aes_y = "mpg"))
  expect_true(res$checks$aes_x$pass)
  expect_true(res$checks$aes_y$pass)
})

test_that("plot: aes_color / aes_colour aliasing", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) + geom_point()
  recs <- plot_recs(p)

  res_color  <- validate(recs, name = "p1", checks = list(aes_color  = "factor(cyl)"))
  res_colour <- validate(recs, name = "p1", checks = list(aes_colour = "factor(cyl)"))
  expect_true(res_color$checks$aes_color$pass)
  expect_true(res_colour$checks$aes_colour$pass)
})

test_that("plot: aes check FAIL when aesthetic not mapped", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(aes_color = "factor(cyl)"))
  expect_false(res$checks$aes_color$pass)
  expect_match(res$checks$aes_color$message, "not mapped|not found")
})

test_that("plot: geom individual check (short form and class form)", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  recs <- plot_recs(p)

  expect_true(validate(recs, name = "p1", checks = list(geom = "point"))$checks$geom$pass)
  expect_true(validate(recs, name = "p1", checks = list(geom = "GeomPoint"))$checks$geom$pass)
  expect_false(validate(recs, name = "p1", checks = list(geom = "line"))$checks$geom$pass)
})

test_that("plot: geom check allows multiple required geoms", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + geom_smooth()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(geom = c("point", "smooth")))
  expect_true(res$checks$geom$pass)
})

test_that("plot: facet_var individual check PASS/FAIL", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~cyl)
  recs <- plot_recs(p)

  expect_true(validate(recs, name = "p1", checks = list(facet_var = "cyl"))$checks$facet_var$pass)
  expect_false(validate(recs, name = "p1", checks = list(facet_var = "am"))$checks$facet_var$pass)
})

test_that("plot: facet_var check works for facet_grid", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_grid(cyl ~ am)
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(facet_var = c("cyl", "am")))
  expect_true(res$checks$facet_var$pass)
})

# ---- individual checks suppress default groups --------------------------------

test_that("plot: individual checks suppress default groups", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(aes_x = "wt"))
  expect_null(res$checks$aesthetics)
  expect_null(res$checks$geoms)
  expect_false(is.null(res$checks$aes_x))
})

# ---- match-based lookup -------------------------------------------------------

test_that("plot: match by aes_x selects correct plot", {
  p1   <- ggplot(mtcars, aes(x = wt,  y = mpg)) + geom_point()
  p2   <- ggplot(mtcars, aes(x = hp,  y = mpg)) + geom_point()
  recs <- make_records(list(obj = p1, name = NULL),
                       list(obj = p2, name = NULL))
  res  <- validate(recs, match = list(type = "ggplot", aes_x = "wt"),
                   checks = list(aes_x = "wt"))
  expect_true(res$checks$aes_x$pass)
})

test_that("plot: match with positional fallback emits warning", {
  p1   <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  p2   <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_line()
  recs <- make_records(list(obj = p1, name = NULL),
                       list(obj = p2, name = NULL))
  # Both have aes_x = "wt"; 2 candidates remain -> warning
  expect_warning(
    validate(recs, match = list(type = "ggplot", aes_x = "wt"),
             checks = list(geom = "line")),
    "candidates"
  )
})


# ---- facet_type individual check ---------------------------------------------

test_that("plot: facet_type 'wrap' PASS", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~cyl)
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(facet_type = "wrap"))
  expect_true(res$checks$facet_type$pass)
})

test_that("plot: facet_type 'grid' PASS", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_grid(cyl ~ am)
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(facet_type = "grid"))
  expect_true(res$checks$facet_type$pass)
})

test_that("plot: facet_type FAIL when wrong type", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~cyl)
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(facet_type = "grid"))
  expect_false(res$checks$facet_type$pass)
})

test_that("plot: facet_type FAIL when no facets but 'wrap' expected", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(facet_type = "wrap"))
  expect_false(res$checks$facet_type$pass)
})

# ---- stat individual check ---------------------------------------------------

test_that("plot: stat 'identity' PASS (geom_point uses StatIdentity)", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(stat = "identity"))
  expect_true(res$checks$stat$pass)
})

test_that("plot: stat 'smooth' PASS (geom_smooth uses StatSmooth)", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_smooth()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(stat = "smooth"))
  expect_true(res$checks$stat$pass)
})

test_that("plot: stat FAIL when stat not present", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(stat = "smooth"))
  expect_false(res$checks$stat$pass)
  expect_match(res$checks$stat$message, "smooth")
})

test_that("plot: stat check accepts full class name (StatSmooth)", {
  p    <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_smooth()
  recs <- plot_recs(p)
  res  <- validate(recs, name = "p1", checks = list(stat = "StatSmooth"))
  expect_true(res$checks$stat$pass)
})
