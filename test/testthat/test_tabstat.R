context("tabstat")

# Test data
data(mtcars)

# Single univariate analysis with Defaults
# tabstat(mtcars$mpg)
# with(mtcars, tabstat(mpg))
# Works

# library(doBy) # Replace this
# summary.stats <- summaryBy(mpg ~ factor(cyl) + factor(vs), data = mtcars, FUN = tabstat)
# summary.stats

test_that("tabstat returns the expected variable name", {
  mpg <- tabstat(mtcars$mpg)
  expect_output(mpg$variable, "mpg")
})

test_that("tabstat returns expected values with defaults", {
  mpg <- tabstat(mtcars$mpg)
  expect_equal(mpg$mean, 20.091)
  expect_null(mpg$n)
  expect_null(mpg$nmiss)
})

test_that("n works", {
  mpg <- tabstat(mtcars$mpg, stats = "n")
  expect_equal(mpg$n, 32)
})

test_that("nmiss works", {
  mpg <- tabstat(mtcars$mpg, stats = "nmiss")
  expect_equal(mpg$nmiss, 0)
})

test_that("ci works", {
  mpg <- tabstat(mtcars$mpg, stats = "ci")
  expect_equal(mpg$mean_lcl, 17.918)
  expect_equal(mpg$mean_ucl, 22.264)
})

test_that("sum works", {
  mpg <- tabstat(mtcars$mpg, stats = "sum")
  expect_equal(mpg$sum, 642.9)
})

test_that("max works", {
  mpg <- tabstat(mtcars$mpg, stats = "max")
  expect_equal(mpg$max, 33.9)
})

test_that("min works", {
  mpg <- tabstat(mtcars$mpg, stats = "min")
  expect_equal(mpg$min, 10.4)
})

test_that("range works", {
  mpg <- tabstat(mtcars$mpg, stats = "range")
  expect_equal(mpg$range, 23.5)
})

test_that("std_dev works", {
  mpg <- tabstat(mtcars$mpg, stats = "sd")
  expect_equal(mpg$std_dev, 6.027)
})

test_that("variance works", {
  mpg <- tabstat(mtcars$mpg, stats = "var")
  expect_equal(mpg$variance, 36.324)
})

test_that("coeffienct of variation works", {
  mpg <- tabstat(mtcars$mpg, stats = "cv")
  expect_equal(mpg$coef_var, 0.3)
})

test_that("sem works", {
  mpg <- tabstat(mtcars$mpg, stats = "sem")
  expect_equal(mpg$sem, 1.065)
})

test_that("skewness works", {
  mpg <- tabstat(mtcars$mpg, stats = "skew")
  expect_equal(mpg$skewness, 0.64)
})

test_that("kurtosis works", {
  mpg <- tabstat(mtcars$mpg, stats = "kurt")
  expect_equal(mpg$kurtosis, 2.799)
})

test_that("p1 works", {
  mpg <- tabstat(mtcars$mpg, stats = "p1")
  expect_equal(mpg$p1, 10.4)
})

test_that("p5 works", {
  mpg <- tabstat(mtcars$mpg, stats = "p5")
  expect_equal(mpg$p5, 11.995)
})

test_that("p10 works", {
  mpg <- tabstat(mtcars$mpg, stats = "p10")
  expect_equal(mpg$p10, 14.34)
})

test_that("p25 works", {
  mpg <- tabstat(mtcars$mpg, stats = "p25")
  expect_equal(mpg$p25, 15.425)
})

test_that("median works", {
  mpg <- tabstat(mtcars$mpg, stats = "median")
  expect_equal(mpg$median, 19.2)
})

test_that("p50 works", {
  mpg <- tabstat(mtcars$mpg, stats = "p50")
  expect_equal(mpg$median, 19.2)
})

test_that("p75 works", {
  mpg <- tabstat(mtcars$mpg, stats = "p75")
  expect_equal(mpg$p75, 22.8)
})

test_that("p90 works", {
  mpg <- tabstat(mtcars$mpg, stats = "p90")
  expect_equal(mpg$p90, 30.09)
})

test_that("p95 works", {
  mpg <- tabstat(mtcars$mpg, stats = "p95")
  expect_equal(mpg$p95, 31.3)
})

test_that("p99 works", {
  mpg <- tabstat(mtcars$mpg, stats = "p99")
  expect_equal(mpg$p99, 33.435)
})

test_that("iqr works", {
  mpg <- tabstat(mtcars$mpg, stats = "iqr")
  expect_equal(mpg$iqr, 7.375)
})
# Do one to test and make sure that they all work.













