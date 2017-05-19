context("stats")

# Test data
data(mtcars)

# Single univariate analysis
# stats(mtcars$mpg)
# with(mtcars, stats(mpg))
# Works

# library(doBy) # Replace this
# summary.stats <- summaryBy(mpg ~ factor(cyl) + factor(vs), data = mtcars, FUN = stats)
# summary.stats

test_that("stats returns expected values for single univariate analysis", {

  mpg <- stats(mtcars$mpg) # Save results

  expect_equal(mpg$Non_Missing, 32)
  expect_equal(mpg$Missing, 0)
  expect_equal(mpg$Mean, 20.091)
  expect_equal(mpg$Mean_LCL, 17.918)
  expect_equal(mpg$Mean_UCL, 22.264)
  expect_equal(mpg$SEM, 1.065)
  expect_equal(mpg$Median, 19.200)
  expect_equal(mpg$Variance, 36.324)
  expect_equal(mpg$SD, 6.027)
  expect_equal(mpg$Min, 10.400)
  expect_equal(mpg$Max, 33.900)
})