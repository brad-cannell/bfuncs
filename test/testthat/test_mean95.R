context("mean95")

# Test data
data(mtcars)

# Single univariate analysis
# mean95(mtcars$mpg)
# with(mtcars, mean95(mpg))
# Works

# Single bivariate analyses
# with(mtcars, by(mpg, am, mean95))
# Works



test_that("mean95 returns expected values for single univariate analysis", {

  mpg <- mean95(mtcars$mpg) # Save results

  expect_equal(mpg$mean, 20.091)
  expect_equal(mpg$sd, 6.027)
  expect_equal(mpg$lcl, 17.918)
  expect_equal(mpg$ucl, 22.264)
})

test_that("mean95 returns expected values for single bivariate analyses", {

  mpg <- with(mtcars, by(mpg, am, mean95)) # Save results

  expect_equal(mpg$`0`$mean, 17.147)
  expect_equal(mpg$`0`$sd, 3.834)
  expect_equal(mpg$`0`$lcl, 15.299)
  expect_equal(mpg$`0`$ucl, 18.995)
  expect_equal(mpg$`1`$mean, 24.392)
  expect_equal(mpg$`1`$sd, 6.167)
  expect_equal(mpg$`1`$lcl, 20.666)
  expect_equal(mpg$`1`$ucl, 28.119)
})