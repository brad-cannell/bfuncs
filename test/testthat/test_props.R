context("props")

# Test data
data(mtcars)

# Test - single univariate
# props(mtcars$cyl, citype = "normal", round = 3)
# with(mtcars, props(cyl))
# Works

# Test - single bivariate
with(mtcars, by(cyl, am, props))
# Numbers Works
# Variable name doesn't work

# Test - multiple univariate
x <- mtcars[c("cyl", "am")] # Only want cyl and am
lapply(x, props)
do.call(rbind, lapply(x, props))
# Numbers Works
# Variable name doesn't work



test_that("props returns expected values for single univariate analysis", {

  cyl <- props(mtcars$cyl, citype = "normal", digits = 3) # Save Results

  expect_output(cyl$variable[1], "cyl")
  expect_output(cyl$variable[2], "cyl")
  expect_output(cyl$variable[3], "cyl")
  expect_output(cyl$level[1], "4")
  expect_output(cyl$level[2], "6")
  expect_output(cyl$level[3], "8")
  expect_equivalent(cyl$non_miss[1], 11)
  expect_equivalent(cyl$non_miss[2], 7)
  expect_equivalent(cyl$non_miss[3], 14)
  expect_equivalent(cyl$percent[1], 34.375)
  expect_equivalent(cyl$percent[2], 21.875)
  expect_equivalent(cyl$percent[3], 43.750)
  expect_true(sum(cyl$percent) >= 99 & sum(cyl$percent) <= 101)
  expect_equivalent(cyl$lcl[1], 16.977)
  expect_equivalent(cyl$lcl[2], 6.732)
  expect_equivalent(cyl$lcl[3], 25.578)
  expect_equivalent(cyl$ucl[1], 51.773)
  expect_equivalent(cyl$ucl[2], 37.018)
  expect_equivalent(cyl$ucl[3], 61.922)
})
