library(tidyverse)
library(bfuncs)
data(mtcars)

context("na_locf.cpp")

# =============================================================================
# Test on numeric vector
# =============================================================================
mtcars$mpg[c(3, 10, 18, 22)] <- NA
mpg_w_na <- mtcars$mpg
mpg_locf <- bfuncs::na_locf(mtcars$mpg)

test_that("The values prior to the NA values are carried forward as expected", {
  expect_equal(
    mpg_locf,
    c(
      21, 21, 21, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 22.8, 17.8, 16.4, 17.3,
      15.2, 10.4, 10.4, 14.7, 14.7, 30.4, 33.9, 21.5, 21.5, 15.2, 13.3, 19.2,
      27.3, 26, 30.4, 15.8, 19.7, 15, 21.4
    )
  )
})
