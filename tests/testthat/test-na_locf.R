library(tidyverse)
library(bfuncs)
data(mtcars)

context("na_locf.cpp")

# =============================================================================
# Test on numeric vector
# =============================================================================
set.seed(123)
mtcars$mpg[sample(seq_along(mtcars$mpg), 4)] <- NA
mpg_w_na <- mtcars$mpg
mpg_locf <- bfuncs::na_locf(mtcars$mpg)

test_that("The values prior to the NA values are carried forward as expected", {
  expect_equal(
    mpg_locf,
    c(
      21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 22.8, 17.8, 16.4, 16.4,
      15.2, 10.4, 10.4, 14.7, 32.4, 30.4, 33.9, 21.5, 15.5, 15.2, 13.3, 13.3,
      13.3, 26, 30.4, 15.8, 19.7, 15, 21.4
    )
  )
})
