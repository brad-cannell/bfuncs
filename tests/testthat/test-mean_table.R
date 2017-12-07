library(tidyverse)
library(bfuncs)
data(mtcars)

context("test-mean_table.R")


# =============================================================================
# Test one-way table of means
# =============================================================================
df <- mtcars %>%
  mean_table(mpg)

test_that("Dimensions of the object returned by mean_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(1, 7))
})

test_that("Class of mean_table is mean_table", {
  expect_is(df, "mean_table")
})

test_that("The correct var name is returned by mean_table", {
  name <- df[1, 1] %>% unlist()
  expect_match(name, "mpg")
})

test_that("The correct statistics are returned by mean_table", {
  n    <- df[1, 2] %>% as.integer()
  mean <- df[1, 3] %>% as.numeric()
  lcl  <- df[1, 4] %>% as.numeric()
  ucl  <- df[1, 5] %>% as.numeric()
  min  <- df[1, 6] %>% as.numeric()
  max  <- df[1, 7] %>% as.numeric()

  expect_equal(n, 32L)
  expect_equal(mean, 20.09)
  expect_equal(lcl, 17.92)
  expect_equal(ucl, 22.26)
  expect_equal(min, 10.4)
  expect_equal(max, 33.9)
})




# =============================================================================
# Test grouped means table
# =============================================================================
df <- mtcars %>%
  group_by(cyl) %>%
  mean_table(mpg)

test_that("Dimensions of the object returned by mean_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(3, 8))
})

test_that("Class of mean_table is mean_table_grouped", {
  expect_is(df, "mean_table_grouped")
})

test_that("The correct var names are returned by mean_table", {
  group_level <- df[1, 1] %>% as.numeric()
  name <- df[1, 2] %>% unlist()

  expect_equal(group_level, 4)
  expect_match(name, "mpg")
})

test_that("The correct statistics are returned by mean_table", {
  n    <- df[1, 3] %>% as.integer()
  mean <- df[1, 4] %>% as.numeric()
  lcl  <- df[1, 5] %>% as.numeric()
  ucl  <- df[1, 6] %>% as.numeric()
  min  <- df[1, 7] %>% as.numeric()
  max  <- df[1, 8] %>% as.numeric()

  expect_equal(n, 11L)
  expect_equal(mean, 26.66)
  expect_equal(lcl, 23.63)
  expect_equal(ucl, 29.69)
  expect_equal(min, 21.4)
  expect_equal(max, 33.9)
})




# =============================================================================
# Check changing default parameters
# =============================================================================

# 99% confidence intervals instead of 95% confidence intervals
alpha <- 1 - .99
t <- 1 - alpha / 2

df <- mtcars %>%
  mean_table(mpg, t_prob = t)

test_that("The 99% confidence intervals are correct", {
  lcl <- df[1, 4] %>% as.numeric()
  ucl <- df[1, 5] %>% as.numeric()

  expect_equal(lcl, 17.17)
  expect_equal(ucl, 23.01)
})

# Output = "all"
df <- mtcars %>%
  mean_table(mpg, output = "all")

test_that("Additional statistics from output = 'all' are as expected", {
  n_miss <- df[1, 2] %>% as.numeric()
  t_crit <- df[1, 5] %>% as.numeric()
  sem    <- df[1, 6] %>% as.numeric()

  expect_equal(n_miss, 0)
  expect_equal(t_crit, 2.039513, tolerance = .000001)
  expect_equal(sem, 1.065424, tolerance = .000001)
})

# digits = 3
df <- mtcars %>%
  mean_table(mpg, digits = 3)

test_that("The 'digits' parameter works as expected", {
  mean <- df[1, 3] %>% as.numeric()
  lcl  <- df[1, 4] %>% as.numeric()
  ucl  <- df[1, 5] %>% as.numeric()

  expect_equal(mean, 20.091)
  expect_equal(lcl, 17.918)
  expect_equal(ucl, 22.264)
})




# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, alpha, t)
detach("package:tidyverse", unload=TRUE)
detach("package:bfuncs", unload=TRUE)
