library(tidyverse)
library(bfuncs)
data(mtcars)

context("test-mean_table.R")


# =============================================================================
# Test one-way table of means with output = all
# =============================================================================
df <- mtcars %>%
  mean_table(mpg, output = all)

test_that("Dimensions of the object returned by mean_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(1L, 10L))
})

test_that("Class of mean_table is mean_table", {
  expect_is(df, "mean_table")
})

test_that("The correct var name is returned by mean_table", {
  response_var <- pull(df, response_var) %>% unique()
  expect_match(response_var, "mpg")
})

test_that("The correct statistics are returned by mean_table", {
  n_miss <- pull(df, n_miss)
  n      <- pull(df, n)
  mean   <- pull(df, mean)
  t_crit <- pull(df, t_crit) %>% round(6)
  sem    <- pull(df, sem) %>% round(6)
  lcl    <- pull(df, lcl)
  ucl    <- pull(df, ucl)
  min    <- pull(df, min)
  max    <- pull(df, max)

  expect_equal(n_miss, 0L)
  expect_equal(n, 32L)
  expect_equal(mean, 20.09)
  expect_equal(t_crit, 2.039513)
  expect_equal(sem, 1.065424)
  expect_equal(lcl, 17.92)
  expect_equal(ucl, 22.26)
  expect_equal(min, 10.4)
  expect_equal(max, 33.9)
})




# =============================================================================
# Test grouped means table with output = all
# =============================================================================
df <- mtcars %>%
  group_by(cyl) %>%
  mean_table(mpg, output = all)

test_that("Dimensions of the object returned by mean_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(3L, 12L))
})

test_that("Class of mean_table is mean_table_grouped", {
  expect_is(df, "mean_table_grouped")
})

test_that("The correct var names are returned by mean_table", {
  response_var <- pull(df, response_var)
  group_var    <- pull(df, group_var)

  expect_match(response_var, "mpg")
  expect_match(group_var, "cyl")
})

test_that("The expected group categories are returned by mean_table", {
  group_cat <- pull(df, group_cat)
  expect_equal(group_cat, c(4L, 6L, 8L))
})

test_that("The correct statistics are returned by mean_table", {
  n_miss <- pull(df, n_miss)
  n      <- pull(df, n)
  mean   <- pull(df, mean)
  t_crit <- pull(df, t_crit) %>% round(6)
  sem    <- pull(df, sem) %>% round(7)
  lcl    <- pull(df, lcl)
  ucl    <- pull(df, ucl)
  min    <- pull(df, min)
  max    <- pull(df, max)

  expect_equal(n_miss, c(0L, 0L, 0L))
  expect_equal(n, c(11L, 7L, 14L))
  expect_equal(mean, c(26.66, 19.74, 15.10))
  expect_equal(t_crit, c(2.228139, 2.446912, 2.160369))
  expect_equal(sem, c(1.3597642, 0.5493967, 0.6842016))
  expect_equal(lcl, c(23.63, 18.40, 13.62))
  expect_equal(ucl, c(29.69, 21.09, 16.58))
  expect_equal(min, c(21.4, 17.8, 10.4))
  expect_equal(max, c(33.9, 21.4, 19.2))
})




# =============================================================================
# Test grouped means table with two group_by variables and output = all
# =============================================================================
df <- mtcars %>%
  group_by(cyl, am) %>%
  mean_table(mpg, output = all)

test_that("Dimensions of the object returned by mean_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(6L, 14L))
})

test_that("Class of mean_table is mean_table_grouped", {
  expect_is(df, "mean_table_grouped")
})

test_that("The correct var names are returned by mean_table", {
  response_var <- pull(df, response_var)
  group_1      <- pull(df, group_1)
  group_2      <- pull(df, group_2)

  expect_match(response_var, "mpg")
  expect_match(group_1, "cyl")
  expect_match(group_2, "am")
})

test_that("The expected group categories are returned by mean_table", {
  group_1_cat <- pull(df, group_1_cat)
  group_2_cat <- pull(df, group_2_cat)

  expect_equal(group_1_cat, c(4L, 4L, 6L, 6L, 8L, 8L))
  expect_equal(group_2_cat, c(0L, 1L, 0L, 1L, 0L, 1L))
})

test_that("The correct statistics are returned by mean_table", {
  # Just need to check a subgroup of all stats here.
  n      <- pull(df, n)
  mean   <- pull(df, mean)
  lcl    <- pull(df, lcl)
  ucl    <- pull(df, ucl)

  expect_equal(n, c(3L, 8L, 4L, 3L, 12L, 2L))
  expect_equal(mean, c(22.90, 28.07, 19.12, 20.57, 15.05, 15.40))
  expect_equal(lcl, c(19.29, 24.33, 16.53, 18.70, 13.29, 10.32))
  expect_equal(ucl, c(26.51, 31.82, 21.72, 22.43, 16.81, 20.48))
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
  lcl <- pull(df, lcl)
  ucl <- pull(df, ucl)

  expect_equal(lcl, 17.17)
  expect_equal(ucl, 23.01)
})

# Output = "default"
df <- mtcars %>%
  mean_table(mpg, output = default)

test_that("Additional default list of statistics from output = default are as expected", {
  vars <- names(df)
  expect_equal(vars, c("response_var", "n", "mean", "sem", "lcl", "ucl",
                       "min", "max"))
})

# digits = 3
df <- mtcars %>%
  mean_table(mpg, digits = 3)

test_that("The 'digits' parameter works as expected", {
  mean <- pull(df, mean)
  lcl  <- pull(df, lcl)
  ucl  <- pull(df, ucl)

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
