library(tidyverse)
library(bfuncs)
data(mtcars)

context("test-format_table.R")

# =============================================================================
# Test one-way table of means, stats = mean and ci (default)
# =============================================================================
df <- mtcars %>%
  mean_table(mpg) %>%
  format_table()

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(1L, 2L))
})

test_that("The correct var name is returned by format_table", {
  response_var <- pull(df, response_var) %>% unique()
  expect_match(response_var, "mpg")
})

test_that("The correct statistics are returned by format_table", {
  mean_95 <- pull(df, mean_95)
  expect_equal(mean_95, "20.09 (17.92 - 22.26)")
})




# =============================================================================
# Test one-way table of means, stats = n and mean
# =============================================================================
df <- mtcars %>%
  mean_table(mpg) %>%
  format_table(stats = "n and mean")

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(1L, 2L))
})

test_that("The correct var name is returned by format_table", {
  response_var <- pull(df, response_var) %>% unique()
  expect_match(response_var, "mpg")
})

test_that("The correct statistics are returned by format_table", {
  n_mean <- pull(df, n_mean)
  expect_equal(n_mean, "32 (20.09)")
})




# =============================================================================
# Test grouped table of means, stats = mean and ci (default)
# =============================================================================
df <- mtcars %>%
  group_by(am) %>%
  mean_table(mpg) %>%
  format_table()

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(2L, 4L))
})

test_that("The correct var name is returned by format_table", {
  response_var <- pull(df, response_var) %>% unique()
  group_var    <- pull(df, group_var) %>% unique()

  expect_match(response_var, "mpg")
  expect_match(group_var, "am")
})

test_that("The expected group categories are returned by format_table", {
  group_cat <- pull(df, group_cat)
  expect_equal(group_cat, c(0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  mean_95 <- pull(df, mean_95)
  expect_equal(mean_95, c("17.15 (15.30 - 19.00)", "24.39 (20.67 - 28.12)"))
})




# =============================================================================
# Test grouped table of means, stats = n and mean
# =============================================================================
df <- mtcars %>%
  group_by(am) %>%
  mean_table(mpg) %>%
  format_table(stats = "n and mean")

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(2L, 4L))
})

test_that("The correct var name is returned by format_table", {
  response_var <- pull(df, response_var) %>% unique()
  group_var    <- pull(df, group_var) %>% unique()

  expect_match(response_var, "mpg")
  expect_match(group_var, "am")
})

test_that("The expected group categories are returned by format_table", {
  group_cat <- pull(df, group_cat)
  expect_equal(group_cat, c(0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  n_mean <- pull(df, n_mean)
  expect_equal(n_mean, c("19 (17.15)", "13 (24.39)"))
})




# =============================================================================
# Test grouped table of means with two group_by vars
# =============================================================================
df <- mtcars %>%
  group_by(am, vs) %>%
  mean_table(mpg) %>%
  format_table(stats = "n and mean")

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(4L, 6L))
})

test_that("The correct var name is returned by format_table", {
  response_var <- pull(df, response_var) %>% unique()
  group_1      <- pull(df, group_1) %>% unique()
  group_2      <- pull(df, group_2) %>% unique()

  expect_match(response_var, "mpg")
  expect_match(group_1, "am")
  expect_match(group_2, "vs")
})

test_that("The expected group categories are returned by format_table", {
  group_1_cat <- pull(df, group_1_cat)
  group_2_cat <- pull(df, group_2_cat)

  expect_equal(group_1_cat, c(0L, 0L, 1L, 1L))
  expect_equal(group_2_cat, c(0L, 1L, 0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  n_mean <- pull(df, n_mean)
  expect_equal(n_mean, c("12 (15.05)", "7 (20.74)", "6 (19.75)", "7 (28.37)"))
})




# =============================================================================
# Test one-way frequency table, stats = percent and ci (default)
# =============================================================================
df <- mtcars %>%
  group_by(am) %>%
  freq_table() %>%
  format_table()

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(2L, 3L))
})

test_that("The correct var name is returned by format_table", {
  var <- pull(df, var) %>% unique()
  expect_match(var, "am")
})

test_that("The correct variables levels are returned by format_table", {
  cat <- pull(df, cat)
  expect_equal(cat, c(0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  percent_95 <- pull(df, percent_95)
  expect_equal(percent_95, c("59.38 (40.94 - 75.50)", "40.62 (24.50 - 59.06)"))
})




# =============================================================================
# Test one-way frequency table, stats = n and percent
# =============================================================================
df <- mtcars %>%
  group_by(am) %>%
  freq_table() %>%
  format_table(stats = "n and percent")

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(2L, 3L))
})

test_that("The correct var name is returned by format_table", {
  var <- pull(df, var) %>% unique()
  expect_match(var, "am")
})

test_that("The correct variables levels are returned by format_table", {
  cat <- pull(df, cat)
  expect_equal(cat, c(0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  n_percent <- pull(df, n_percent)
  expect_equal(n_percent, c("19 (59.38)", "13 (40.62)"))
})




# =============================================================================
# Test two-way frequency table, stats = percent and ci (default)
# =============================================================================
df <- mtcars %>%
  group_by(am, vs) %>%
  freq_table() %>%
  format_table()

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(4L, 5L))
})

test_that("The correct var name is returned by format_table", {
  row_var <- pull(df, row_var) %>% unique()
  col_var <- pull(df, col_var) %>% unique()

  expect_match(row_var, "am")
  expect_match(col_var, "vs")
})

test_that("The correct variables levels are returned by format_table", {
  row_cat <- pull(df, row_cat)
  col_cat <- pull(df, col_cat)

  expect_equal(row_cat, c(0L, 0L, 1L, 1L))
  expect_equal(col_cat, c(0L, 1L, 0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  percent_row_95 <- pull(df, percent_row_95)
  expect_equal(percent_row_95, c("63.16 (38.76 - 82.28)", "36.84 (17.72 - 61.24)",
                                 "46.15 (20.83 - 73.63)", "53.85 (26.37 - 79.17)"))
})




# =============================================================================
# Test two-way frequency table, stats = n and percent
# =============================================================================
df <- mtcars %>%
  group_by(am, vs) %>%
  freq_table(output = all) %>%
  format_table(stats = "n and percent")

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(4L, 5L))
})

test_that("The correct var name is returned by format_table", {
  row_var <- pull(df, row_var) %>% unique()
  col_var <- pull(df, col_var) %>% unique()

  expect_match(row_var, "am")
  expect_match(col_var, "vs")
})

test_that("The correct variables levels are returned by format_table", {
  row_cat <- pull(df, row_cat)
  col_cat <- pull(df, col_cat)

  expect_equal(row_cat, c(0L, 0L, 1L, 1L))
  expect_equal(col_cat, c(0L, 1L, 0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  n_percent_total <- pull(df, n_percent_total)
  expect_equal(n_percent_total, c("12 (37.50)", "7 (21.88)",
                                  "6 (18.75)", "7 (21.88)"))
})




# =============================================================================
# Test digits option
# =============================================================================
df <- mtcars %>%
  mean_table(mpg) %>%
  format_table(digits = 3)

test_that("The digits argument of format_table works as expected", {
  mean_95 <- pull(df, mean_95)
  expect_equal(mean_95, "20.090 (17.920 - 22.260)")
})
