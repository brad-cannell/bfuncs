library(tidyverse)
library(bfuncs)
data(mtcars)

context("test-freq_table.R")

# =============================================================================
# Test one-way frequency tables
# =============================================================================
df <- mtcars %>%
  group_by(am) %>%
  freq_table()

test_that("Dimensions of the object returned by freq_table are as expected", {
  rows    <- nrow(df)
  columns <- ncol(df)

  expect_equal(rows, 2L)
  expect_equal(columns, 6L)
})

test_that("Class of freq_table_one_way is freq_table_one_way", {
  expect_is(df, "freq_table_one_way")
})

test_that("The correct var name is returned by freq_table", {
  name <- names(df)[1]
  expect_match(name, "am")
})

test_that("The correct variables levels are returned by freq_table", {
  levels <- df[, 1] %>% unlist() %>% unname()
  expect_equal(levels, c(0, 1))
})

test_that("The correct default statistics are returned by freq_table", {
  n        <- df[, 2] %>% unlist() %>% unname()
  n_total  <- df[, 3] %>% unlist() %>% unname()
  percent  <- df[, 4] %>% unlist() %>% unname()
  lcl      <- df[, 5] %>% unlist() %>% unname()
  ucl      <- df[, 6] %>% unlist() %>% unname()

  expect_equal(n,       c(19, 13))
  expect_equal(n_total, c(32, 32))
  expect_equal(percent, c(59.38, 40.62))
  expect_equal(lcl,     c(40.94, 24.50))
  expect_equal(ucl,     c(75.50, 59.06))
})

# Testing Wald CI's
# -----------------
df <- mtcars %>%
  group_by(am) %>%
  freq_table(ci_type = "wald")

test_that("The correct Wald CI's are returned by freq_table", {
  lcl      <- df[1, 5] %>% as.numeric()
  ucl      <- df[1, 6] %>% as.numeric()

  expect_equal(lcl, 41.38)
  expect_equal(ucl, 77.37)
})




# =============================================================================
# Test two-way freq tables
# =============================================================================
df <- mtcars %>%
  group_by(am, cyl) %>%
  freq_table()

test_that("Dimensions of the object returned by freq_table are as expected", {
  rows    <- nrow(df)
  columns <- ncol(df)

  expect_equal(rows, 6L)
  expect_equal(columns, 8L)
})

test_that("Class of freq_table_two_way is freq_table_two_way", {
  expect_is(df, "freq_table_two_way")
})

test_that("The correct var names are returned by freq_table", {
  group_1 <- names(df)[1]
  group_2 <- names(df)[2]

  expect_match(group_1, "am")
  expect_match(group_2, "cyl")
})

test_that("The correct variables levels are returned by freq_table", {
  level_1 <- df[, 1] %>% unlist() %>% unname()
  level_2 <- df[, 2] %>% unlist() %>% unname()

  expect_equal(level_1, c(0, 0, 0, 1, 1, 1))
  expect_equal(level_2, c(4, 6, 8, 4, 6, 8))
})

test_that("The correct default statistics are returned by freq_table", {
  n           <- df[, 3] %>% unlist() %>% unname()
  n_row       <- df[, 4] %>% unlist() %>% unname()
  n_total     <- df[, 5] %>% unlist() %>% unname()
  percent_row <- df[, 6] %>% unlist() %>% unname()
  lcl_row     <- df[, 7] %>% unlist() %>% unname()
  ucl_row     <- df[, 8] %>% unlist() %>% unname()

  expect_equal(n,           c(3, 4, 12, 8, 3, 2))
  expect_equal(n_row,       c(rep(19, 3), rep(13, 3)))
  expect_equal(n_total,     c(rep(32, 6)))
  expect_equal(percent_row, c(15.79, 21.05, 63.16, 61.54, 23.08, 15.38))
  expect_equal(lcl_row,     c(4.78, 7.58, 38.76, 32.30, 6.91, 3.43))
  expect_equal(ucl_row,     c(41.20, 46.44, 82.28, 84.29, 54.82, 48.18))
})

# Checking overall percents and CI's
# ----------------------------------
df <- mtcars %>%
  group_by(am, cyl) %>%
  freq_table(output = "all")

test_that("The correct overall percents and 95% CI's are returned", {
  percent_total <- df[, 6] %>% unlist() %>% unname()
  lcl_total     <- df[, 9] %>% unlist() %>% unname()
  ucl_total     <- df[, 10] %>% unlist() %>% unname()

  expect_equal(percent_total, c(9.38, 12.50, 37.50, 25.00, 9.38, 6.25))
  expect_equal(lcl_total,     c(2.86, 4.51, 21.97, 12.51, 2.86, 1.45))
  expect_equal(ucl_total,     c(26.66, 30.19, 56.11, 43.72, 26.66, 23.24))
})




# =============================================================================
# Check changing default parameters
# =============================================================================

# 99% confidence intervals instead of 95% confidence intervals
# ------------------------------------------------------------
alpha <- 1 - .99
t <- 1 - alpha / 2

df <- mtcars %>%
  group_by(am) %>%
  freq_table(t_prob = t)

test_that("The 99% confidence intervals are correct", {
  lcl <- df[, 5] %>% unlist() %>% unname()
  ucl <- df[, 6] %>% unlist() %>% unname()

  expect_equal(lcl, c(34.89, 20.05))
  expect_equal(ucl, c(79.95, 65.11))
})

# digits = 3
# ----------
df <- mtcars %>%
  group_by(am) %>%
  freq_table(digits = 3)

test_that("The 'digits' parameter works as expected", {
  percent <- df[, 4] %>% unlist() %>% unname()
  lcl     <- df[, 5] %>% unlist() %>% unname()
  ucl     <- df[, 6] %>% unlist() %>% unname()

  expect_equal(percent, c(59.375, 40.625))
  expect_equal(lcl,     c(40.942, 24.502))
  expect_equal(ucl,     c(75.498, 59.058))
})




# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, alpha, t)
detach("package:tidyverse", unload=TRUE)
detach("package:bfuncs", unload=TRUE)
