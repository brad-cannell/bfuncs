library(tidyverse)
library(bfuncs)
data(mtcars)

context("test-freq_test.R")

# =============================================================================
# Test one-way frequency tables
# =============================================================================
df <- mtcars %>%
  group_by(am) %>%
  freq_table() %>%
  freq_test()

test_that("Dimensions of the object returned by freq_test are as expected", {
  rows    <- nrow(df)
  columns <- ncol(df)

  expect_equal(rows, 2L)
  expect_equal(columns, 11L)
})

test_that("Class of freq_table_one_way is freq_table_one_way", {
  expect_is(df, "freq_table_one_way")
})

test_that("The correct var name is returned by freq_test", {
  name <- names(df)[1]
  expect_match(name, "am")
})

test_that("The correct variables levels are returned by freq_test", {
  levels <- df[, 1] %>% unlist() %>% unname()
  expect_equal(levels, c(0, 1))
})

test_that("The correct default statistics are returned by freq_test", {
  n_expected     <- df[, 7] %>% unlist() %>% unname()
  chi2_contrib   <- df[, 8] %>% unlist() %>% unname()
  chi2_pearson   <- df[, 9] %>% unlist() %>% unname()
  deg_freedom    <- df[, 10] %>% unlist() %>% unname()
  p_chi2_pearson <- df[, 11] %>% unlist() %>% unname() %>% round(7)

  expect_equal(n_expected,     rep(16, 2))
  expect_equal(chi2_contrib,   rep(0.5625, 2))
  expect_equal(chi2_pearson,   rep(1.125, 2))
  expect_equal(deg_freedom,    rep(1, 2))
  expect_equal(p_chi2_pearson, rep(0.2888444, 2))
})




# =============================================================================
# Test two-way freq tables
# =============================================================================
df <- mtcars %>%
  group_by(am, cyl) %>%
  freq_table()%>%
  freq_test()

test_that("Dimensions of the object returned by freq_table are as expected", {
  rows    <- nrow(df)
  columns <- ncol(df)

  expect_equal(rows, 6L)
  expect_equal(columns, 16L)
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
  n_col          <- df[, 9] %>% unlist() %>% unname()
  n_expected     <- df[, 10] %>% unlist() %>% unname()
  chi2_contrib   <- df[, 11] %>% unlist() %>% unname()
  chi2_pearson   <- df[, 12] %>% unlist() %>% unname()
  r_column       <- df[, 13] %>% unlist() %>% unname()
  c_column       <- df[, 14] %>% unlist() %>% unname()
  deg_freedom    <- df[, 15] %>% unlist() %>% unname()
  p_chi2_pearson <- df[, 16] %>% unlist() %>% unname() %>% round(7)

  expect_equal(n_col,          c(11, 7, 14, 11, 7, 14))
  expect_equal(n_expected,     c(6.53125, 4.15625, 8.31250, 4.46875, 2.84375, 5.68750))
  expect_equal(chi2_contrib,   c(1.909240431, 0.005874060, 1.635808271, 2.790428322,
                                 0.008585165, 2.390796703))
  expect_equal(chi2_pearson,   rep(8.740733, 6))
  expect_equal(r_column,       rep(2, 6))
  expect_equal(c_column,       rep(3, 6))
  expect_equal(deg_freedom,    rep(2, 6))
  expect_equal(p_chi2_pearson, rep(0.01264661, 6))
})

# Checking Fisher's Exact Method
# ------------------------------
df <- mtcars %>%
  group_by(am, cyl) %>%
  freq_table()%>%
  freq_test(method = "fisher")

test_that("The expected p-value is returned from the fisher method", {
  fisher_p_value <- df[, 17] %>% unlist() %>% unname()
  expect_equal(fisher_p_value, rep(0.009104702, 6))
})




# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, alpha, t)
detach("package:tidyverse", unload=TRUE)
detach("package:bfuncs", unload=TRUE)
