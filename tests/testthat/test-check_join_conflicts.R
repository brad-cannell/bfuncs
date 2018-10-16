library(testthat)
library(tidyverse)
library(bfuncs)

context("test-check-join-conflicts.R")

df1 <- tibble::tribble(
  ~id, ~first_name, ~gender, ~height,
  1,   "john",      "m",     71,
  2,   "jane",      "f",     64,
  3,   "sally",     "f",     65
)

df2 <- tibble::tribble(
  ~id, ~first_name, ~gender, ~height,
  1,   "jon",       "m",     70,
  2,   "jane",      "f",     64,
  3,   "salle",     "f",     65
)

df3 <- df1 %>% full_join(df2, by = "id")
df3


# =============================================================================
# Test function
# =============================================================================

df <- check_join_conflicts(df3, show_context = FALSE)

test_that("Dimensions of the data frame returned by check_join_conflicts are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(4L, 4L))
})

test_that("The correct var names are returned by check_join_conflicts", {
  expected_var_names <- c("variable", "row", ".x", ".y")
  var_names <- names(df)
  expect_setequal(var_names, expected_var_names)
})

test_that("The correct values are returned by check_join_conflicts", {
  var <- c("first_name", "first_name", "gender", "height")
  row <- c(1, 3, NA, 1)
  x <- c("john", "sally", NA, 71)
  y <- c("jon", "salle", NA, 70)

  expect_setequal(var, df[["variable"]])
  expect_setequal(row, df[["row"]])
  expect_setequal(x, df[[".x"]])
  expect_setequal(y, df[[".y"]])
})

df <- check_join_conflicts(df3, show_context = TRUE)

test_that("Show context works", {
  dims <- dim(df)
  expect_equal(dims, c(4L, 10L))
})


# =============================================================================
# Test on different suffix names
# =============================================================================

df4 <- df3
names(df4) <- stringr::str_replace_all(names(df4), "\\.x", ".medstar")
names(df4) <- stringr::str_replace_all(names(df4), "\\.y", ".aps")

df <- check_join_conflicts(df4, suffix = c("medstar", "aps"), show_context = FALSE)

test_that("Dimensions of the data frame returned by check_join_conflicts are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(4L, 4L))
})

test_that("The correct var names are returned by check_join_conflicts", {
  expected_var_names <- c("variable", "row", ".medstar", ".aps")
  var_names <- names(df)
  expect_setequal(var_names, expected_var_names)
})

test_that("The correct values are returned by check_join_conflicts", {
  var <- c("first_name", "first_name", "gender", "height")
  row <- c(1, 3, NA, 1)
  x <- c("john", "sally", NA, 71)
  y <- c("jon", "salle", NA, 70)

  expect_setequal(var, df[["variable"]])
  expect_setequal(row, df[["row"]])
  expect_setequal(x, df[[".medstar"]])
  expect_setequal(y, df[[".aps"]])
})
