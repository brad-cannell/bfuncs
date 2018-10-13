library(testthat)
library(tidyverse)
library(bfuncs)

context("test-check-join-conflicts.R")

df1 <- tibble::tribble(
  ~id, ~first_name, ~gender,
  1,   "john",      "m",
  2,   "jane",      "f",
  3,   "sally",     "f"
)

df2 <- tibble::tribble(
  ~id, ~first_name, ~gender,
  1,   "jon",       "m",
  2,   "jane",      "f",
  3,   "salle",     "f"
)

df3 <- df1 %>% full_join(df2, by = "id")
df3


# =============================================================================
# Test function
# =============================================================================
df <- check_join_conflicts(df3)

test_that("Dimensions of the data frame returned by check_join_conflicts are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(3L, 4L))
})

test_that("The correct var names are returned by check_join_conflicts", {
  expected_var_names <- c("variable", "row", ".x", ".y")
  var_names <- names(df)
  expect_setequal(var_names, expected_var_names)
})

test_that("The correct values are returned by check_join_conflicts", {
  var <- c("first_name", "first_name", "gender")
  row <- c(1, 3, NA)
  x <- c("john", "sally", NA)
  y <- c("jon", "salle", NA)

  expect_setequal(var, df[["variable"]])
  expect_setequal(row, df[["row"]])
  expect_setequal(x, df[[".x"]])
  expect_setequal(y, df[[".y"]])
})


# =============================================================================
# Test on different suffix names
# =============================================================================
df4 <- df3
names(df4) <- stringr::str_replace_all(names(df4), "\\.x", ".medstar")
names(df4) <- stringr::str_replace_all(names(df4), "\\.y", ".aps")

df <- check_join_conflicts(df4, suffix = c("medstar", "aps"))

test_that("Dimensions of the data frame returned by check_join_conflicts are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(3L, 4L))
})

test_that("The correct var names are returned by check_join_conflicts", {
  expected_var_names <- c("variable", "row", ".medstar", ".aps")
  var_names <- names(df)
  expect_setequal(var_names, expected_var_names)
})

test_that("The correct values are returned by check_join_conflicts", {
  var <- c("first_name", "first_name", "gender")
  row <- c(1, 3, NA)
  x <- c("john", "sally", NA)
  y <- c("jon", "salle", NA)

  expect_setequal(var, df[["variable"]])
  expect_setequal(row, df[["row"]])
  expect_setequal(x, df[[".medstar"]])
  expect_setequal(y, df[[".aps"]])
})
