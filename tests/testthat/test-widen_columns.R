library(testthat)
library(tidyverse)
library(bfuncs)

context("test-widen-columns.R")

df <- tibble(
  id = c(1, 1, 1, 1, 2),
  first = c("john", "john", "john", "john", "jane"),
  last = c("doe", "doe", "doe", "doe", "smith"),
  condition = c("diabetes", "diabetes", "mi", "mi", "depression"),
  meds = c("asprin", "tylenol", "asprin", "tylenol", "asprin")
)

df <- df %>%
  group_by(id) %>%
  widen_columns(meds, condition)

test_that("Dimensions of the data frame returned by widen_columns are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(2L, 7L))
})

test_that("The correct var names are returned", {
  expected_var_names <- c("id", "first", "last", "meds_1", "meds_2", "condition_1", "condition_2")
  var_names <- names(df)
  expect_setequal(var_names, expected_var_names)
})

test_that("The correct values are returned", {
  ex_meds_1 <- c("asprin", "asprin")
  ex_meds_2 <- c("tylenol", NA)
  ex_conditions_1 <- c("diabetes", "depression")
  ex_conditions_2 <- c("mi", NA)

  expect_setequal(ex_meds_1, df[["meds_1"]])
  expect_setequal(ex_meds_2, df[["meds_2"]])
  expect_setequal(ex_conditions_1, df[["condition_1"]])
  expect_setequal(ex_conditions_2, df[["condition_2"]])
})
