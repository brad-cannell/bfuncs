context("recode_miss")

(df <- data.frame(
  edu = c(1, 2, 4, 7, 4),
  marital = c(1, 1, 7, 9, 3),
  race = c(1, 1, 3, 2, NA),
  month = c("May", "Feb", "Sep", "UNK", "?"),
  stringsAsFactors = FALSE
))

# This is how I might use the function interactively:

# Recode a single numeric variable
# recode_miss(df$marital, values = c(7, 9))

# Recode a single character variable
# recode_miss(df$month, values = c("UNK", "?"))

# Recode multiple variables
# lapply(df, recode_miss, values = c(7, 9, "UNK", "?"))


test_that("Nothing changes when values argument at default", {
  expect_that(recode_miss(df$edu), equals(c(1, 2, 4, 7, 4)))
  expect_that(recode_miss(df$marital), equals(c(1, 1, 7, 9, 3)))
  expect_that(recode_miss(df$race), equals(c(1, 1, 3, 2, NA)))
  expect_that(recode_miss(df$month), equals(c("May", "Feb", "Sep", "UNK", "?")))
})


test_that("Desired values are converted to NA", {
  expect_that(recode_miss(df$edu, values = c(7, 9)),
              equals(c(1, 2, 4, NA, 4)))
  expect_that(recode_miss(df$marital, values = c(7, 9)),
              equals(c(1, 1, NA, NA, 3)))
  expect_that(recode_miss(df$race, values = c(7, 9)),
              equals(c(1, 1, 3, 2, NA)))
  expect_that(recode_miss(df$month, values = c("UNK", "?")),
              equals(c("May", "Feb", "Sep", NA, NA)))
})