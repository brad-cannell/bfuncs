context("check_catvars")

# Test data
df <- data.frame(
  a = 1:25,
  b = rep(1:5, times = 5),
  c = rep(letters[1:5], times = 5),
  stringsAsFactors = F
)

# This is how I might use the function interactively

# Check a single categorical variable
# check_catvars(df$a)

# Check every variable in a data frame
# lapply(df, check_catvars)

# Saved Results
results <- lapply(df, check_catvars)

test_that("check_catvars returns message that a variable isn't categorical", {
  expect_match(check_catvars(df$a), "Not a categorical variable")
  expect_match(lapply(df,check_catvars)$a, "Not a categorical variable")
})

test_that("Expected number of categories are returned", {
  expect_that(length(results$a), equals(1))
  expect_that(length(results$b), equals(5))
  expect_that(length(results$c), equals(5))
})

test_that("Expected number of observations for each category are returned", {
  expect_that(results$b[1], is_equivalent_to(5))
  expect_that(results$b[2], is_equivalent_to(5))
  expect_that(results$b[3], is_equivalent_to(5))
  expect_that(results$b[4], is_equivalent_to(5))
  expect_that(results$b[5], is_equivalent_to(5))
  expect_that(results$c[1], is_equivalent_to(5))
  expect_that(results$c[2], is_equivalent_to(5))
  expect_that(results$c[3], is_equivalent_to(5))
  expect_that(results$c[4], is_equivalent_to(5))
  expect_that(results$c[5], is_equivalent_to(5))
})



# Test NA's
# -----------------------------------------------------------------------------
df2 <- data.frame(
  a = c(1, 2, 3, NA, NA)
)

# check_catvars(df2)

test_that("Returns expected number of categories with NA", {
  expect_that(length(check_catvars(df2)), equals(3))
})

test_that("Returns expected number of observations with NA", {
  expect_that(check_catvars(df2)[1], is_equivalent_to(1))
  expect_that(check_catvars(df2)[2], is_equivalent_to(1))
  expect_that(check_catvars(df2)[3], is_equivalent_to(1))
})



# Test with data.table
# -----------------------------------------------------------------------------
dt <- data.table::data.table(df)
results <- lapply(dt, check_catvars)

test_that("check_catvars acts as expected with data.table", {
  expect_match(check_catvars(df$a), "Not a categorical variable")
  expect_that(length(results$a), equals(1))
  expect_that(results$b[1], is_equivalent_to(5))
})