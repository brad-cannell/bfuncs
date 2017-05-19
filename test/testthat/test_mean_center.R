context("mean_center")

df <- data.frame(
  a = 1:100,
  b = 101:200,
  c = rep(letters, length.out = 100),
  stringsAsFactors = F
)

# This is how I might use the function interactively

# Mean center a single numeric variable
# (df$c_a <- mean_center(df$a))

# Mean center multiple numeric variables
# do.call(cbind, lapply(df[c("a", "b")], mean_center))
# colnames(centered) <- c("c_a", "c_b")
# df2 <- cbind(df, centered)

test_that("mean_center returns a warning for non-numeric vectors", {
  expect_that(mean_center(df$c), throws_error("x must be numeric"))
})

test_that("mean_center returns expected values", {

  mean_center(df[1:5, "a"])
  mean_center(df[1:5, "b"])

  expect_that(mean_center(df[1:5, "a"]), equals(c(-2, -1, 0, 1, 2)))
  expect_that(mean_center(df[1:5, "b"]), equals(c(-2, -1, 0, 1, 2)))
})