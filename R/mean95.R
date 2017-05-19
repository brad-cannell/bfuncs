#' Calculate Means and 95 Percent Confidence Intervals
#'
#' The mean95 function calculates the mean, sd, and 95% confidence interval for
#' a numeric vector.
#'
#' @param x A numeric vector.
#' @param digits Rounds the values returned to the specified number of decimal
#'  places (default 3).
#'
#' @return A named vector containing the mean, sd, and 95% confidence interval.
#' @export
#'
#' @examples
#' data("mtcars")
#'
#' # Single univariate analysis
#' mean95(mtcars$mpg)
#' with(mtcars, mean95(mpg))
#'
#' # Single bivariate analyses
#' with(mtcars, by(mpg, am, mean95))
mean95 <- function(x, digits = 3) {
  count <- length(na.omit(x))
  mean = mean(x, na.rm = T)
  sd = sd(x, na.rm = T)
  lcl = mean(x, na.rm=TRUE) - qt(.975, count - 1) *
      sd(x, na.rm = TRUE) / sqrt(count)
  ucl = mean(x, na.rm=TRUE) + qt(.975, count - 1) *
      sd(x, na.rm = TRUE) / sqrt(count)

  # Output data frame
  data.frame(
    mean = round(mean, digits),
    sd = round(sd, digits),
    lcl = round(lcl, digits),
    ucl = round(ucl, digits)
  )
}
