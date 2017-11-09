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
  count_x = length(stats::na.omit(x))
  mean_x  = mean(x, na.rm = T)
  sd_x    = stats::sd(x, na.rm = T)
  t_crit  = stats::qt(.975, count_x - 1)
  sem     = sd_x / sqrt(count_x)
  lcl     = mean_x - t_crit * sem
  ucl     = mean_x + t_crit * sem

  # Output data frame
  data.frame(
    mean = round(mean_x, digits),
    sem  = round(sem, digits),
    lcl  = round(lcl, digits),
    ucl  = round(ucl, digits)
  )
}
