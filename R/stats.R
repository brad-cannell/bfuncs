#' @title Estimate Means and Related Statistics for Continuous Variables
#'
#' @description Estimate Means and Related Statistics for Continuous Variables
#'
#' @param x A continuous variable.
#' @param digits Rounds the values returned to the specified number of decimal
#'  places (default 3).
#'
#' @return A numeric vector contining the n, mean, median, variance, standard
#'  deviation, standard error of the mean, t critical variable, 95 percent
#'  confidnece interval, min, and max.
#' @export
#'
#' @examples
#' data(mtcars)
#'
#' # Single univariate analysis
#' stats(mtcars$mpg)
stats <- function(x, digits = 3) {
  count <- function(x) length(na.omit(x))
  n = count(x)
  miss = sum(is.na(x))
  mean = mean(x, na.rm = TRUE)
  lcl = mean(x, na.rm = TRUE) - qt(.975, count(x) - 1) *
      sd(x, na.rm = TRUE) / sqrt(count(x))
  ucl = mean(x, na.rm = TRUE) + qt(.975, count(x) - 1) *
      sd(x, na.rm = TRUE) / sqrt(count(x))
  sem = sqrt(var(x, na.rm = TRUE) / length(na.omit(x)))
  median = median(x, na.rm = TRUE)
  var = var(x, na.rm = TRUE)
  sd = sd(x, na.rm = TRUE)
  min = min(x, na.rm = TRUE)
  max = max(x, na.rm = TRUE)

  # Output data frame
  data.frame(
    Non_Missing = n,
    Missing = miss,
    Mean = round(mean, digits),
    Mean_LCL = round(lcl, digits),
    Mean_UCL = round(ucl, digits),
    SEM = round(sem, digits),
    Median = round(median, digits),
    Variance = round(var, digits),
    SD = round(sd, digits),
    Min = round(min, digits),
    Max = round(max, digits)
  )
}