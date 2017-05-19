#' @title Compact Table of Summary Statistics
#'
#' @description  Based on Stata's "tabstat" command. "tabstat displays summary
#'  statistics for a series of numeric variables in one table.  It allows you
#'  to specify the list of statistics to be displayed.  Statistics can be
#'  calculated (conditioned on) another variable.  tabstat allows substantial
#'  flexibility in terms of the statistics presented and the format of the
#'  table" (Stata, 2016).
#'
#' @param x A continuous variable.
#' @param digits Rounds the values returned to the specified number of decimal
#'  places (default 3).
#' @param stats Return specified statistics. Options include:
#'  \describe{
#'    \item{n}{Count of nonmissing values of x}
#'    \item{nmiss}{Count of missing values of x}
#'    \item{ci}{95 percent confidence interval for the mean of x}
#'    \item{sum}{Sum of x}
#'    \item{max}{Maximum value of x}
#'    \item{min}{Minimum value of x}
#'    \item{range}{(Maximum value of x) - (minimum value of x)}
#'    \item{sd}{Standard deviation of x}
#'    \item{var}{Variance of x}
#'    \item{cv}{Coefficient of variation (sd / mean) of x}
#'    \item{sem}{Standard error of the mean of x}
#'    \item{skew}{Skewness of x}
#'    \item{kurt}{Kurtosis of x}
#'    \item{p1}{1st percentile of x}
#'    \item{p5}{5th percentile of x}
#'    \item{p10}{10th percentile of x}
#'    \item{p25}{25th percentile of x}
#'    \item{p50}{Median value of x}
#'    \item{median}{Median value of x}
#'    \item{p75}{75th percentile of x}
#'    \item{p90}{90th percentile of x}
#'    \item{p95}{95th percentile of x}
#'    \item{p99}{99th percentile of x}
#'    \item{iqr}{Interquartile range (p75 - p25)}
#'    \item{q}{Equivalent to specifying p25 p50 p75}
#'  }
#'
#' @return A data frame. By default, the data frame contains the variable name
#'  and mean.
#' @export
#'
#' @references Stata 14 help for tabstat
#'  \url{http://www.stata.com/help.cgi?tabstat}
#'
#' @examples
#' data(mtcars)
#'
#' # Single univariate analysis with Defaults
#' tabstat(mtcars$mpg)
#'
#' # Single univariate analysis with all stats
#' tabstat(mtcars$mpg, stats = c("n", "nmiss", "ci", "sum", "max", "min",
#' "range", "sd", "var", "cv", "sem", "skew", "kurt", "p1", "p5", "p10",
#' "p25", "p50", "median", "p75", "p90", "p95", "p99", "iqr", "q"))

tabstat <- function(x, digits = 3, stats = "default") {

  if (missing(x)){                               # Check arguments
    stop()
  } else if (is.factor(x)) {
    stop("factors objects not allowed, argument 'x' should be a numeric")
  } else if (is.character(x)) {
    stop("character objects not allowed, argument 'x' should be a numeric")
  }

  name = deparse(substitute(x))                  # Grab variable string
  name = sub("^[^\\$]*\\$", "", name)            # Regex remove up to $
  x2 = na.omit(x)                                # Remove NA
  n = length(x2)                                 # Count nonmissing observations
  mean = mean(x2)                                # Mean

  df <- data.frame(                              # Default data frame
    variable = name,
    mean = round(mean, digits)
  )

  if ("n" %in% stats){                           # nonmissing observations
    df <- cbind(df, n)
    df <- df[c(1, 3, 2)]                         # Reorder n before mean
  }

  if ("nmiss" %in% stats){                       # missing observations
    nmiss = sum(is.na(x2))
    df <- cbind(df, nmiss)
    if ("n" %in% stats){
      df <- df[c(1:2, 4, 3)]                     # Reorder name, n, nmiss, mean
    } else {
      df <- df[c(1, 3, 2)]                       # Reorder name, nmiss, mean
    }
  }

  if ("ci" %in% stats) {                         # 95% Confidence Intervals
    lcl = round(mean(x2) - qt(.975, n - 1) * sd(x2) / sqrt(n), digits)
    ucl = round(mean(x2) + qt(.975, n - 1) * sd(x2) / sqrt(n), digits)
    df <- cbind(df, mean_lcl = lcl, mean_ucl = ucl)
  }

  if ("sum" %in% stats) {                        # Sum values
    sum = round(sum(x2), digits)
    df <- cbind(df, sum)
  }

  if ("max" %in% stats) {                        # Maximum value
    max = round(max(x2), digits)
    df <- cbind(df, max)
  }

  if ("min" %in% stats) {                        # Minimum value
    min = round(min(x2), digits)
    df <- cbind(df, min)
  }

  if ("range" %in% stats) {                      # Range
    range = round(max(x2) - min(x2), digits)
    df <- cbind(df, range)
  }

  if ("sd" %in% stats){                          # Standard deviation
    std_dev = round(sd(x2), digits)
    df <- cbind(df, std_dev)
  }

  if ("var" %in% stats){                         # Variance
    variance = round(var(x2), digits)
    df <- cbind(df, variance)
  }

  if ("cv" %in% stats){                          # Coefficient of variation
    coef_var = round(sd(x2) / mean(x2), digits)  # (sd / mean)
    df <- cbind(df, coef_var)
  }

  if ("sem" %in% stats){                         # Coefficient of variation
    sem = round(sd(x2) / sqrt(n), digits)        # (sd / sqrt(n))
    df <- cbind(df, sem)
  }

  if ("skew" %in% stats){                        # Skewness
    skewness = round((sum((x2 - mean(x2))^3) / n) /
        (sum((x2 - mean(x2))^2) / n)^(3 / 2), digits)
    df <- cbind(df, skewness)
  }

  if ("kurt" %in% stats){                        # Kurtosis
    kurtosis = round(n * sum((x2 - mean(x2))^4) /
        (sum((x2 - mean(x2))^2)^2), digits)
    df <- cbind(df, kurtosis)
  }

  if ("p1" %in% stats){                          # 1st percentile
    p1 = round(quantile(x2, .01), digits)
    names(p1) <- NULL
    df <- cbind(df, p1)
  }

  if ("p5" %in% stats){                          # 5th percentile
    p5 = round(quantile(x2, .05), digits)
    names(p5) <- NULL
    df <- cbind(df, p5)
  }

  if ("p10" %in% stats){                         # 10th percentile
    p10 = round(quantile(x2, .1), digits)
    names(p10) <- NULL
    df <- cbind(df, p10)
  }

  if ("p25" %in% stats){                         # 25th percentile
    p25 = round(quantile(x2, .25), digits)
    names(p25) <- NULL
    df <- cbind(df, p25)
  }

  if ("median" %in% stats | "p50" %in% stats){   # median
    median = round(median(x2), digits)
    df <- cbind(df, median)
  }

  if ("p75" %in% stats){                         # 75th percentile
    p75 = round(quantile(x2, .75), digits)
    names(p75) <- NULL
    df <- cbind(df, p75)
  }

  if ("p90" %in% stats){                         # 90th percentile
    p90 = round(quantile(x2, .90), digits)
    names(p90) <- NULL
    df <- cbind(df, p90)
  }

  if ("p95" %in% stats){                         # 95th percentile
    p95 = round(quantile(x2, .95), digits)
    names(p95) <- NULL
    df <- cbind(df, p95)
  }

  if ("p99" %in% stats){                         # 99th percentile
    p99 = round(quantile(x2, .99), digits)
    names(p99) <- NULL
    df <- cbind(df, p99)
  }

  if ("iqr" %in% stats){                         # Interquartile Range
    p75 = quantile(x2, .75)
    p25 = quantile(x2, .25)
    iqr = round(p75 - p25, digits)
    names(iqr) <- NULL
    df <- cbind(df, iqr)
  }

  if ("q" %in% stats){                           # Shortcut for p25, p50, p75
    p25 = round(quantile(x2, .25), digits)
    names(p25) <- NULL
    p50 = round(quantile(x2, .50), digits)
    names(p50) <- NULL
    p75 = round(quantile(x2, .75), digits)
    names(p75) <- NULL
    df <- cbind(df, p25, p50, p75)
  }

  return(df)
}

# Build out test as you go.