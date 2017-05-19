#' @title Estimate Percents and 95 Percent Confidence Intervals
#'
#' @description The props function calculates the number of nonmissing values,
#'  percent, and 95 percent confidence interval for categorical variables.
#'
#' @param x A vector or a list element. Should be a categorical variable.
#' @param citype Choose a method for calculating confidence intervals. Either
#'  "normal" or "logit" (Agresti, 2012).
#' @param digits Number of digits after the decimal point for percents.s
#'
#' @return A data frame containing the variable name and unique levels
#'  (categories). Within each level, the number of non-missing values,
#'  percent, and 95 percent confidence interval for the percent are returned.
#' @export
#'
#' @references Agresti, A. (2012). Categorical Data Analysis (3rd ed.). Hoboken, NJ: Wiley.
#'
#' @examples
#' # data("mtcars")
#'
#' # Single univariate analysis
#' props(mtcars$cyl, citype = "normal", digits = 2)
#' with(mtcars, props(cyl))
props <- function(x, citype = "normal", digits = 5) {
  name <- deparse(substitute(x))               # Grab variable string
  name <- sub("^[^\\$]*\\$", "", name)         # Regex remove up to $
  var <- na.omit(x)                            # Remove NA
  N <- length(var)                             # N of non-missing values
  tab <- unclass(table(var))                   # Create a table
  levels <- names(tab)                         # Grab category names
  prop <- tab / N                              # Proportions
  se <- sqrt(prop * (1-prop)/(N-1))            # Standard errors of props
  stat <- Vectorize(qt, "df")(0.975, N-1)      # tstats

  if (citype == "normal") {                    # Normal approximation cis
    lower <- prop - stat * se                  # Lower ci
    upper <- prop + stat * se                  # Upper ci
  } else  if (citype == "logit") {             # Logit transformed cis
    lprop <- log(prop) - log(1-prop)           # Logged prop
    lse <- se / (prop * (1-prop))              # Logged se
    llower <- lprop - stat * lse               # Log lower
    lupper <- lprop + stat * lse               # Log upper
    lower <- exp(llower) / (1 + exp(llower))   # Lower ci
    upper <- exp(lupper) / (1 + exp(lupper))   # Upper ci
  }

  out <- data.frame(                           # Output data frame
    variable = name,
    level = levels,
    non_miss = tab,
    percent = round(100 * prop, digits = digits),
    lcl = round(100 * lower, digits = digits),
    ucl = round(100 * upper, digits = digits))
  rownames(out) <- 1:nrow(out)                 # Sequentially number rows
  return(out)
}