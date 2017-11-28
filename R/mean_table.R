#' @title Estimate Percents and 95 Percent Confidence Intervals in dplyr Pipelines
#'
#' @description The mean_table function produces overall and grouped
#'   tables of means with related statistics. In addition to means, the
#'   mean_table missing/non-missing frequencies, the standared error of the
#'   mean (sem), and 95% confidence intervals for the mean(s). For grouped
#'   tibbles, mean_table displays these statistics for each category of the
#'   group_by variable.
#'
#' @param .data A tibble or grouped tibble.
#'
#' @param x The continuous response variable for which the statistics are
#'   desired.
#'
#' @param t_prob (1 - alpha / 2). Default value is 0.975, which corresponds to
#'   an alpha of 0.05. Used to calculate a critical value from Student's t
#'   distribution with n - 1 degrees of freedom.
#'
#' @param output Options for this parameter are "default" and "all".
#'
#'   Default output includes the n, mean, and 95% confidence interval for the
#'   mean. Using output = "all" also returns the standard error of the number
#'   of missing values for x, the critical t-value, and the standard error of
#'   the mean.
#'
#' @param digits Round mean, lcl, and ucl to digits. Default is 2.
#'
#' @param ... Other parameters to be passed on.
#'
#' @return A tibble of class "mean_table" or "mean_table_grouped"
#' @export
#'
#' @references
#'   SAS documentation: http://support.sas.com/documentation/cdl/en/proc/65145/HTML/default/viewer.htm#p0klmrp4k89pz0n1p72t0clpavyx.htm
#'
#' @examples
#' library(tidyverse)
#' library(bfuncs)
#'
#' data(mtcars)
#'
#' # Overall mean table with defaults
#'
#' mtcars %>%
#'   mean_table(mpg)
#'
#' #> # A tibble: 1 x 5
#' #>     var     n  mean   lcl   ucl
#' #>   <chr> <int> <dbl> <dbl> <dbl>
#' #> 1   mpg    32 20.09 17.92 22.26
#'
#' # Grouped means table with defaults
#'
#' mtcars %>%
#'   group_by(cyl) %>%
#'   mean_table(mpg)
#'
#' #> # A tibble: 3 x 6
#' #>     cyl   var     n  mean   lcl   ucl
#' #>   <dbl> <chr> <int> <dbl> <dbl> <dbl>
#' #> 1     4   mpg    11 26.66 23.63 29.69
#' #> 2     6   mpg     7 19.74 18.40 21.09
#' #> 3     8   mpg    14 15.10 13.62 16.58

mean_table <- function(.data, x, t_prob = 0.975, output = "default", digits = 2, ...) {

  # ===========================================================================
  # Enquo the x argument so that it can be used in the dplyr pipeline below.
  # ===========================================================================
  response_var <- enquo(x)


  # ===========================================================================
  # Quick data checks
  # ===========================================================================
  if (!("data.frame" %in% class(.data))) {
    message("Expecting the class of .data to include data.frame. Instead, the ",
         "class was ", class(.data))
  }

  if (missing(x)) {
    stop("No argument was passed to the 'x' parameter. Expecting 'x' to be a ",
         "numeric column.")

  }

  # ===========================================================================
  # One-way table of means and related stats
  # Just "works" if grouped_df too
  # ===========================================================================
  out <- .data %>%
    filter(!is.na(!!response_var)) %>%                         # Drop missing
    summarise(
      var    = quo_name(response_var),                         # Grab variable (x) name
      n_miss = is.na(.data[[quo_name(response_var)]]) %>% sum, # Count missing from before drop
      n      = n(),
      mean   = mean(!!response_var),
      t_crit = qt(t_prob, n - 1),
      sem    = sd(!!response_var) / sqrt(n),
      lcl    = mean - t_crit * sem,
      ucl    = mean + t_crit * sem,
      mean   = round(mean, digits),                            # Round mean
      lcl    = round(lcl, digits),                             # Round lcl
      ucl    = round(ucl, digits)                              # Round ucl
    ) %>%
    as.tibble()

  # ===========================================================================
  # Classes of output
  # If the input data frame (.data) was a grouped data frame, then the output
  # will be a bivariate analysis of means ("mean_table_grouped"). Pass that
  # information on to out. It can be used later in format_table.
  # Otherwise the output will be a univariate analysis of means ("mean_table")
  # That class will also be used later in format_table.
  # ===========================================================================
  if ("grouped_df" %in% class(.data)) {
    class(out) <- c("mean_table_grouped", class(out))
  } else {
    class(out) <- c("mean_table", class(out))
  }

  # Control output
  # Typically, I only want the frequency, mean, and 95% CI
  # Make that the default
  if (output == "default" && class(out) == "mean_table") {
    out <- out %>%
      select(var, n, mean, lcl, ucl)

  } else if (output == "default" && class(out) == "mean_table_grouped") {
    out <- out %>%
      select(1, var, n, mean, lcl, ucl)
  } else {
    out <- out # do nothing
  }

  # Return tibble of results
  out
}
