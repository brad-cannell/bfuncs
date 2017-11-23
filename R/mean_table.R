#' @title Estimate Percents and 95 Percent Confidence Intervals in dplyr Pipelines
#'
#' @description The mean_table function produces overall and grouped
#'   tables of means with related statistics. In addition to means, the
#'   mean_table missing/non-missing frequencies, the standared error of the
#'   mean (sem), and 95% confidence intervals for the mean(s). For grouped
#'   tibbles, mean_table displays these statistics for each category of the
#'   group_by variable.
#'
#' @param .data A tibble or grouped tibble
#' @param x The continuous response variable for which the statistics are
#'   desired.
#' @param digits Round mean, lcl, and ucl to `digits`. Default is 2.
#' @param ... Other parameters to be passed on.
#'
#' @return A tibble
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
#' # Overall mean table
#' mtcars %>%
#'   mean_table(mpg)
#'
#' #> # A tibble: 1 x 8
#' #>     var n_miss     n  mean   t_crit      sem   lcl   ucl
#' #>   <chr>  <int> <int> <dbl>    <dbl>    <dbl> <dbl> <dbl>
#' #> 1   mpg      0    32 20.09 2.039513 1.065424 17.92 22.26
#'
#' # Grouped means table
#' mtcars %>%
#'   group_by(cyl) %>%
#'   mean_table(mpg)
#'
#' #> # A tibble: 3 x 9
#' #>     cyl   var n_miss     n  mean   t_crit       sem   lcl   ucl
#' #>   <dbl> <chr>  <int> <int> <dbl>    <dbl>     <dbl> <dbl> <dbl>
#' #> 1     4   mpg      0    11 26.66 2.228139 1.3597642 23.63 29.69
#' #> 2     6   mpg      0     7 19.74 2.446912 0.5493967 18.40 21.09
#' #> 3     8   mpg      0    14 15.10 2.160369 0.6842016 13.62 16.58
mean_table <- function(.data, x, digits = 2, ...) {

  # ===========================================================================
  # Quick data checks
  # ===========================================================================
  if (!("data.frame" %in% class(.data))) {
    stop("Expecting the class of .data to include data.frame. Instead, the ",
         "class was ", class(.data))
  }

  # ===========================================================================
  # quos arguments to function that will be used below
  # ===========================================================================
  response_var <- enquo(x)

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
      t_crit = qt(0.975, n - 1),
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
  # If the input data frame (.data) was a grouped data frame, pass that
  # information on to out. It can be used later in format_table.
  # Add mean_table class no matter what. It will also be used later in
  # format_table
  # ===========================================================================
  class(out) <- append(class(out), "mean_table")

  if ("grouped_df" %in% class(.data)) {
    class(out) <- append(class(out), "grouped_df")
  }

  # Return tibble of results
  out
}
