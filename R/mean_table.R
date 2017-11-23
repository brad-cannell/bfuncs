#' @title Estimate Percents and 95 Percent Confidence Intervals  in dplyr Pipelines
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
#'
#' data(mtcars)
#'
#' # Overall mean table
#' mtcars %>%
#'   mean_table(mpg)
#'
#' #> # A tibble: 1 x 8
#' #>     var n_miss     n     mean   t_crit      sem      lcl      ucl
#' #>   <chr>  <int> <int>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#' #> 1   mpg      0    32 20.09062 2.039513 1.065424 17.91768 22.26357
#'
#' # Grouped means table
#' mtcars %>%
#'   group_by(cyl) %>%
#'   mean_table(mpg)
#'
#' #> # A tibble: 3 x 9
#' #>     cyl   var n_miss     n     mean   t_crit       sem      lcl      ucl
#' #>   <dbl> <chr>  <int> <int>    <dbl>    <dbl>     <dbl>    <dbl>    <dbl>
#' #> 1     4   mpg      0    11 26.66364 2.228139 1.3597642 23.63389 29.69338
#' #> 2     6   mpg      0     7 19.74286 2.446912 0.5493967 18.39853 21.08718
#' #> 3     8   mpg      0    14 15.10000 2.160369 0.6842016 13.62187 16.57813
mean_table <- function(.data, x, ...) {

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
      mean   = mean(mpg),
      t_crit = qt(0.975, n - 1),
      sem    = sd(mpg) / sqrt(n),
      lcl    = mean - t_crit * sem,
      ucl    = mean + t_crit * sem
    )

  # Return tibble of results
  out
}
