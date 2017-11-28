#' @title Format freq_table and mean_table Output for Publication and Dissemination
#'
#' @description The format_table function is an S3 generic. It currently has
#'   methods for formatting the output of the freq_table and mean_table
#'   functions. For example, a mean and 95% confidence interval would be
#'   formatted as 24.00 (21.00 - 27.00) by default.
#'
#' @param .data A data frame of an accepted class.
#'
#' @param ... Other parameters to be passed on.
#'
#' @param digits Determines the number of decimal place to display. Passed to
#'   the "nsmall =" parameter of the format function.
#'
#'   Note: Changing the digits argument to format_table will change the number
#'   of digits displayed, but does not change the underlying rounding of the
#'   value. That must be changed in the digits argument to mean_table or
#'   freq_table.
#'
#' @param stats Options for this parameter are "mean and ci" (default) and
#'   "n and mean". They control which formatted statistics are returned.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(bfuncs)
#'
#' data(mtcars)
#'
#' # Overall mean table
#'
#' mtcars %>%
#'   mean_table(mpg) %>%
#'   format_table()
#'
#' #> # A tibble: 1 x 2
#' #>     var               mean_95
#' #>   <chr>                 <chr>
#' #> 1   mpg 20.09 (17.92 - 22.26)
#'
#' # Grouped means table
#'
#' mtcars %>%
#'   group_by(cyl) %>%
#'   mean_table(mpg) %>%
#'   format_table()
#'
#' #> # A tibble: 3 x 3
#' #>     cyl   var               mean_95
#' #>   <dbl> <chr>                 <chr>
#' #> 1     4   mpg 26.66 (23.63 - 29.69)
#' #> 2     6   mpg 19.74 (18.40 - 21.09)
#' #> 3     8   mpg 15.10 (13.62 - 16.58)

# =============================================================================
# S3 Generic function
# =============================================================================
format_table <- function(.data, ...) {
  UseMethod("format_table")
}




# =============================================================================
# Method for class mean_table
# Overall means
# =============================================================================
#' @inheritParams format_table
#' @export
#' @rdname format_table
format_table.mean_table <- function(.data, digits = 2, stats = "mean and ci", ...) {

  # Format statistics
  out <- .data %>%
    mutate(
      mean    = format(mean, nsmall = digits),
      lcl     = format(lcl,  nsmall = digits),
      ucl     = format(ucl,  nsmall = digits),
      mean_95 = paste0(mean, " (", lcl, " - ", ucl, ")"),
      n_mean  = paste0(n, " (", mean, ")")
    )

  # Control output
  if (stats == "mean and ci") {
    out <- out %>%
      select(var, mean_95)

  } else if (stats == "n and mean") {
    out <- out %>%
      select(var, n_mean )
  }

  # Return result
  out
}




# =============================================================================
# Method for class mean_table_grouped
# Grouped means
# =============================================================================
#' @inheritParams format_table
#' @export
#' @rdname format_table
format_table.mean_table_grouped <- function(.data, digits = 2, stats = "mean and ci", ...) {

  # Format statistics
  out <- .data %>%
    mutate(
      mean    = format(mean, nsmall = digits),
      lcl     = format(lcl,  nsmall = digits),
      ucl     = format(ucl,  nsmall = digits),
      mean_95 = paste0(mean, " (", lcl, " - ", ucl, ")"),
      n_mean  = paste0(n, " (", mean, ")")
    )

  # Control output
  if (stats == "mean and ci") {
    out <- out %>%
      select(1, var, mean_95)

  } else if (stats == "n and mean") {
    out <- out %>%
      select(1, var, n_mean )
  }

  # Return result
  out
}




# =============================================================================
# Method for class freq_table_one_way
# One-way frequency tables
# =============================================================================

