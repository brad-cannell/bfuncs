#' @title Format freq_table and mean_table for Publication Tables
#'
#' #' @description The format_table function takes the results of the
#'   function or the mean_table function and formats them for publication
#'   tables. For example, a mean and 95% confidence interval would be formatted
#'   as 24.00 (21.00 - 27.00) by default.
#'
#' @param .data A data frame of class mean_table.
#' @param digits Determines the number of decimal place to display. Passed to
#'   the "nsmall =" parameter of the format function.
#' @param ... Other parameters to be passed on.
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
format_table <- function(.data, digits = 2, ...) {

  # ===========================================================================
  # Quick data checks
  # ===========================================================================
  if (!("mean_table" %in% class(.data) | "freq_table" %in% class(.data))) {
    stop("Expecting the class of .data to include mean_table. Instead, the ",
         "class was ", class(.data))
  }

  # ===========================================================================
  # Perform operations of interest
  # Format mean, lcl, and ucl as character and ensure number of decimal places
  # shown. Then paste together as a formatted string.
  # ===========================================================================
  out <- .data %>%
    mutate(
      mean    = format(mean, nsmall = digits),
      lcl     = format(lcl,  nsmall = digits),
      ucl     = format(ucl,  nsmall = digits),
      mean_95 = paste0(mean, " (", lcl, " - ", ucl, ")")
    )

  # ===========================================================================
  # Check to see if .data is grouped
  # If so, we need to keep the group variable name too (column 1)
  # ===========================================================================
  if ("grouped_df" %in% class(.data)) {
    out <- out %>%
      select(1, var, mean_95)
  } else {
    out <- out %>%
      select(var, mean_95)
  }

  # Return tibble of results
  out
}
