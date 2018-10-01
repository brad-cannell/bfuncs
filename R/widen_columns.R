#' @title Create Multiple Sequentially Numbered Columns Containing A Single
#' Value From A Single Column With Multiple Values
#'
#' @description Widen columns is intended to be used in a dplyr pipeline. Pass
#' one or more columns containing discreet values to the widen columns function.
#' Widen columns return a data frame with multiple sequentially numbered columns
#' containing a single value for each value of each of the columns passed in.
#' The names of the new sequentially numbered columns will be prefixed with the
#' original column name and an underscore.
#'
#' Typically,  this function will be used on a grouped data frame.
#'
#' @param .data a data frame (typically a grouped data frame)
#' @param ... a comma separated list of column names to widen
#'
#' @return a data frame
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' df <- tibble(
#'   id = c(1, 1, 1, 1, 2),
#'   first = c("john", "john", "john", "john", "jane"),
#'   last = c("doe", "doe", "doe", "doe", "smith"),
#'   condition = c("diabetes", "diabetes", "mi", "mi", "depression"),
#'   meds = c("asprin", "tylenol", "asprin", "tylenol", "asprin")
#' ) %>%
#'   print()
#'
#' #>  # A tibble: 5 x 5
#' #>       id first last  condition  meds
#' #>    <dbl> <chr> <chr> <chr>      <chr>
#' #>  1     1 john  doe   diabetes   asprin
#' #>  2     1 john  doe   diabetes   tylenol
#' #>  3     1 john  doe   mi         asprin
#' #>  4     1 john  doe   mi         tylenol
#' #>  5     2 jane  smith depression asprin
#'
#' df %>%
#'   group_by(id) %>%
#'   widen_columns(meds, condition)
#'
#' #>  # A tibble: 2 x 7
#' #>  # Groups:   id [2]
#' #>       id first last  meds_1 meds_2  condition_1 condition_2
#' #>    <dbl> <chr> <chr> <chr>  <chr>   <chr>       <chr>
#' #>  1     1 john  doe   asprin tylenol diabetes    mi
#' #>  2     2 jane  smith asprin NA      depression  NA
widen_columns <- function(.data, ...) {

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ------------------------------------------------------------------
  temp = NULL

  # Create quotures
  col <- dplyr::enquos(...)

  # Widen each column
  for (i in seq_along(col)){
    .data <- .data %>%
      dplyr::mutate(temp = paste(dplyr::quo_name(col[[i]]), match(!!col[[i]], unique(!!col[[i]])), sep = "_")) %>%
      tidyr::spread(key = temp, value = !!col[[i]])
  }

  # Return modified data frame
  .data
}
