#' @title Count the number of unique ID's
#'
#' @description This function returns a value representing the number of unique
#'   values for a given variable as a nicely formatted value. In input variable
#'   could be any variable, however, the intent is for it to be a variable that
#'   identifies each unit of observation in the data (ID variable).
#'
#' @param x A variable (vector). Typically an ID variable.
#' @param description A character string that describes the unit of observation
#'   in the return value (e.g., "women", "participants", "schools", "cities").
#'   The default is "values".
#' @param ... Additional arguments
#'
#' @return A character string
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   id = rep(1:5, 2),
#'   x = rnorm(10)
#' )
#'
#' count_ids(df$id)
count_ids <- function(x, description = "values", ...) {
  count <- format(length(unique(x)), big.mark = ",")
  cat(count, "unique", description)
}
