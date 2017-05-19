#' Quick Look at Categorical Variables
#'
#' Sometimes when cleaning data, we want to see all the levels that exist for
#' all categorical variables.
#'
#' @param x A data frame.
#'
#' @return A list of frequency tables for all categorical variables in x.
#' @export
#'
#' @examples
#' # Load mtcars
#' data(mtcars)
#'
#' # Check a single categorical variable
#' check_catvars(mtcars$cyl)
#'
#' # Check every variable in a data frame
#' lapply(mtcars, check_catvars)
check_catvars <- function(x) {
  if (length(unique(x)) >= 20) {
    "Not a categorical variable"
  } else {
    table(x)
  }
}
