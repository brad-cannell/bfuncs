#' Recode Values to NA
#'
#' @param x A vector.
#' @param values The values that you want to recode to NA.
#'
#' @return A vector with desired values recoded to NA.
#' @export
#'
#' @examples
#' # Example data set:
#' (df <- data.frame(
#'   edu = c(1, 2, 4, 7, 4),
#'   marital = c(1, 1, 7, 9, 3),
#'   race = c(1, 1, 3, 2, NA),
#'   month = c("May", "Feb", "Sep", "UNK", "?"),
#'   stringsAsFactors = FALSE
#' ))
#'
#' # Recode a single numeric variable
#' recode_miss(df$marital, values = c(7, 9))
#'
#' # Recode a single character variable
#' recode_miss(df$month, values = c("UNK", "?"))
#'
#' # Recode multiple variables
#' lapply(df, recode_miss, values = c(7, 9, "UNK", "?"))
recode_miss <- function(x, values = NA) {
  for (val in values) {
    x[x == val] <- NA
  }
  return(x)
}





























