#' Find rows that contain only NA values
#'
#' Given a data frame, this function returns the row number(s) where the value
#'  for every variable(s) in the row(s) is/are NA.
#'
#' @param x A data frame
#'
#' @return A vector of row numbers
#' @export
#'
#' @examples
#' df <- data.frame(
#' var1 = c(1, NA, 2, NA, 5, NA),
#' var2 = c(NA, "b", "c", NA, NA, NA),
#' var3 = c(TRUE, FALSE, TRUE, NA, FALSE, NA)
#' )
#'
#' all_na(df)
all_na <- function(x) {
  out <- list()
  for (i in seq_along(x)){
    out[[i]] <- which(is.na(x[[i]]))
  }
  out
  Reduce(intersect, out)
}
