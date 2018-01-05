#' Returns the Number of Observations and Variables in a Data Frame
#'
#' Sometimes you will want to post documents (e.g., R markdown) in a public
#'  forum (e.g., GitHub), but you don't want to reveal any raw data values.
#'  In such a case, using str() or glimpse() may be problematic. about_data()
#'  is a simple function that returns the number of observations and variables
#'  in a data frame, but none of the data values.
#'
#' @param x A data frame
#' @param ... Additional arguments
#'
#' @return A character string
#' @export
#'
#' @examples
#' data("mtcars")
#' about_data(mtcars)
#' names(mtcars)
about_data <- function(x, ...) {

  # Make sure x is a data frame
  # ---------------------------
  if (!("data.frame" %in% class(x))) {
    stop("Expecting x to be a data frame. Instead, x had class: ",
         paste(class(x), collapse = " "))
  }

  # Format the output
  # -----------------
  out  <- paste(
    format(dim(x)[1], big.mark = ","), # formatted rows
    "observations and",
    format(dim(x)[2], big.mark = ","), # formatted cols
    "variables"
  )

  # Return out
  out
}
