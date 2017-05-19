#' Mean Center Numeric Variables
#'
#' Function to mean center predictor variables.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing x minus the mean of x. Note that NA's are ignored.
#' @export
#'
#' @examples
#' # load mtcars data
#' data(mtcars)
#'
#' # Mean center a single numeric variable
#' (mtcars$c_mpg <- mean_center(mtcars$mpg))
#'
#' # Mean center multiple numeric variables
#' centered <- do.call(cbind, lapply(mtcars[c("mpg", "hp")], mean_center))
#' colnames(centered) <- c("c_mpg", "c_hp")
#' mtcars2 <- cbind(mtcars, centered)

mean_center <- function(x) {

  if (!is.numeric(x)){
    stop("x must be numeric")
  }

  centered_var <- x - mean(x, na.rm = T)
  return(centered_var)
}