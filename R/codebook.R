#' Automate creation of a data codebook
#'
#' The codebook function assists with the creation of a codebook for a given
#'  data frame.
#'
#' @param x A data frame
#' @param label Optional vector of variable labels. Default is NA.
#'
#' @return A list of lists
#' @export
#'
#' @examples
#' data("mtcars")
#'
#' # Optional: Create label vector
#' labels <- c("Miles Per Gallon", "Number of cylinders")
#'
#' # Codebook with one variable
#' codebook(mtcars["mpg"], label = labels)
#'
#' # Codebook with multiple variables
#' varlist <- c("mpg", "cyl")
#' codebook(mtcars[varlist], label = labels)
#'
#' # Codebook with all variables
#' codebook(mtcars)
#'
#' # Do not use:
#' # lapply(mtcars[varlist], codebook)
codebook <- function(x, label = NA){
  if (!is.data.frame(x)){
    stop("x must be a data frame")
  }

  i <- 1
  for (vars in x) {
    cat("--------------------------------------------------------------------------------------------------------------", "\n")
    cat(names(x)[i], "\n")
    cat("--------------------------------------------------------------------------------------------------------------", "\n")
    cat("Label:", label[i], "\n")
    cat("Class:", class(vars), "\n")
    cat("Unique:", length(unique(vars)), "\n")
    cat("Miss:", sum(is.na(vars)), "\n")
    cat("Summary: \n")
    print(summary(vars))
    cat("\n")
    cat("\n")
    i <- i + 1
  }
}

