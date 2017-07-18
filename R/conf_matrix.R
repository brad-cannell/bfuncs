#' @title Confusion Matrix
#'
#' @description Create a confusion matrix along with the following measures
#'  of performance
#'
#' @param truth A vector containing actual or "gold standard" outcomes.
#' @param prediction A vector containing predicted values or observed
#'  diagnostic values.
#' @param show_matrix A logical (TRUE or FALSE). Set to TRUE by default. Change
#'  to FALSE if you want a confusion matrix table to be printed to the console.
#'
#' @return A list containing:
#'  \describe{
#'    \item{Sensativity}{(also called the true positive rate, or the recall
#'      in some fields) measures the proportion of positives that are
#'      correctly classified as such (e.g., the percentage of sick people who
#'      are correctly classified as having the condition).}
#'    \item{Specificity}{(also called the true negative rate) measures the
#'      proportion of negatives that are correctly identified as such (e.g.,
#'      the percentage of healthy people who are correctly identified as not
#'      having the condition).}
#'    \item{FPR}{False Positive Rate (Not really a rate). Measures the
#'      proportion of negatives that are incorrectly classified as a positive.
#'      1 - specificity.}
#'    \item{FNR}{False Negative Rate (Not really a rate). Measures the
#'      proportion of positives that are incorrectly classified as a negative.
#'      1 – sensitivity.}
#'    \item{FDR}{False Discovery Rate. Measures the proportion of cases
#'      predicted positive, that were truly negative.}
#'    \item{Accuracy}{The overall proportion of correctly classified
#'      observations.}
#'    \item{Misclassification}{The overall proportion of incorrectly classified
#'      observations.}
#'    \item{Precision}{The overall proportion of incorrectly classified
#'      observations.}
#'    \item{Prevalence}{The overall proportion of true positives.}
#'  }
#'
#' @references Wikipedia - Sensativity and Specificity
#'  \url{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}
#'
#' @export
#'
#' @examples
#' data("mtcars")
#'
#' # Create "true" outcome vector
#' truth <- as.integer(mtcars$vs)
#'
#' # Create a vector meant to represent a set of predicted outcomes
#' set.seed(123)
#' predicted <- sample(0:1, length(truth), replace = TRUE)
#'
#' # Create confusion matrix
#' my_cm <- conf_matrix(truth = truth, prediction = predicted)
#' my_cm
#'
#' # --------------------------------------------------------------------------
#' # Example of running conf_matrix over multiple variables
#' df <- data.frame(
#' x1 = factor(c("No", "Yes")),
#' x2 = factor(c("No", "Yes")),
#' x3 = factor(c("Yes", "No"))
#' )
#' # Calculate measures
#' test <- conf_matrix(truth = df$x1, prediction = df$x2, show_matrix = FALSE)
#' as.data.frame(test)
#'
#' # Make empty table
#' results <- data.frame(
#'   var = NA,
#'   tp = NA,
#'   fp = NA,
#'   fn = NA,
#'   tn = NA,
#'   sensativity = NA,
#'   specificity = NA,
#'   fpr = NA,
#'   fnr = NA,
#'   accuracy = NA,
#'   misclassification = NA,
#'   precision = NA,
#'   prevalence = NA
#' )
#'
#' # for each screener item:
#' #   capture the name of the screener item
#' #   put the name into the first column of the results df
#' #   capture the performance measures of the screener item
#' #   put each performance measure into the coresponding element in the results df
#' var <- names(df[2:3])
#' r <- 1
#' for (i in var) {
#'   results[r, 1] <- i
#'   results[r, 2:13] <- conf_matrix(truth = df$x1, prediction = df[[i]], show_matrix = FALSE)
#'   r <- r + 1
#' }
conf_matrix <- function(truth, prediction, show_matrix = TRUE) {
  # Test to make sure that inputs are of the same class
  if (class(truth) != class(prediction)) {
    stop(paste0("Truth and prediction must have the same class. Truth is ",
                class(truth), " and prediction is ", class(prediction), "."),
         call. = FALSE)
  }

  # Test to make sure that truth has two levels
  # Change this to accomidate multiple classes
  # Delete everything down to checking for matching values
  # Could do this now, but not sure how to calculate stats of interest for
  # vars with more than 2 levels
  if (class(truth) == "factor") {
    val_t <- levels(truth)
    if (length(val_t) != 2) {
      stop(paste0("Truth must have 2 levels. It has ", length(val_t),
                  "."), call. = FALSE)
    }
  } else {
    val_t <- unique(truth)
    if (length(val_t) != 2) {
      stop(paste0("Truth must have 2 unique values. It has ",
                  length(val_t), "."), call. = FALSE)
    }
  }

  # Test to make sure that prediction has two levels
  if (class(prediction) == "factor") {
    val_p <- levels(prediction)
    if (length(val_p) != 2) {
      stop(paste0("Prediction must have 2 levels. It has ",
                  length(val_p), "."), call. = FALSE)
    }
  } else {
    val_p <- unique(prediction)
    if (length(val_p) != 2) {
      stop(paste0("Prediction must have 2 unique values. It has ",
                  length(val_p), "."), call. = FALSE)
    }
  }

  # Test that the levels for truth and prediction match
  if (sum(val_t %in% val_p) != 2) {
    stop("The value list for truth does not match the value list for prediction.",
         call. = FALSE)
  }

  t <- table(truth, prediction)   # Tabulte actual vs predicted
  t <- t[2:1, 2:1]                # Reorder table
  tp <- t[1]
  fp <- t[2]
  fn <- t[3]
  tn <- t[4]

  colnames(t) <- c("P1", "P0")    # Rename columns
  rownames(t) <- c("T1", "T0")    # Rename rows

  # Create a list containing performance measures
  measures <- list(
    tp                = tp,
    fp                = fp,
    fn                = fn,
    tn                = tn,
    Sensativity       = round(tp / (tp + fn), 2),
    Specificity       = round(tn / (tn + fp), 2),
    FPR               = round(fp / (tn + fp), 2),
    FNR               = round(fn / (fn + tp), 2),
    FDR               = round(fp / (tp + fp), 2),
    Accuracy          = round((tp + tn) / (tp + fp + tn + fn), 2),
    Misclassification = round((fp + fn) / (tp + fp + tn + fn), 2),
    Precision         = round(tp / (tp + fp), 2),
    Prevalence        = round((tp + fn) / (tp + fp + tn + fn), 2)
  )
  # By default, CrossTable is printed. When that happens, we still want to
  # store tp, fp, fn, and tn, but it's redundant to print them out.
  # If we don't show the CrossTable, then we do want to print out tp-fp.
  # To save screen space, print measures as a data frame.
  if (show_matrix == TRUE) {
    gmodels::CrossTable(t)        # Print confusion matrix
    print(as.data.frame(measures[5:12]))
  } else {
    print(as.data.frame(measures))
  }
}


## Still need to update unit tests:
# Test that should work, works
# tlf <- factor(detect$cg_deceptive46, levels = c("No", "Yes"))
# conf_matrix(detect$any_valid_f, tlf)
# Works

# Test different classes for error
# conf_matrix(detect$any_valid, tlf)
# Correct error

# Test that truth has 2 levels (factor)
# xf <- factor(rep(c("No", "Yes", "Don't Know", "Not Sure"), 10))
# conf_matrix(xf, tlf)
# Correct error

# Test that truth has 2 unique values (not factor)
# xc <- rep(c("No", "Yes", "Don't Know", "Not Sure"), 10)
# tlc <- rep(c("No", "Yes"), 20)
# conf_matrix(xc, tlc)
# Correct error

# Test that prediction has 2 levels
# conf_matrix(detect$any_valid_f, detect$cg_deceptive46)
# Correct error

# Test that prediction has 2 unique values (not factor)
# conf_matrix(tlc, xc)
# Correct error

# Test that truth and prediction have the same set of values
# x1 <- factor(c("No", "Yes"))
# x2 <- factor(c("No", "Yes"))
# x3 <- factor(c("Yes", "No"))
# x4 <- factor(c("foo", "bar"))
# conf_matrix(x1, x2)
# Works
# conf_matrix(x1, x3)
# Works
# conf_matrix(x1, x4)
# Correct error

# Test turn off show matrix
# conf_matrix(detect$any_valid_f, tlf, show_matrix = FALSE)
# Works
