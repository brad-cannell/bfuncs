#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::interfaces(r, cpp)]]

//' Numeric Last Observation Carried Forward
//'
//' @param x a numeric vector
//' @export
// [[Rcpp::export]]
NumericVector na_locf(NumericVector x) {

  // Initialize stashed_val as NA
  double stashed_val = NumericVector::get_na();

  // Initialize result with same values as x
  NumericVector result = x;

  // Loop over each value in x
  // Check to see if the i'th value of x is missing.
  // If so, set the i'th value of result to the stashed value
  // If not, do nothing to result. Update the stashed value to the i'th value
  // of x
  for(int i = 0; i < x.size(); i++) {
    if(NumericVector::is_na(x[i])) {
      result[i] = stashed_val;
    } else {
      stashed_val = x[i];
    }
  }

  //  Return numeric vector with locf
  return result;
}

/*** R
# REALLY simple example
x <- c(1, NA, 3)
x

# Run na_locf(x)
na_locf(x)

# library(microbenchmark)
set.seed(42)
x <- rnorm(1e5)

# Sprinkle some NA into x
x[sample(1e5, 100)] <- NA

microbenchmark::microbenchmark(
  zoo::na.locf(x),
  na_locf(x),
  times = 5
)
*/
