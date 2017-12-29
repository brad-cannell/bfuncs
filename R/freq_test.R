#' @title Hypothesis Testing For Frequency Tables
#'
#' @description The freq_test function is an S3 generic. It currently has
#'   methods for conducting hypothesis tests on one-way and two-way frequency
#'   tables. Further, it is made to work in a dplyr pipeline with the
#'   freq_table function.
#'
#' @param x A tibble of class freq_table_one_way or freq_table_two_way.
#'
#' @param ... Other parameters to be passed on.
#'
#' @param method Options for this parameter control the method used to
#'   calculate p-values.
#'
#'   For the freq_table_two_way class, the options are "pearson" (default) -
#'   to use Pearson's chi-square test of independence, and "fisher" - to use
#'   Fisher's exact test.
#'
#' @return A tibble.
#'
#' @import magrittr
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(bfuncs)
#'
#' data(mtcars)
#'
#' # Test equality of proportions
#'
#' mtcars %>%
#'   group_by(am) %>%
#'   freq_table() %>%
#'   freq_test() %>%
#'   select(1:6, p_chi2_pearson)
#'
#' #>  # A tibble: 2 x 7
#' #>     am     n n_total percent   lcl   ucl p_chi2_pearson
#' #>  <dbl> <int>   <int>   <dbl> <dbl> <dbl>          <dbl>
#' #>  1   0    19      32   59.38 40.94 75.50      0.2888444
#' #>  2   1    13      32   40.62 24.50 59.06      0.2888444
#'
#' # Chi-square test of independence
#'
#' mtcars %>%
#'   group_by(am, vs) %>%
#'   freq_table() %>%
#'   freq_test() %>%
#'   select(1:8, p_chi2_pearson)
#'
#' #> # A tibble: 4 x 9
#' #>      am    vs     n n_row n_total percent_row lcl_row ucl_row p_chi2_pearson
#' #>   <dbl> <dbl> <int> <int>   <int>       <dbl>   <dbl>   <dbl>          <dbl>
#' #> 1     0     0    12    19      32       63.16   38.76   82.28      0.3409429
#' #> 2     0     1     7    19      32       36.84   17.72   61.24      0.3409429
#' #> 3     1     0     6    13      32       46.15   20.83   73.63      0.3409429
#' #> 4     1     1     7    13      32       53.85   26.37   79.17      0.3409429

# =============================================================================
# S3 Generic function
# =============================================================================
freq_test <- function(x, ...) {
  UseMethod("freq_test")
}




# =============================================================================
# Method for class freq_table_one_way
# Chi-square test for equal proportions
# =============================================================================
#' @inheritParams freq_test
#' @export
#' @rdname freq_test

freq_test.freq_table_one_way <- function(x, ...) {

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ------------------------------------------------------------------
  n = n_total = n_expected = chi2_contrib = pchisq = chi2_pearson = df = NULL

  # Check to make sure x is a freq_table_one_way
  # --------------------------------------------
  if (!("freq_table_one_way" %in% class(x))) {
    stop("x must be of class freq_table_one_way. It is currently: ", class(x))
  }

  # Calculate chi-square test of equality
  # Test whether population is equally distributed across categories of x
  # ---------------------------------------------------------------------
  out <- x %>%
    dplyr::mutate(
      n_expected     = n_total / nrow(x),
      chi2_contrib   = (n - n_expected)**2 / n_expected,
      chi2_pearson   = sum(chi2_contrib),
      df             = nrow(x) - 1,
      p_chi2_pearson = pchisq(chi2_pearson, df, lower.tail = FALSE)
    )

  # Add class to out that describes the information it contains
  # -----------------------------------------------------------
  class(out) <- c("pearson", "freq_table_one_way", class(out))

  # Return tibble of results
  out
}




# =============================================================================
# Method for class freq_table_two_way
# Pearson's Chi-square test for independence
# Fisher's exact test for independence
# =============================================================================
#' @inheritParams freq_test
#' @export
#' @rdname freq_test

freq_test.freq_table_two_way <- function(x, method = "pearson", ...) {

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ------------------------------------------------------------------
  n_row = n_col = n_total = n_expected = chi2_contrib = r = pchisq = NULL
  chi2_pearson = df = NULL

  # Check to make sure x is a freq_table_two_way
  # --------------------------------------------
  if (!("freq_table_two_way" %in% class(x))) {
    stop("x must be of class freq_table_two_way. It is currently: ", class(x))
  }

  # Grab name of row variable (first column from freq_table)
  # Grab name of column variable (second column from freq_table)
  # ------------------------------------------------------------
  row_var <- x %>% dplyr::select(1) %>% names() %>% rlang::sym()
  col_var <- x %>% dplyr::select(2) %>% names() %>% rlang::sym()

  # Calculate Pearson's Chi-square test
  # Test whether population is equally distributed across categories of x
  # ---------------------------------------------------------------------
  out <- x %>%
    dplyr::group_by(!! col_var) %>%
    dplyr::mutate(n_col = sum(n)) %>%  # Find marginal totals for "columns"
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_expected     = (n_row * n_col) / n_total,
      chi2_contrib   = (n - n_expected)**2 / n_expected,
      chi2_pearson   = sum(chi2_contrib),
      r              = unique(!! row_var) %>% length(),
      c              = unique(!! col_var) %>% length(),
      df             = (r -1) * (c - 1),
      p_chi2_pearson = pchisq(chi2_pearson, df, lower.tail = FALSE)
    )

  # Test for expected cell counts <= 5
  # ----------------------------------
  if ( min(out$n_expected) <= 5 ) {
    warning(paste0("One or more expected cell counts are <= 5. Pearson's ",
                   "Chi-square may not be a valid test."))

    # Add Fisher's Exact Test
    # -------------------
    if ("fisher" %in% method) {

      # Convert x to a matrix
      n  <- x[, 3] %>% unlist()
      mx <- matrix(n, nrow = 2, byrow = TRUE)

      # Use R's built-in fisher.test
      fisher <- stats::fisher.test(mx)

      # Add Fisher's p_value to out
      out <- out %>%
        dplyr::mutate(p_fisher = fisher$p.value)
    }
  }

  # Add class to out that describes the information it contains
  # -----------------------------------------------------------
  if ("fisher" %in% names(out)) {
    class(out) <- c("fisher", "freq_table_two_way", class(out))
  } else {
    class(out) <- c("pearson", "freq_table_two_way", class(out))
  }

  # Return tibble of results
  out
}
