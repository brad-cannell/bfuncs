#' Compute Summary Statistics for Categorical Variables with Few Categories
#'
#' @param df Data frame of interest
#' @param .x Column of interest
#' @param digits Number of digits after decimal to display
#'
#' @return A tibble
codebook_summary_stats_few_cats <- function(df, .x, digits = 2) {

  # ===========================================================================
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ===========================================================================
  var = NULL

  # ===========================================================================
  # Variable management
  # ===========================================================================
  x <- rlang::sym(.x)

  # ===========================================================================
  # Calculate measures of interest
  # ===========================================================================
  summary <- df %>%
    dplyr::group_by(!!x) %>%
    bfuncs::freq_table(digits = digits) %>%
    dplyr::mutate(cat = tidyr::replace_na(cat, "Missing")) %>%
    dplyr::select(-var)

  # ===========================================================================
  # Return tibble of results
  # ===========================================================================
  summary
}
