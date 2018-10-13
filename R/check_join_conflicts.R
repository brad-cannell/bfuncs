#' @title Compare Values of Non-Joined Duplicate Variables After Joining Data
#' Frames
#'
#' @description The data frame that results from joining two data frames using
#' dplyr::*_join functions sometimes contains non-joined duplicate variables. For
#' example, df1 and df2 may have each had a variable named first_name. If the
#' user does not include first_name in the dplyr::*_join function, then
#' the resulting joined data frame will include two fist name variables --
#' first_name.x and first_name.y by default. Typically, the user will expect
#' the values of first_name.x and first_name.y to match. However, that isn't
#' always the case. The check_join_conflicts function checks for values that
#' don't match.
#'
#' @param .data The joined data frame -- resulting from a dplyr::*_join function.
#' @param suffix The suffix disambiguates non-joined duplicate variables. The
#' default is x and y.
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' df1 <- tibble::tribble(
#' ~id, ~first_name, ~gender,
#' 1,   "john",      "m",
#' 2,   "jane",      "f",
#' 3,   "sally",     "f"
#' )
#'
#' df2 <- tibble::tribble(
#'   ~id, ~first_name, ~gender,
#'   1,   "jon",       "m",
#'   2,   "jane",      "f",
#'   3,   "salle",     "f"
#' )
#'
#' df3 <- dplyr::full_join(df1, df2, by = "id")
#' df3
#'
#' #>  A tibble: 3 x 5
#' #>     id first_name.x gender.x first_name.y gender.y
#' #>  <dbl> <chr>        <chr>    <chr>        <chr>
#' #>      1 john         m        jon          m
#' #>      2 jane         f        jane         f
#' #>      3 sally        f        salle        f
#'
#' check_join_conflicts(df3)
#'
#' #>  A tibble: 3 x 4
#' #>  variable     row .x    .y
#' #>  <chr>      <int> <chr> <chr>
#' #>  first_name     1 john  jon
#' #>  first_name     3 sally salle
#' #>  gender        NA NA    NA
#'
#' # Example with different suffix names
#'
#' df4 <- df3
#' names(df4) <- stringr::str_replace_all(names(df4), "\\.x", ".medstar")
#' names(df4) <- stringr::str_replace_all(names(df4), "\\.y", ".aps")
#'
#' check_join_conflicts(df4, suffix = c("medstar", "aps"))
#' #>  A tibble: 3 x 4
#' #>  variable     row .medstar .aps
#' #>  <chr>      <int> <chr>    <chr>
#' #>  first_name     1 john     jon
#' #>  first_name     3 sally    salle
#' #>  gender        NA NA       NA
check_join_conflicts <- function(.data, suffix = c("x", "y")) {

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ------------------------------------------------------------------
  temp = NULL

  # ===========================================================================
  # Check for two unique suffix values
  # ===========================================================================
  check_length <- unique(suffix) %>% length()
  if (check_length != 2) {
    stop(
      "Expecting two unique values for suffix. Current values are: ",
      paste(suffix, collapse = ", ")
    )
  }

  # ===========================================================================
  # Create shell for data frame of results
  # ===========================================================================
  out <- tibble::tibble(
    variable = vector(mode = "character"),
    row      = vector(mode = "integer"),
    .x       = vector(mode = "character"),
    .y       = vector(mode = "character")
  )


  # ===========================================================================
  # Convert all columns of .data to character
  # Just prevents problems
  # ===========================================================================
  .data <- .data %>% dplyr::mutate_all(.funs = as.character)


  # ===========================================================================
  # Find all variables with a suffix
  # ===========================================================================
  var_names_to_check <- names(.data)
  suffix_pattern_1   <- paste0("\\.", suffix[1], "$")
  suffix_pattern_2   <- paste0("\\.", suffix[2], "$")
  suffix_1_index     <- stringr::str_detect(var_names_to_check, suffix_pattern_1)
  suffix_2_index     <- stringr::str_detect(var_names_to_check, suffix_pattern_2)
  keep_index         <- as.logical(suffix_1_index + suffix_2_index)
  vars_w_suffix      <- var_names_to_check[keep_index]

  # ------------
  # Error check
  # ------------
  if (sum(suffix_1_index) == 0) {
    message("There are no variables in .data with a suffix of: .", suffix[1])
  }
  if (sum(suffix_2_index) == 0) {
    message("There are no variables in .data with a suffix of: .", suffix[2])
  }

  # Make a short list of variables for use later
  var_list <- stringr::str_replace(vars_w_suffix, suffix_pattern_1, "")
  var_list <- stringr::str_replace(var_list, suffix_pattern_2, "")
  var_list <- unique(var_list)

  # ===========================================================================
  # Fill in data frame of results
  # ===========================================================================
  for (i in seq_along(var_list)) {

    # At this point there is a list of all variables that have the suffix
    # of interest. For each pair, check for conflicts
    compare_index  <- stringr::str_detect(vars_w_suffix, var_list[[i]])
    compare        <- vars_w_suffix[compare_index]
    join_conflicts <- !(.data[[compare[1]]] == .data[[compare[2]]])

    # Get variable name, row, and conflicting values for variable i
    # Just get variable name and NA's if there are no conflicts
    conflict_rows      <- which(join_conflicts)
    conflict_row_count <- length(conflict_rows)
    x_values           <- .data[[compare[1]]][conflict_rows]
    y_values           <- .data[[compare[2]]][conflict_rows]

    if (conflict_row_count == 0) {
      conflict_rows <- NA_integer_
      x_values <- NA_character_
      y_values <- NA_character_
    }

    temp <- tibble::tibble(
      variable = rep(var_list[[i]], max(1, conflict_row_count)), # Have to add at least once
      row = conflict_rows,
      .x = x_values,
      .y = y_values
    )

    # Bind values for variable i to results data frame
    out <- dplyr::bind_rows(out, temp)
  }

  # Make sure shell data frame variable names match suffix names
  if (suffix[1] != "x") {
    names(out)[names(out) == ".x"] <- paste0(".", suffix[1])
  }
  if (suffix[2] != "y") {
    names(out)[names(out) == ".y"] <- paste0(".", suffix[2])
  }

  # ===========================================================================
  # Return data frame of results
  # ===========================================================================
  out
}
