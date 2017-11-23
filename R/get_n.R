#' @title Formatted Group Sample Size for Tables
#'
#' @description Given a tibble and a filter expression, get_n returns
#'   the group sample size formatted as "N = XXXX". Made to work in a dplyr
#'   pipeline, and used when creating tables for publications / reports.
#'
#' @param .data A data frame or tibble
#' @param ... A dplyr::filter expression. Used to select subgroup.
#'
#' @return A character string
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(bfuncs)
#'
#' data(mtcars)
#'
#' # Get sample size for cars with 4 cylinders
#' mtcars %>% get_n(cyl == 4)
#'
#' #> [1] "N = 11"
get_n <- function(.data, ...) {
  filter_exp <- quos(...)

  .data %>%
    filter(!!!filter_exp) %>%
    summarise(n = n()) %>%
    mutate(n = paste0("N = ", format(n, big.mark = ","))) %>%
    pull(n)
}
