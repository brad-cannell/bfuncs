# This was created in DETECT 5-week project. I think it can be more broadly useful, but I don't have time to add
# it today.

widen_columns <- function(.data, ...) {
  col <- dplyr::enquos(...)

  for (i in seq_along(col)){
    .data <- .data %>%
      dplyr::mutate(temp = paste(quo_name(col[[i]]), match(!!col[[i]], unique(!!col[[i]])), sep = "_")) %>%
      tidyr::spread(key = temp, value = !!col[[i]])
  }
  .data
}

df %>%
  group_by(id) %>%
  widen_columns(meds, condition)
