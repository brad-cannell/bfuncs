#' Get Data Frame Attributes
#'
#' Codebook expects that `df ` is a data frame that you have read into memory
#' from a saved data file. Please provide the path to the saved data file. This
#' function gets selected attributes about file saved at `path` and stores
#' those attributes in a data frame, which is later turned into a flextable and
#' added to the codebook document.
#'
#' @param df The saved file at `path`, read into memory as a data frame
#' @param path The path to the saved dataset of interest
#'
#' @return A tibble of selected data frame attributes
codebook_get_df_attributes <- function(df, path = NA) {

  # ===========================================================================
  # Checks
  # ===========================================================================

  # Check to make sure df is a data frame
  if ( !("data.frame" %in% class(df)) ) {
    stop("Expecting df to be of class data.frame. Instead it was ", class(df))
  }

  # Check to make sure the user is not piping in the dataset name
  df_name <- deparse(substitute(df))
  if (df_name == ".") {
    message("The function get_df_attributes is seeing '.' as the df name. ",
            "This can be caused by piping df into the get_df_attributes fucntion.")
  }

  # Check for file path
  if (is.na(path)) {
    stop("Codebook expects that df is a data frame that you have read ",
         "into memory from a saved data file. Please provide the path ",
         "to the saved data file.")
  }

  # Check that file path is valid
  if (!file.exists(path)) {
    stop("The argument to 'path' is not a valid file path.")
  }

  # ===========================================================================
  # Copy code from utils:::format.object_size
  # Can't use ::: operator on CRAN
  # ===========================================================================
  format_object_size <- function (x, units = "b", standard = "auto", digits = 1L, ...)
  {
    known_bases <- c(legacy = 1024, IEC = 1024, SI = 1000)
    known_units <- list(SI = c("B", "kB", "MB", "GB", "TB", "PB",
      "EB", "ZB", "YB"), IEC = c("B", "KiB", "MiB", "GiB",
      "TiB", "PiB", "EiB", "ZiB", "YiB"), legacy = c("b", "Kb",
      "Mb", "Gb", "Tb", "Pb"), LEGACY = c("B", "KB", "MB",
      "GB", "TB", "PB"))
    units <- match.arg(units, c("auto", unique(unlist(known_units),
      use.names = FALSE)))
    standard <- match.arg(standard, c("auto", names(known_bases)))
    if (standard == "auto") {
      standard <- "legacy"
      if (units != "auto") {
        if (grepl("iB$", units))
          standard <- "IEC"
        else if (grepl("b$", units))
          standard <- "legacy"
        else if (units == "kB")
          stop("For SI units, specify 'standard = \"SI\"'")
      }
    }
    base <- known_bases[[standard]]
    units_map <- known_units[[standard]]
    if (units == "auto") {
      power <- if (x <= 0)
        0L
      else min(as.integer(log(x, base = base)), length(units_map) -
        1L)
    }
    else {
      power <- match(toupper(units), toupper(units_map)) -
        1L
      if (is.na(power))
        stop(gettextf("Unit \"%s\" is not part of standard \"%s\"",
                      sQuote(units), sQuote(standard)), domain = NA)
    }
    unit <- units_map[power + 1L]
    if (power == 0 && standard == "legacy")
      unit <- "bytes"
    paste(round(x/base^power, digits = digits), unit)
  }

  # ===========================================================================
  # Collect values of interest into a data frame
  # ===========================================================================
  meta <- tibble::tibble(
    `Dataset name:` = df_name,
    `Dataset size:` = df %>% utils::object.size() %>% format_object_size(units = "auto"),
    `Column count:` = df %>% ncol() %>% format(big.mark = ","),
    `Row count:` = df %>% nrow() %>% format(big.mark = ","),
    `Last modified date:` = file.mtime(path) %>% as.character()
  ) %>%
    tidyr::gather() # Reorient the data frame vertically

  # ===========================================================================
  # Return tibble of selected data frame attributes
  # ===========================================================================s
  meta
}
