#' Automate creation of a data codebook
#'
#' The codebook function assists with the creation of a codebook for a given
#'  data frame.
#'
#' Codebook expects that `df ` is a data frame that you have read into memory
#' from a saved data file. Please provide the path to the saved data file. This
#' function gets selected attributes about file saved at `path` and stores
#' those attributes in a data frame, which is later turned into a flextable and
#' added to the codebook document.
#'
#' @param df The saved file at `path`, read into memory as a data frame
#' @param path The path to the saved dataset of interest
#' @param title Optional title
#' @param subtitle Optional subtitle
#' @param description Text description of the dataset
#'
#' @return An rdocx object that can be printed to a Word document
#' @export
#'
#' @examples
#' # codebook_detect_5wk <- codebook(
#' #   df = detect_5wk %>% select(1:2),
#' #   path = "../data/detect_5wk.csv",
#' #   title = "Detection of Elder abuse Through Emergency Care Technicians (DETECT)",
#' #   subtitle = "5-Week Pilot Study",
#' #   description = description
#' # ) %>%
#' #   print(target = "example_officer_codebook.docx")
codebook <- function(df, path = NA, title = NA, subtitle = NA, description = NA) {

  # ===========================================================================
  # Create an empty Word rdocx object
  # default template contains only an empty paragraph
  # Using cursor_begin and body_remove, we can delete it
  # ===========================================================================
  rdocx <- officer::read_docx() %>%
    officer::cursor_begin() %>%
    officer::body_remove()

  # ===========================================================================
  # Optionally add title and subtitle to top of codebook
  # ===========================================================================
  codebook_shell <- codebook_add_title(
    rdocx = rdocx,
    title = title,
    subtitle = subtitle
  )

  # ===========================================================================
  # Add metadata to codebook shell
  # ===========================================================================
  # Get metadata
  df_metadata <- codebook_get_df_attributes(df, path = path) %>%
    flextable::regulartable() %>%
    codebook_theme_df_attributes()

  # Add metadata to codebook
  rdocx <- rdocx %>%
    flextable::body_add_flextable(df_metadata)

  # ===========================================================================
  # Optionally Add dataset description
  # ===========================================================================
  if (!is.na(description)) {
    # Add Description header
    rdocx <- rdocx %>%
      codebook_add_section_header("Description:")

    # Add dataset description to codebook
    rdocx <- rdocx %>%
      codebook_add_description(description)
  }

  # ===========================================================================
  # Iterate over every column in df - control with dplyr::select
  # Add column attributes and summary statistics to rdocx object
  # ===========================================================================

  # Add column Attributes header
  rdocx <- rdocx %>%
    codebook_add_section_header("Column Attributes:")

  # Create vector of column names
  col_nms <- names(df)

  # Iterate over all columns
  # ------------------------
  for (i in seq_along(col_nms)) {

    # Get column attributes
    table_var_attributes <- df %>%
      codebook_get_col_attributes(col_nms[[i]]) %>%
      flextable::regulartable() %>%
      codebook_theme_col_attr()

    # Add two blank lines above the attributes table
    rdocx <- rdocx %>%
      officer::body_add_par("") %>%
      officer::body_add_par("")

    # Add column attributes flextable to the rdocx object
    rdocx <- rdocx %>%
      flextable::body_add_flextable(table_var_attributes)

    # Get summary statistics
    summary_stats <- df %>%
      codebook_add_summary_stats(col_nms[[i]]) %>%
      codebook_summary_stats_to_ft()

    # Add summary statistics flextable to the codebook object
    rdocx <- rdocx %>%
      flextable::body_add_flextable(summary_stats)
  }

  # ===========================================================================
  # Return rdocx object that can be printed to a Word document
  # ===========================================================================
  rdocx
}

