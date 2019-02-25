#' Insert Comment
#'
#' Call this function as an addin to insert comment notation at the cursor
#' position.
#'
#' @export
insert_comment <- function() {

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ------------------------------------------------------------------
  doc_info = file_path = pattern = file_type = selection_start  = NULL
  selection_end = comment_start = comment_end = NULL

  # Get information about the current document and cursor position
  doc_info <- rstudioapi::getActiveDocumentContext()

  # Get file type
  file_path <- doc_info['path']
  pattern   <- "[a-zA-Z]+$"
  file_type <- stringr::str_extract(file_path, pattern)

  # Get the current cursor position
  selection_start <- doc_info[['selection']][[1]][['range']][['start']]
  selection_end   <- doc_info[['selection']][[1]][['range']][['end']]

  # Prepare the correct comment notation based on file type
  if (file_type %in% c('Rmd', 'html')) {
    comment_start <- "<!-- "
    comment_end <- " -->"
  } else if (file_type %in% c('css', 'js')) {
    comment_start <- "/* "
    comment_end <- " */"
  } else {
    stop(paste("This function does not know how to add comments to the file type: ", file_type))
  }

  # Insert comment
  selection_end['column'] <- selection_end['column'] + nchar(comment_start)
  rstudioapi::insertText(selection_start, comment_start)
  rstudioapi::insertText(selection_end, comment_end)
}
