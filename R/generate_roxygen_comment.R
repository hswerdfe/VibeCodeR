#' Generate Roxygen Comment
#'
#' Adds a Roxygen skeleton for the function at the current cursor position.
#'
#' @return Inserts Roxygen comment block above the function.
#' @export
generate_roxygen_comment <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("RStudio API is not available.")
  }
  
  context <- rstudioapi::getActiveDocumentContext()
  current_line <- context$contents[context$selection[[1]]$range$start["row"]]
  
  # Insert basic Roxygen block above current line
  insert_text <- c(
    "#' Title",
    "#'",
    "#' @param x description",
    "#' @return value",
    "#' @export"
  )
  
  rstudioapi::insertText(
    location = rstudioapi::document_position(context$selection[[1]]$range$start["row"], 1),
    text = paste0(paste(insert_text, collapse = "\n"), "\n")
  )
}