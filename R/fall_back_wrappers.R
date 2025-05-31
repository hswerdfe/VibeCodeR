



#' Check for RStudio API Availability
#'
#' This function checks whether the RStudio API is available in the current R session.
#' It attempts to load the "rstudioapi" package quietly and returns TRUE only if the
#' package is available and RStudio API functions can be used in the current environment.
#'
#' @return A logical value: TRUE if RStudio API is available and usable, FALSE otherwise.
#' @examples
#' has_rstudio_api()
has_rstudio_api <- function(){
  answer <- requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
  if ( ! answer){
    warning("RStudio API not available or not in an interactive session with document context.")
  }
  answer
}



#' Get Active RStudio Document Context
#'
#' Retrieves information about the currently active document in RStudio, including its file path, full contents, and selection.
#' If the RStudio API is not available (e.g., when running outside of RStudio), a default placeholder context is returned.
#'
#' @return A list representing the active document context. The structure includes:
#'   - \code{path}: The path to the active file, or \code{NULL} if unavailable.
#'   - \code{contents}: A character vector of the file contents.
#'   - \code{selection}: A list containing selection details from the document.
#'
#' @examples
#' ctx <- active_document_context()
#' if (!is.null(ctx$path)) {
#'   print(ctx$path)
#' }
#'
#' @keywords rstudio document context
#' @export
active_document_context <- function() {
  if ( ! has_rstudio_api()) {
    return(list(
      path = NULL,
      contents = character(),
      selection = list(list(text = ""))
    ))
  }
  rstudioapi::getActiveDocumentContext()
}



#' Get the Current Cursor Location in RStudio
#'
#' This function returns the start position (row and column) of the first selection
#' in the currently active RStudio document. It utilizes the RStudio API to access
#' the cursor's location. If the RStudio API is not available or no cursor position
#' can be retrieved, the function returns NULL.
#'
#' @return A numeric vector of length two containing the row and column,
#'         or NULL if the cursor location cannot be determined.
#' @examples
#' loc <- cursor_location()
#' if (!is.null(loc)) {
#'   cat("Cursor is at row", loc[[1]], "and column", loc[[2]], "\n")
#' }
cursor_location <- function() {
  if ( ! has_rstudio_api()) {
    # Fallback: no cursor info available
    return(NULL)
  }

  # Returns a list with start row,col of first selection
  context <- active_document_context()
  if (length(context$selection) > 0) {
    context$selection[[1]]$range$start
  }

}






#' Determine Selected Text at Cursor in RStudio
#'
#' This function retrieves the currently selected text from the active
#' document in RStudio using the RStudio API. If RStudio API is not
#' available, it returns an empty string as a fallback.
#'
#' @return A character string representing the selected text at the cursor.
#'         If no selection is made or if RStudio API is unavailable,
#'         returns an empty string.
#'
#' @details Works only within the RStudio environment and requires the
#'          rstudioapi package to be available and functional.
#'
#' @examples
#' \dontrun{
#' selected_text <- cursor_selection()
#' print(selected_text)
#' }
#'
#' @noRd
cursor_selection <- function() {
  if ( ! has_rstudio_api()) {
    # Fallback: no cursor info available
    return("")
  }

  context <- active_document_context()

  if (length(context$selection) > 0) {
    context$selection[[1]]$text
  }
}






#' Insert Text into RStudio Editor or Print to Console
#'
#' This function inserts a given text string at the current cursor position
#' in the RStudio Source Editor. If RStudio API is not available
#' (e.g., when running in a non-RStudio environment), it falls back to
#' printing the text to the console.
#'
#' @param text A character string to insert or print.
#' @return Invisibly returns TRUE if text was inserted via RStudio API,
#'         or FALSE if it was printed to the console.
insert_Text <- function(text) {
  if ( ! has_rstudio_api()) {
    # Fallback: no cursor info available
    cat(text, "\n")
    return(invisible(FALSE))
  }
  # Insert text at current selection
  rstudioapi::insertText(text = text)
  invisible(TRUE)
}

text_input <- function(prompt = "Enter input:", title = "Input") {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ans <- rstudioapi::showPrompt(title = title, message = prompt)
    return(ans)
  }
  # fallback: base R readline
  ans <- readline(prompt = paste0(prompt, " "))
  return(ans)
}


















safe_get_selection_input <- function(prompt = "Select an option:", choices = c("Yes", "No")) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    # rstudioapi does not have true selection prompt
    # We'll use showQuestion for two choices if relevant
    # Otherwise, simulate with showPrompt or fall back
    if (length(choices) == 2 && all(tolower(choices) %in% c("yes", "no"))) {
      ans <- rstudioapi::showQuestion(title = "Question", message = prompt)
      # returns TRUE for Yes, FALSE for No
      return(if (ans) choices[1] else choices[2])
    }
    # fallback for multiple choices: ask user to input choice by number
    choice_prompt <- paste0(
      prompt, "\n",
      paste0(seq_along(choices), ": ", choices, collapse = "\n"),
      "\nEnter choice number: "
    )
    ans <- readline(prompt = choice_prompt)
    choice_num <- suppressWarnings(as.integer(ans))
    if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(choices)) {
      return(choices[choice_num])
    }
    return(NULL)
  }
  # fallback base R: use menu()
  selected <- menu(choices, title = prompt)
  if (selected == 0) return(NULL)
  return(choices[selected])
}
