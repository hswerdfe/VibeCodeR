



#' Check for RStudio API Availability
#'
#' This function checks whether the RStudio API is available in the current R session.
#' It attempts to load the "rstudioapi" package quietly and returns TRUE only if the
#' package is available and RStudio API functions can be used in the current environment.
#'
#' @return A logical value: TRUE if RStudio API is available and usable, FALSE otherwise.
#' @examples
#' has_rstudio_api()
#' @export 
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
#' @export
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
#' @export
cursor_selection <- function(context = NULL) {
  if ( ! has_rstudio_api()) {
    # Fallback: no cursor info available
    return("")
  }
  context <- 
    if (is.null(context))(
      active_document_context()
    )else{context}

  if (length(context$selection) > 0) {
    context$selection[[1]]$text
  }else{""}
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
#' @export         
insert_text <- function(text, 
                        location = NULL, 
                        context = NULL) {
  if ( ! has_rstudio_api()) {
    # Fallback: no cursor info available
    cat(text, "\n")
    return( invisible(text) )
  }
  
  
  location <- 
    if ( is.null(location)  ){
      if ( ! is.null(context)  ){
        # Moves the cursor to the start of the line containing the start of the selection.
        rstudioapi::document_position(context$selection[[1]]$range$start["row"], 1)
      }else{
        # Replaces the current selection.
        rstudioapi::insertText(text = text)
        return (invisible(text))
      }
    }
  text = ''

    # insert at the given context
    rstudioapi::insertText(
      location = location,
      text = paste(text, collapse = "\n")
    )
    return (invisible(text))
}


  
#' Get Text Input from User
#'
#' Prompts the user for text input, using RStudio's prompt if available, otherwise falling back to base R's `readline`.
#' @description Obtains text input from the user.
#' @details This function attempts to use the `rstudioapi` package to display a modal prompt for user input if running within RStudio. If `rstudioapi` is not available or not running within RStudio, it falls back to using the base R `readline` function. This ensures cross-platform compatibility and a consistent user experience.  The function prioritizes using `rstudioapi` because it provides a more user-friendly modal dialog for input, preventing the console from being cluttered with input prompts.  When using `readline`, a space is appended to the prompt for better readability.
#'
#' @param prompt character. The message displayed to the user as a prompt. Defaults to "Enter input:".
#' @param title character. The title of the input dialog (only applicable when using `rstudioapi`). Defaults to "Input".
#'
#' @return character. The text entered by the user.
#' @examples
#' \dontrun{
#' # Example 1: Using the default prompt and title
#' user_input <- text_input()
#' print(paste("You entered:", user_input))
#'
#' # Example 2: Specifying a custom prompt
#' name <- text_input(prompt = "Please enter your name:")
#' print(paste("Hello,", name, "!"))
#'
#' # Example 3: Specifying a custom prompt and title (RStudio only)
#' answer <- text_input(prompt = "What is the capital of France?", title = "Geography Quiz")
#' print(paste("Your answer:", answer))
#' }
#' @export
#' @author Placeholder
text_input <- function(prompt = "Enter input:", title = "Input") {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ans <- rstudioapi::showPrompt(title = title, message = prompt)
    return(ans)
  }
  # fallback: base R readline
  ans <- readline(prompt = paste0(prompt, " "))
  return(ans)
}





















#' Get User Selection Safely with Fallbacks
#'
#' This function prompts the user to select an option from a list of choices,
#' providing a safe and consistent experience across different R environments,
#' including RStudio and base R. It leverages `rstudioapi` when available for
#' a more integrated experience, and falls back to base R methods when not.
#'
#' @description Safely prompts the user to select an option from a list of choices.
#'
#' @details
#' This function first checks if the `rstudioapi` package is available and if the
#' RStudio API is active. If so, it attempts to use RStudio's native prompting
#' capabilities.  For binary (Yes/No) choices, it uses `rstudioapi::showQuestion`.
#' For multiple choices, it constructs a numbered list and prompts the user to
#' enter the corresponding number. If `rstudioapi` is not available, it falls back
#' to the base R `menu()` function.  The function handles cases where the user
#' cancels the selection or enters invalid input by returning `NULL`.
#'
#' @param prompt A character string specifying the prompt message to display to the user.
#' @param choices A character vector containing the options to present to the user.
#'
#' @return A character string representing the user's selected option, or `NULL` if the user cancels the selection or enters invalid input.
#'
#' @examples
#' \dontrun{
#' # Example 1: Simple Yes/No selection
#' answer <- safe_get_selection_input(prompt = "Do you want to proceed?", choices = c("Yes", "No"))
#' if (!is.null(answer)) {
#'   print(paste("You selected:", answer))
#' } else {
#'   print("Selection cancelled.")
#' }
#'
#' # Example 2: Multiple choice selection
#' fruit <- safe_get_selection_input(prompt = "Choose your favorite fruit:", choices = c("Apple", "Banana", "Orange"))
#' if (!is.null(fruit)) {
#'   print(paste("Your favorite fruit is:", fruit))
#' } else {
#'   print("Selection cancelled.")
#' }
#'
#' # Example 3: Using default values
#' proceed <- safe_get_selection_input("Proceed?")
#' if (!is.null(proceed)) {
#'   print(paste("You selected:", proceed))
#' } else {
#'   print("Selection cancelled.")
#' }
#' }
#' @export
#' @author Placeholder
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
