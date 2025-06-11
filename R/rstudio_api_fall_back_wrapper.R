




#' Check for RStudio API Availability
#'
#' This function checks whether the RStudio API is available in the current R session.
#' It attempts to load the "rstudioapi" package quietly and returns TRUE only if the
#' package is available and RStudio API functions can be used in the current environment.
#'
#' @return A logical value: TRUE if RStudio API is available and usable, FALSE otherwise.
#' @examples
#'     VibeCodeR:::has_rstudio_api()
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
#' \dontrun{
#'   ctx <- active_document_context()
#'   if (!is.null(ctx$path)) {
#'     print(ctx$path)
#'   }
#' }
#' @keywords rstudio document context
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
#' \dontrun{
#'   loc <- cursor_location()
#'   if (!is.null(loc)) {
#'     cat("Cursor is at row", loc[[1]], "and column", loc[[2]], "\n")
#'   }
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
#' @param context a context, optional defaults to NULL if NULL will try to get the active document context
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





#' Insert Text into RStudio Editor or Print to Console with Flexible Location
#'
#' This function provides a way to insert text into the RStudio source editor
#' at a specified location, or prints the text to the console if RStudio API
#' is not available. It also provides flexibility in specifying the insertion
#' point relative to a given location (before, after, or replace).
#'
#' @description Inserts text into the RStudio editor or prints to console.
#' @details When running in an RStudio environment, this function uses the
#'   \code{rstudioapi} package to insert text at a specific location, which
#'   can be defined explicitly or derived from the current context (e.g.,
#'   the start of the current selection). If \code{rstudioapi} is unavailable
#'   (e.g., outside of RStudio), the function falls back to printing the
#'   text to the console. The \code{relative_location} parameter allows
#'   specifying whether the text should be inserted before, after, or replace
#'   a given location. If no location is provided the text will be inserted at the cursor location.
#'
#' @param text character. A string or character vector to insert or print.  If a character vector is provided, the elements are collapsed with newline separators.
#' @param location list. An optional list specifying the location to insert the text. If \code{NULL} and \code{context} is provided, the location will be derived from the context. Defaults to \code{NULL}.
#' @param context list. An optional list providing context information, such as the current selection. If provided and \code{location} is \code{NULL}, the location will be derived from the context. Defaults to \code{NULL}.
#' @param relative_location character. Specifies how the text should be inserted relative to the given \code{location}. Can be "replace" (default), "before", or "after".
#'
#' @return Invisibly returns the input \code{text}.
#' @examples
#' \dontrun{
#' # Example 1: Insert text at the cursor position
#' insert_text("Hello, RStudio!")
#'
#' # Example 2: Insert text at a specific location (row and column)
#' insert_text("Important comment", location = list(row = 10, column = 5))
#'
#' # Example 3: Replace the current selection with new text
#' insert_text("New selection", context = active_document_context())
#'
#' # Example 4: Insert text before a specific location
#' insert_text("Prefix: ", location = list(row = 15, column = 1), relative_location = "before")
#' }

insert_text <- function(text, 
                        location = NULL, 
                        context = NULL, 
                        relative_location = 'replace') {
  #generate_roxygen_comment()
  #context <- active_document_context()
  if ( ! has_rstudio_api()) {
    # Fallback: no cursor info available
    cat(text, "\n")
    return( invisible(text) )
  }
  
  #######################
  # get location from context if needed
  location <- 
    if ( is.null(location) & !is.null(context) ){
      context$selection[[1]]$range
    }else{location}
  
  
  ###################################
  # before / after / replace
  location_relative <- 
    if ( is.null(location)){
       NULL
    }else if (relative_location == 'replace'){
      location
    }else if (relative_location == 'before'){
      location$start
    }else if (relative_location == 'after'){
      location$end
    }else{
      stop(glue::glue('relative_location = `{relative_location}`'))
    }
  if (is.null(location_relative)){
    # insert at the given context
    rstudioapi::insertText(
      text = paste(text, collapse = "\n")
    )
    
  }else{
    # insert at the given context
    rstudioapi::insertText(
      location = location,
      text = paste(text, collapse = "\n")
    )
    
  }
  
  return(invisible(text))
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
text_input <- function(
    prompt = "Enter input:", 
    title = "Input"
) {
  if (base::requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ans <- rstudioapi::showPrompt(title = title, message = prompt)
    return(ans)
  }
  # fallback: base R readline
  ans <- readline(prompt = paste0(prompt, " "))
  return(ans)
}


insert_lines <- function(
  line,
  context,
  text
){
  rstudioapi::insertText(
    location = c(line, 1),  
    text = paste0(text |> txt_single(), "\n"),  
    id = context$id
  )      
}



replace_lines <- function(start_line, 
                          end_line, 
                          context, 
                          replacement, 
                          ask = TRUE){
  #start_line  = 126
  #end_line = 136
  #replacement <- comment
  
  end_line_chars <- nchar(context$contents[[end_line]]) + 1
  orig <- context$contents[start_line:end_line] |> txt_single()
  new <- replacement |> txt_single() 
  user_reponse <- 
    if (ask){
      show_diff_gadget(original =orig, refactored = new  )  
    }else{'accept'}
  
  
  
  
  if (user_reponse == 'accept'){
    rstudioapi::modifyRange(
      location = rstudioapi::document_range(
        start = c(start_line, 1),  # 
        end = c(end_line, end_line_chars)    # 
      ),
      text = paste0(new, "\n"),  
      id = context$id
    )      
  }
  return(paste0(new, ""))
}





# # Helper function to find function boundaries
# find_function_boundaries <- function(contents, start_row) {
#   # Find the start of the function (look for assignment or function keyword)
#   func_start <- start_row
#   
#   # Look backwards to find the actual start of the function definition
#   for (i in start_row:1) {
#     line <- contents[i]
#     # Check if this line contains function assignment or definition
#     if (grepl("\\s*\\w+\\s*(<-|=)\\s*function", line) || 
#         grepl("^\\s*function\\s*\\(", line)) {
#       func_start <- i
#       break
#     }
#     # Stop if we hit an empty line or other function
#     if (grepl("^\\s*$", line) && i < start_row) {
#       func_start <- i + 1
#       break
#     }
#   }









