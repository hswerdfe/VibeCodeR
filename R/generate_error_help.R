
#' Generate Error Explanation from Clipboard Content Using LLMs
#'
#' This function reads content from the clipboard, assumes it's a code-related
#' error, and utilizes a language model to provide assistance and a potential
#' solution.
#' @description Analyzes code copied to the clipboard and provides debugging help using an LLM.
#' @details The function reads the clipboard content using `clipr::read_clip()`.
#'   It then constructs a prompt including the clipboard content and other
#'   contextual information (error messages, history, platform details,
#'   traceback, R version, environment context).  This prompt is sent to an
#'   LLM (via `llm_request()`) to generate a helpful explanation and solution.
#'   The LLM response and the original clipboard content are then displayed using
#'   `display_markdown()`. The prompt instructs the LLM to assist with the
#'   provided code, using markdown for clarity.
#' @return None. The function displays the LLM's response in the RStudio Viewer.
#' @examples
#' \dontrun{
#' # Copy some problematic code to the clipboard (e.g., from an error message)
#' # Then run:
#' generate_clipboard_error_help()
#' }
generate_clipboard_error_help <- function(){
  
  #highlighted_msg <-context_highlighted_message(clipboard_content = clipboard_content)
  highlighted_msg <-context_highlighted_message()
  
  highlighted_msg <- clipboard_content <- clipr::read_clip()
  prompt <- c(
    'In R coding the programmer is having some issue and has coppied some text below which is likely an error or warning or some other problematic code',
    highlighted_msg,
    'Please help the user with this code as best you can.',
    'your answer will be displayed on screen to a user who will then attempt to fix the issue.',
    'Please use markdown to highlight important sections and headers of the response.',
    'below is some auto generated context information which may be helpfull in your reply.',
    context_error_message(),
    context_history(),
    context_platform(),
    context_traceback(),
    context_r_version(),
    environment_context(),
    'Please now provide a consise response to the help fix the above issue'
  ) |> 
    txt_single()
  
  
  response <- llm_request(prompt)
  
  response_plus <-c(highlighted_msg, response) |> txt_single()
  # Convert and display in RStudio Viewer
  display_markdown(response_plus)
}




#' Generate Helpful Error Explanations Using LLMs
#'
#' This function captures the last error, gathers contextual information, and
#' uses a language model to generate an explanation and solution for the error.
#' @description Generates a user-friendly explanation of an R error using an LLM.
#' @details This function leverages `context_error_message()` to capture the
#'   most recent error, and combines it with contextual information such as
#'   command history, platform details, traceback, R version, and environment
#'   variables.  This information is then formatted into a prompt and sent to
#'   an LLM (via `llm_request()`) to generate a helpful explanation and
#'   solution.  The response from the LLM, along with the original error
#'   message, are then displayed using `display_markdown()`. The prompt instructs
#'   the LLM to explain the error's nature, suggest fixes, provide debugging
#'   steps, and use markdown formatting for clarity.
#' @return None. The function displays the LLM's response in the RStudio Viewer.
#' @examples
#' \dontrun{
#' # Simulate an error
#' try(log("a"))
#' generate_last_error_help()
#' 
#' # Variable object name in error message - creates different error text each time
#' try(get(paste0("nonexistent_", sample(letters, 5, replace=TRUE), collapse="")), silent=TRUE)
#' generate_last_error_help()
#' 
#' 
#' # Random function name in error message - generates different "could not find function" errors
#' try(do.call(paste0("fake_func_", sample(1000:9999, 1)), list()), silent=TRUE)
#' generate_last_error_help()
#' 
#' # rownames(mtcars)/2 
#' generate_last_error_help()
#' 
#' }
generate_last_error_help <- function(){
  
  error_msg <- context_error_message()
  prompt <- c(
    'An error has occured in R Code',
    'Please explain the nature of the error, and how to fix the error, as well as any debugging steps that can be taken if the error is ambiguous',
    'your answer will be displayed on screen to a user who will then attempt to fix the issue.',
    'Please use markdown to highlight important sections and headers of the response.',
    'below is some auto generated context information which may be helpfull in your reply.',
    error_msg,
    context_history(),
    context_platform(),
    context_traceback(),
    context_r_version(),
    environment_context(),
    'Please now provide a consise answer to the help fix the above error.'
  ) |> 
    txt_single()
  
  
  response <- llm_request(prompt)
  
  response_plus <-c(error_msg, response) |> txt_single()
  # Convert and display in RStudio Viewer
  display_markdown(response_plus)
}

