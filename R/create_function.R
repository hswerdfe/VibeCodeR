#' Generate Function
#'
#' Inserts a function skeleton at the current cursor position.
#'
#' @export
generate_function <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("RStudio API is not available.")
  }
  
  answers <- user_input(questions = list(list(
    question = 'Describe what the function will do.',
    type = 'textarea',
    default = 'add two numbers then add one'
    )))
  
  # Function skeleton text
  fun_text <- paste(
    "my_function <- function(arg1, arg2) {",
    "  # TODO: implement",
    "  return(NULL)",
    "}",
    sep = "\n"
  )
  
  # Insert into active document
  rstudioapi::insertText(text = paste0(fun_text, "\n"))
}


