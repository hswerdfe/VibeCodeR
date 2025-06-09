#' Extract Function Code from Context
#'
#' Extracts either the currently selected code or the full function definition
#' surrounding the cursor from a given context, such as from an editor in an IDE like RStudio.
#'
#' @param context A list representing the editor context. It should contain:
#'   - `context$selection`: A list with the currently selected text and its range.
#'   - `context$contents`: A character vector of all lines in the editor.
#'
#' @return A character string containing the selected code or the full function definition
#'   around the cursor. Returns `NULL` if no selection or function is found.
#'
#' @examples
#' # In an interactive editor context, this might return the full function code
#' # surrounding the cursor position, or the selected code if something is highlighted.
#'
#' @keywords internal
function_code <- function(context){

  selection <- context$selection[[1]]$text
  # If something is selected, use it
  if (nchar(selection) > 0) {
    return(selection)
  }

  # Otherwise, try to find function definition around cursor
  current_line <- context$selection[[1]]$range$start[1]
  all_lines <- context$contents

  # Look backwards for function definition
  func_start <- current_line
  for (i in current_line:max(1, current_line - 10)) {
    if (grepl("<-\\s*function\\s*\\(", all_lines[i])) {
      func_start <- i
      break
    }
  }

  # Look forwards for function end (find matching brace)
  func_end <- current_line
  brace_count <- 0
  found_start <- FALSE

  for (i in func_start:min(length(all_lines), current_line + 50)) {
    line <- all_lines[i]
    if (grepl("\\{", line)) {
      found_start <- TRUE
      brace_count <- brace_count + length(gregexpr("\\{", line)[[1]])
    }
    if (found_start && grepl("\\}", line)) {
      brace_count <- brace_count - length(gregexpr("\\}", line)[[1]])
      if (brace_count <= 0) {
        func_end <- i
        break
      }
    }
  }

  if (func_start == current_line && func_end == current_line) {
    return(NULL)
  }

  # Extract function code
  func_lines <- all_lines[func_start:func_end]
  return(paste(func_lines, collapse = "\n"))
}


#' Extract Function Name
#'
#' Extracts function name from function definition
#'
#' @param func_code Character string with function code
#' @return Character string with function name
#'
#' @examples
#'   extract_function_name(".myFunc = function() {}")
#'   extract_function_name("f <<- function(...) NULL")
#'   extract_function_name("foo <- function(x) x + 1")
extract_function_name <- function(func_code) {
  # Extract function name
  func_pattern <- "\\b([a-zA-Z.][a-zA-Z0-9._]*)\\s*([<]{1,2}-|=)\\s*function\\b"
  match <- base::regexec(func_pattern, func_code)

  if (match[[1]][1] == -1) {
    return("function")
  }

  matches <- base::regmatches(func_code, match)[[1]]
  return(matches[2])
}

