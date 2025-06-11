


#' Collapse a String Vector into a Single String with Newlines
#'
#' This function takes a character vector, splits each element by a specified newline character,
#' and then collapses the resulting vector into a single string, using the same newline character
#' as a separator. This effectively ensures that the input, potentially containing multiple
#' lines, is represented as a single string with consistent newline formatting.
#'
#' @description Converts a character vector into a single string with newlines.
#'
#' @details The function first splits each string in the input vector `x` by the `new_line` character.
#'   It then unlists the resulting list into a character vector and collapses it back into a single
#'   string, using `new_line` as the separator. This is useful for handling text data where
#'   newline characters might be inconsistent or need standardization. The default newline character
#'   is `'\\n'`.
#'
#' @param x A character vector to be processed.
#' @param new_line The newline character used for splitting and collapsing the strings.
#'   Defaults to `'\\n'`.
#'
#' @return A character string representing the collapsed input, with all elements separated by the
#'   specified newline character.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default newline character
#' txt_single(c("line1", "line2", "line3"))
#'
#' # Using a different newline character (e.g., carriage return)
#' txt_single(c("line1", "line2", "line3"), new_line = "\r")
#'
#' # Handling multiple lines within a single string element
#' txt_single(c("line1\nline2", "line3"))
#' 
#' # Example with mixed newline characters (requires additional cleaning)
#' mixed_text <- c("line1\r\nline2", "line3\nline4")
#' # To handle mixed newlines, you might need to replace \r\n with \n first
#' cleaned_text <- stringr::str_replace_all(mixed_text, "\r\n", "\n")
#' txt_single(cleaned_text)
#' }
txt_single <- function(x, new_line = '\n'){
  x |>
    stringr::str_split(new_line) |> 
    base::unlist() |> 
    base::paste0(collapse = new_line)
}  




#' Split a String into Multiple Substrings by Newline Characters
#'
#' This function takes a character string and splits it into a vector of substrings,
#' using a specified newline character as the delimiter.
#'
#' @description Splits a string into multiple substrings based on newline characters.
#'
#' @details The function uses `stringr::str_split()` to split the input string `x` by the
#'   `new_line` character.  The resulting list is then unlisted into a character vector.
#'   This is useful for processing text data where you need to handle multiple lines
#'   within a single string. The default newline character is `'\\n'`.
#'
#' @param x A character string to be split.
#' @param new_line The newline character used for splitting the string.
#'   Defaults to `'\\n'`.
#'
#' @return A character vector where each element is a substring resulting from the split.
#'
#' @examples
#' # Basic usage with default newline character
#' \dontrun{
#' txt_multi("line1\nline2\nline3")
#'
#' # Using a different newline character (e.g., carriage return)
#' txt_multi("line1\rline2\rline3", new_line = "\r")
#'
#' # Handling a single line string
#' txt_multi("line1")
#'
#' 
#' # Example with mixed newline characters (requires additional cleaning)
#' mixed_text <- "line1\r\nline2\nline3"
#' # To handle mixed newlines, you might need to replace \r\n with \n first
#' cleaned_text <- stringr::str_replace_all(mixed_text, "\r\n", "\n")
#' txt_multi(cleaned_text)
#' }
txt_multi <- function(x, new_line = '\n'){
  x |> 
    stringr::str_split(new_line) |>
    unlist()
  
}






#' Check if a String Contains Only Blank Roxygen Comments
#'
#' This function determines whether a given string contains only blank lines
#' or lines consisting solely of roxygen comment markers (`#'`) and whitespace.
#'
#' @description Checks if a string is effectively a blank roxygen comment.
#'
#' @details The function splits the input string by newline characters, then removes
#'   leading and trailing whitespace and roxygen comment markers (`#'`) from each line.
#'   It then counts the number of non-empty lines remaining. If no non-empty lines
#'   are found, the function returns `TRUE`, indicating that the input string
#'   consists only of blank or commented-out lines.
#'
#' @param x A character string to be checked.
#'
#' @return A logical value: `TRUE` if the string contains only blank or roxygen-commented lines,
#'   `FALSE` otherwise.
#'
#' @examples
#' # Example with a blank roxygen comment
#' \dontrun{
#' is_blank_comment("#'  \n#'   \n")
#'
#' # Example with a non-blank roxygen comment
#' is_blank_comment("#' This is a comment\n#' Another line")
#'
#' # Example with no roxygen comment markers
#' is_blank_comment("This is a comment\nAnother line")
#'
#' # Example with only whitespace
#' is_blank_comment("   \n  \n")
#' }
is_blank_comment <- function(x){
  #generate_roxygen_comment()
  #context <- active_document_context()
  len <- 
    x |>
    stringr::str_split('\n') |>
    unlist() |>
    stringr::str_remove("^\\s*#'") |>
    stringr::str_remove("^\\s*#'") |>
    stringr::str_trim() |> 
    stringr::str_subset('^.+$') |>
    length()
  len == 0
}


