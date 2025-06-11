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
  #"\\s*\\w+\\s*(<-|=)\\s*function"
  #"^\\s*function\\s*\\("
  #"<-\\s*function\\s*\\("
  
  # selection <- context$selection[[1]]$text
  # # If something is selected, use it
  # if (nchar(selection) > 0) {
  #   return(selection)
  # }

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
#function_code(context) |> cat()

#' Extract Function Name
#'
#' Extracts function name from function definition
#'
#' @param func_code Character string with function code
#' @return Character string with function name
#'
#' @examples
#'  \dontrun{
#'   extract_function_name(".myFunc = function() {}")
#'   extract_function_name("f <<- function(...) NULL")
#'   extract_function_name("foo <- function(x) x + 1")
#'  }
extract_function_name <- function(func_code) {
  # Extract function name
  func_code_2 <- func_code|> stringr::str_split('\n') |> unlist() |> paste(collapse = '\n')
  
  func_pattern <- "\\b([a-zA-Z.][a-zA-Z0-9._]*)\\s*([<]{1,2}-|=)\\s*function\\b"
  match <- base::regexec(func_pattern, func_code_2)

  if (match[[1]][1] == -1) {
    return("")
  }

  matches <- base::regmatches(func_code_2, match)[[1]]
  return(matches[2])
}





roxygen_comment <- function(full_text, func_first_line) {
  
  full_text_2<- full_text |> txt_multi()
    
  text_above <- full_text_2[1:func_first_line-1]
  
  non_empty_lines <- which(trimws(text_above) != "")
  if (length(non_empty_lines) == 0){
    return(list(
      comment_text = '',
      comment_lines = c(NA, NA)
    )
    )
  }
  
  last_non_empty <- max(non_empty_lines)
  text_above <- text_above[1:last_non_empty]
  
  first_line <- NULL
  for (i in seq(length(text_above), 1)){
    curr_line <- trimws(text_above[[i]])
    if ( ! stringr::str_detect(curr_line, "^#'")){
      first_line <- i+1
      break
    }
  }
  
  
  if (is.null(first_line)){
    return(list(
      comment_text = '',
      comment_lines = c(NA, NA)
    )
    )
  }
  comment_text <- 
    if (first_line <= length(text_above)){
      text_above[first_line:length(text_above)]  
    }else{
      ""
    }
  
  return(list(
    comment_text = comment_text,
    comment_lines = c(first_line, length(text_above))
    )
  )
}


function_code_2 <- function(full_text, cursor_line){
  
  full_text_2<-
    full_text |> txt_multi()
  
  #full_text <- context$contents
  #cursor_line <- context$selection[[1]]$range$start[['row']]
  #cursor_line = 5
  
  
  first_line <- NULL
  for (i in seq(cursor_line, 1) ) {
    #i = 35
    curr_line <- full_text_2[[i]]
    
    if (
      stringr::str_detect(curr_line, "\\s*\\w+\\s(<<-|<-|=)\\s*function\\s*\\(")
    ){
      first_line <- i
      break
    }
    
    above_line <- if (i == 1){''} else {full_text_2[[i-1]]}
    if  (stringr::str_detect(curr_line, "^\\s*function\\s*\\(") &
         stringr::str_detect(above_line, "^\\s*\\w+\\s(<<-|<-|=)\\s*")
    ){
      first_line <- i - 1
      break      
    }
  }
  
  if ( is.null(first_line)){
    message(glue::glue('looking below line {cursor_line}`'))
    
    for (i in seq(cursor_line, length(full_text_2)) ) {
      curr_line <- full_text_2[[i]]
      if (
        stringr::str_detect(curr_line, "\\s*\\w+\\s(<<-|<-|=)\\s*function\\s*\\(")
      ){
        first_line <- i
        break
      }   
      above_line <- if (i == 1){''} else {full_text_2[[i-1]]}
      if  (stringr::str_detect(curr_line, "^\\s*function\\s*\\(") &
           stringr::str_detect(above_line, "^\\s*\\w+\\s(<<-|<-|=)\\s*")
      ){
        first_line <- i - 1
        break      
      }      
    }
    
    if ( is.null(first_line)){
      message(glue::glue('No function start found at cursor (or below). selected was `{full_text}`'))
      return(NULL)  
    }
  }
  
  total_open <- 0
  total_close <- 0
  last_line <- NULL
  for (j in seq(first_line, length(full_text_2))){
    #j = 140
    curr_line <- full_text_2[[j]]
    #curr_line = 'asdfa { asdfas{df'
    total_open <- total_open + curr_line |> stringr::str_count(pattern = '\\{')
    total_close <- total_close + curr_line |> stringr::str_count(pattern = '\\}')
    
    #print(glue::glue('{j}, {total_open}, {total_close}'))
    if (total_open <= total_close & total_open > 0){
      last_line <- j
      break
    }
  }
  
  if ( is.null(last_line)){
    warning(glue::glue('No function end found at cursor. selected was `{full_text}`'))
    function_text <- NULL
    last_line <- NULL
    #return(NULL)
  }  
  function_text <- 
    if (is.numeric(first_line) & is.numeric(last_line)){
      full_text_2[first_line:last_line]  
    }else{NULL}
  
  function_name <- extract_function_name(function_text)
  
  
  comment <- roxygen_comment(full_text = full_text_2, func_first_line =first_line)
  return(
    list(
      code = function_text,
      code_lines = c(first_line, last_line), 
      name = function_name,
      comment = comment$comment_text,
      comment_lines = comment$comment_lines
    )
  )
}

#' 
#' function_code_2(
#'   full_text, 8
#' )
#' 
#' full_text <- "
#' othere text
#' 
#'    #' asdfasdfasfd
#' #' adfasdfas
#' 
#' test_f <-
#'   function(){
#'     return('a')
#'   }
#' asdfasdf
#' qqqqq
#' "
#' test_g <<-
#'   function
#' (){
#'     return('a')
#'   }
#' test_g(
