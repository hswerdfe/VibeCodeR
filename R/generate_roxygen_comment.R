









#' Generate Roxygen2 Comment from Function Code
#'
#' Generates a roxygen2 documentation comment block from provided function code.
#' @description Creates a roxygen2 comment string based on the given function code.
#' @details This function takes function code, an existing comment, the function name,
#'   file extension, and context information, and uses an LLM to generate a roxygen2
#'   comment block. It constructs a prompt using configuration files and the provided
#'   function details, sends it to the LLM via `llm_request()`, and extracts the comment
#'   from the response. If no existing comment is available, "<<MISSING>>" is used as
#'   a placeholder. The context is passed to `context_prompts()` to include relevant
#'   information in the prompt.
#' @param function_code Character, the function code to document.
#' @param function_comment Character, any existing roxygen2 comment for the function.
#' @param function_name Character, the name of the function.
#' @param file_extension Character, the file extension of the R script (e.g., "R").
#' @param context List, context information, typically obtained from `rstudioapi::getSourceEditorContext()`.
#' @param ... Additional arguments passed to other methods.
#' @return Character, a string containing the generated roxygen2 comment.
generate_roxygen_comment_from_code <- function(
    function_code, function_comment, function_name, file_extension, context,
    ...
){
  #generate_roxygen_comment()
  #context <- active_document_context()
  function_code <- function_code |> txt_single()
  function_comment <- function_comment |> txt_single()
  function_name <- function_name |> txt_single()
  
  function_comment <- if (function_comment == ''){'<<MISSING>>'}else{function_comment}
  
  context_prompt <- context_prompts(context = context)

  
  prompt <- 
    paste(
      read_vibe_coder_config('.generic_project_style_guides.config'), 
      glue::glue(read_vibe_coder_config('.generate_roxygen_prompt.config')),
      function_code,
      context_prompt,
      #'REMEMBER ONLY THE COMMENT!!!',
      collapse = '\n'
    )
  #prompt |> cat()
  response <- llm_request(prompt)
  #response  |> cat()
  
  response_2 <- 
    response |> 
    txt_multi() |>
    stringr::str_subset("^\\s*#\\s*'") |>
    txt_single()
  
  response_2
}


#' Generate Roxygen2 Comment for Current Function in RStudio
#'
#' Generates a roxygen2 documentation comment block for the R function
#' at the current cursor position in RStudio, replacing any existing comments.
#' @description Automatically creates a roxygen2 comment block for a function.
#' @details This function uses `rstudioapi` to get the function's code,
#'   name, and existing comments. It then generates a roxygen2 skeleton
#'   and inserts it above the function in the R script.
#'   This function operates by side effect, modifying the current R script.
#' @return No return value. This function operates by side effect, modifying the R script in RStudio.
#' @examples
#' \dontrun{
#' generate_roxygen_comment()
#' }
generate_roxygen_comment <- function() {
  
  context <- active_document_context()
  # print(context)
  full_file_text <- context$contents
  function_details <- 
    function_code_2(
      full_text = context$contents, 
      cursor_line = context$selection[[1]]$range$start[["row"]]
    )
  #current_line <- context$contents[context$selection[[1]]$range$start["row"]]
  
  #generate_roxygen_comment()
  file_extension <- tools::file_ext(basename(context$path))
  func_code <- function_code(context)
  comment <- generate_roxygen_comment_from_code(
    function_code = function_details$code, 
    function_comment = function_details$comment, 
    function_name = function_details$name,
    file_extension = file_extension,
    context = context
  )
  #comment |> cat()
  if ( ! is_blank_comment(x = function_details$comment)){
    replace_lines(
      start_line = function_details$comment_lines[[1]],
      end_line = function_details$comment_lines[[2]],
      context = context,
      replacement = comment
    )    
  }else{
    insert_lines(
      line = function_details$code_lines[[1]],
      context = context,
      text = comment
    )
  }
}
