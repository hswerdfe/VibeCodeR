



#' Generate Contextual Prompts for Code Generation
#'
#' Generates context prompts based on specified configuration files, including
#' information about the currently selected file, libraries in memory, and
#' libraries referenced in other parts of the code.
#' @description Constructs a context prompt by reading configuration files
#'   and including relevant information based on the included context types.
#' @details This function reads configuration files located in the `.VibeCodeR`
#'   directory to determine which context elements to include in the prompt.
#'   It checks for files named `.include_context_*.config`, where `*` can be
#'   `currently_selected_file`, `libraries_in_memmory`, or
#'   `libraries_refrenced_in_other_parts_of_code`. The contents of these files
#'   (TRUE or FALSE) determine whether the corresponding context is included.
#'   If no context is included, a warning is issued, and an empty string is
#'   returned. The function retrieves information about the current file,
#'   loaded libraries, and referenced libraries (using `libraries_in_memmory()`
#'   and `libraries_extract_files()`).  The context prompt is then constructed by
#'   concatenating relevant information with section splitters.
#' @param context A list containing context information, typically obtained from
#'   `rstudioapi::getSourceEditorContext()`.  It must contain at least a
#'   `contents` element (a character vector representing the lines of the current document).
#' @param path The base path for the `.VibeCodeR` directory. Defaults to the
#'   project root directory using `here::here()`.
#' @param config_path The full path to the `.VibeCodeR` directory. Defaults to
#'   `here::here()/.VibeCodeR`.
#' @return A character string containing the constructed context prompt, or an
#'   empty string if no context is included.
#' @examples
#' \dontrun{
#' # Assuming you have a .VibeCodeR directory with context configuration files
#' context <- rstudioapi::getSourceEditorContext()
#' prompt <- context_prompts(context)
#' cat(prompt)
#' }
context_prompts <- function(
    context,
    path = here::here(),
    config_path = path |> file.path('.VibeCodeR')  
    
  ){
  #context <- active_document_context()
  #generate_roxygen_comment()
  section_splitter <- '\n---------------------------------\n'
  
  context_to_include <- 
    config_path |> 
    list.files(pattern = '^\\.include_context_.*\\.config$', all.files = TRUE, full.names = TRUE) |>
    purrr::set_names() |>
    purrr::keep(~{
      as.logical(readLines(.x))
    }) |>
    basename() |>
    stringr::str_remove_all('^.include_context_') |> 
    stringr::str_remove_all('.config$')
  
  if (length(context_to_include) == 0){
    warning('No context to include')
    return("")
  }
  
  context_prompt <- c(
    '\nFor context with the assigned task please see the following context:\n'
  )
  
  context_prompt <- 
    if ("currently_selected_file" %in% context_to_include){
      paste0(
        context_prompt,
        'As context this is the text of the current file:',
        section_splitter,
        context$contents |> paste0(collapse = '\n'),
        section_splitter
      )
    }else{context_prompt}
  
  context_prompt <- 
    if ("libraries_in_memmory" %in% context_to_include){
      paste0(
        context_prompt,
        'As context these are the libraries currently in memmory, feel free to refrence them if needed remember to use :: ',
        section_splitter,
        libraries_in_memmory() |> paste0(collapse = ', '),
        section_splitter
      )
    }else{context_prompt}
  
  
  context_prompt <- 
    if ("libraries_refrenced_in_other_parts_of_code" %in% context_to_include){
      paste0(
        context_prompt,
        'As context these are the libraries referenced by other files, feel free to refrence them if needed remember to use :: ,',
        section_splitter,
        libraries_extract_files() |> paste0(collapse = ', '),
        section_splitter
      )
    }else{context_prompt}
  
  context_prompt
}









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
