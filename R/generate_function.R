








#' Generate Function from Specifications using LLM
#'
#' @title Generate Function from Specifications using LLM
#' @description Generates an R function as a string from a provided specification
#'   using a Large Language Model (LLM).
#' @param specs Character; Specifications for what the function should do.
#'   This should be a clear description of the desired function's
#'   functionality, arguments, and expected behavior.
#' @param function_name Character; The desired name for the generated function.
#'   If an empty string (the default), the LLM will name the function.
#'   Defaults to `''`.
#' @param context list; An optional document context object providing additional
#'   information for the LLM, such as existing code in the current document.
#'   Defaults to `NULL`, in which case the active document context is used.
#' @return Character; The string returned from the LLM, which should contain
#'   the generated R function code.
#' @description
#' Generates a function from an LLM given a whiten specification for the function and an optional function name
#' @details
#' This function constructs a prompt including a generic project style guide,
#' a function generation prompt, the user-supplied specifications, and
#' optionally the desired function name. It then sends this prompt to an LLM
#' via `llm_request()` and returns the LLM's response. The function uses
#' `read_vibe_coder_config()` to retrieve configuration files. The `context`
#' parameter allows for providing contextual information to the LLM.
#'
#' Note: This function relies on the `llm_request`, `read_vibe_coder_config`,
#' and `active_document_context` functions, which are assumed to be defined
#' elsewhere. The `.generic_project_style_guides.config` and
#' `.generate_function_prompt.config` files are also required.
#' @examples
#' \dontrun{
#' # Example 1: Generate a simple addition function
#' specs <- "Take two numbers as input and return their sum"
#' addition_function <- generate_function_from_specs(specs)
#' cat(addition_function)
#'
#' # Example 2: Generate a function with a specified name
#' specs <- "Calculate the mean of a numeric vector"
#' function_name <- "calculate_mean"
#' mean_function <- generate_function_from_specs(specs, function_name)
#' cat(mean_function)
#'
#' # Example 3: Provide additional context (replace with actual context object)
#' # context <- active_document_context()
#' # specs <- "Update the data frame with new calculated columns"
#' # updated_df_function <- generate_function_from_specs(specs, context = context)
#' # cat(updated_df_function)
#' }
generate_function_from_specs <- function(specs, function_name = '' , context = NULL){
  #generate_roxygen_comment()
  #context <- active_document_context()
  #specs = 'make some Noise'
  prompt <- 
    paste(
      read_vibe_coder_config('.generic_project_style_guides.config'), 
      read_vibe_coder_config('.generate_function_prompt.config'),
      specs,
      collapse = '\n'
    )
  context <- if (is.null(context)){active_document_context()} else {context}
  
  
  prompt <- 
    if ( trimws(function_name) == ""){
      prompt
    }else{
      paste(
        c(
          prompt, 
          paste('Name the function:', function_name, collapse = ' ')
        ),
        collapse = '\n'
      ) 
    }  
  context_prompt <- context_prompts(context)
  
  
  prompt <- 
    paste(
      prompt, 
      context_prompt, sep = '\n'
    )
  
  # print(prompt)
  
  
  response <- llm_request(prompt = prompt)

  response
}



user_input_function_specs <- function(){
  answers <- user_input(questions = list(
    list(
      question = 'Function Name (optional)',
      type = 'text',
      default = ''
    ),
    list(
      question = 'Describe the specifications for the function',
      type = 'textarea',
      default = 'Take two numbers as arguments add them togeather then add one'
    ), list(
      question = 'Where to place Function?',
      choices = c('Cursor Location', 'Start of File', 'End of File'),
      default = 'Cursor Location'
    )
  ))
  list(
    function_name = answers$function_name_optional,
    specs = answers$describe_the_specifications_for_the_function,
    location = answers$where_to_place_function
  )
}




#' Generate Function
#'
#' Inserts a function skeleton at the current cursor position.
#'
#' @export
generate_function <- function() {

  answers <- user_input_function_specs()
  response <- generate_function_from_specs(specs  = answers$specs, function_name = answers$function_name)
  context <- active_document_context()
  
  line_number <- 
    if (answers$location == "Cursor Location"){
      context$selection[[1]]$range$start[[1]]
    }else if (answers$location == 'Start of File'){
      1
    }else if (answers$location == 'End of File'){
      context$contents |> length()
    }else {
      stop(glue::glue('unable to insert new function cause of bad location specification `{answers$location}`'))
    }
  
  insert_lines(line = line_number, context = context, text = response)

}



