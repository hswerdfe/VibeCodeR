








#' generate_function_from_specs
#'
#' @param specs 
#' @param function_name 
#'
#' @returns the string returned from the LLM
generate_function_from_specs <- function(specs, function_name = ''){
  #specs = 'make some Noise'
  prompt <- 
    paste(
      read_vibe_coder_config('.generic_project_style_guides.config'), 
      read_vibe_coder_config('.generate_function_prompt.config'),
      specs,
      collapse = '\n'
    )
  
  
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
    )
  ))
  list(
    function_name = answers$function_name_optional,
    specs = answers$describe_the_specifications_for_the_function
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
  insert_text(response)
}



