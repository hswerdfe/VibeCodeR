
#' generate_roxygen_comment_from_code
#'
#' @param func_code 
#' @param ... 
#'
#' @returns the string returned from the LLM
generate_roxygen_comment_from_code <- function(
    func_code, 
    ...
){
  prompt <- 
    paste(
      read_vibe_coder_config('.generic_project_style_guides.config'), 
      read_vibe_coder_config('.generate_roxygen_prompt.config'),
      func_code,
      collapse = '\n'
    )
  
  response <- llm_request(prompt)
  
  response
}


#' Generate Roxygen Comment
#'
#' Adds a Roxygen skeleton for the function at the current cursor position.
#'
#' @return Inserts Roxygen comment block above the function.
#' @export
generate_roxygen_comment <- function() {
  
  context <- active_document_context()
  current_line <- context$contents[context$selection[[1]]$range$start["row"]]
  
  # base_functions <- ls("package:base", all.names = TRUE)
  # base_function_names <- base_functions[sapply(base_functions, function(x) is.function(get(x, envir = asNamespace("base"))))]
  # sampled_func_name <- sample(base_function_names, 1)
  # sampled_func <- get(sampled_func_name, envir = asNamespace("base"))
  # func_code <- paste(deparse(sampled_func), collapse = "\n")
  
  
  
  func_code <- function_code(context)
  comment <- generate_roxygen_comment_from_code(func_code = func_code)
  
  #location <- rstudioapi::document_position(context$selection[[1]]$range$start["row"], 1)
  insert_text(text = comment, context = context)
}

