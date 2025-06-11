

generate_testthat_from_function_details <- function(function_details, context){
  function_details$code
  
  function_comment <- function_details$comment |> txt_single()
  function_comment <- if (function_comment == ''){'<<MISSING>>'}else{function_comment}
  function_code <- function_details$code |> txt_single()
  function_code <- if (function_code == ''){'<<MISSING>>'}else{function_code}
  
  file_extension <- tools::file_ext(basename(context$path))
  
  
  prompt <- 
    paste(
      read_vibe_coder_config('.generic_project_style_guides.config'), 
      glue::glue(read_vibe_coder_config('.generate_tests_prompt.config')),
      
      context_prompts(context =  context),
      collapse = '\n'
    )
  
  
  
  #prompt |> cat()
  response <- llm_request(prompt)
  #response |> cat()
  response
  
}


#' @title Generate Testthat File for a Function
#'
#' @description This function automates the creation of a testthat file for a specified function.
#' @description Generates a testthat file by extracting function details, creating a test file, and appending test code.
#' @details The function uses `function_code_2` to extract details of the function under the cursor in the active RStudio document. It then calls `generate_testthat_from_function_details` to generate the test code.  The generated test code is saved in a file named `test-<function_name>.R` within the `tests/testthat` directory.  If the directory does not exist, it will be created. Finally, the function optionally opens the created test file in RStudio.
#' @return No return value. This function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a function defined in your script, place the cursor
#' # on the line containing the function definition and run this function.
#' generate_testthat()
#' }
#' @export
generate_testthat <- function(){
  #generate_roxygen_comment()
  #generate_testthat()
  context <- active_document_context()
  full_file_text <- context$contents
  function_details <- 
    function_code_2(
      full_text = context$contents, 
      cursor_line = context$selection[[1]]$range$start[["row"]]
    )
  
  response = generate_testthat_from_function_details(function_details = function_details, context = context)
  
  
  fn_name <- function_details$name
  
  test_file_path <- file.path(here::here(), "tests", "testthat", paste0("test-", fn_name, ".R"))
  
  # Ensure the directory exists
  if (!dir.exists(dirname(test_file_path))) {
    dir.create(dirname(test_file_path), recursive = TRUE)
  }
  
  # Append the generated test code to the test file
  cat("\n\n", response, file = test_file_path, append = TRUE)
  
  # Optionally open the test file in RStudio
  rstudioapi::navigateToFile(test_file_path)

}
