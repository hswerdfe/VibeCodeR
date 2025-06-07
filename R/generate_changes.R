


generate_changes_from_code <- function(selected_code = NULL, instructions = NULL){
  
  questions <- list(
    list(question = "Selected Code.", type = "textarea", default = selected_code),
    list(question = "Instructions?", type = "textarea", default = instructions, rows = 10)
  )
  user_instructions <- user_input(questions)
  
  selected_code <- user_instructions$selected_code
  user_specific_instructions <- user_instructions$instructions
  
  prompt <- 
    paste(
      read_vibe_coder_config('.generic_project_style_guides.config'), 
      glue::glue(read_vibe_coder_config('.refactor_code_prompt.config')),
      '\n',
      selected_code,
      collapse = '\n'
    )  
  response <- llm_request(prompt = prompt)
  # cat(prompt)
  # cat(response)
  user_reponse <- show_diff_gadget(original = selected_code, refactored = response, instructions_given = user_specific_instructions)
  if (user_reponse == 'accept'){
    return(response)
  }else{
    message('The user has rejected the changes ')
    return(NULL)
  }
}










#' Generate Code Changes Based on Cursor Selection
#'
#' This function generates code changes based on the code currently selected in the RStudio cursor.
#' @description Generates and inserts code changes based on the cursor selection.
#' @details This function first retrieves the code selected by the cursor using `cursor_selection()`.
#' It then passes this selected code to `generate_changes_from_code()` to generate the suggested changes.
#' Finally, it inserts the generated changes into the RStudio editor using `insert_text()`.
#' This function assumes the existence of `cursor_selection()`, `generate_changes_from_code()`, and `insert_text()` functions.
#'
#' @return None. This function does not return a value; it directly modifies the RStudio editor.
#' @examples
#' \dontrun{
#' # This example assumes you have some code selected in the RStudio editor.
#' generate_changes()
#' # The selected code will be replaced with the generated changes.
#' }
#' @export
#' @author Placeholder
generate_changes <- function(){
  selected_code <- cursor_selection() 
  response <- generate_changes_from_code(selected_code = selected_code)
  insert_text(text = response)
}