
# library(httr2)
# library(janitor)
# library(snakecase)
# library(stringr)
# library(glue)
# library(ellmer)
# library(magrittr)





#' List Available Chat Services in the ellmer Package
#'
#' This function lists the available chat services provided by the 'ellmer' package.
#' @description Lists available chat services from the ellmer package.
#' @details This function retrieves a list of chat service functions within the 'ellmer' package.
#'   It identifies functions starting with "chat_", removes the prefix, replaces underscores with spaces,
#'   and converts the resulting strings to title case for better readability. This provides a user-friendly
#'   list of available chat service options.
#' @return character vector. A character vector containing the names of available chat services in title case.
#' @examples
#' \dontrun{
#' # List available chat services
#' llm_services()
#' }
llm_services <- function(){
  pkg = "ellmer"
  pattern = "^chat_"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    base::stop(base::sprintf("Package '%s' is not installed.", pkg))
  }
  
  base::getNamespaceExports(pkg) |>
    stringr::str_subset(pattern = pattern) |>
    stringr::str_remove(pattern) |>
    stringr::str_replace_all('_', ' ') |>
    stringr::str_to_title()
}




#' List Available LLM Models for a Given Service
#'
#' This function retrieves a list of available large language models (LLMs) for a specified service.
#' @description Retrieves a list of available LLM model IDs for a given service.
#' @param service A character string specifying the LLM service provider (e.g., "Google Gemini"). Case-insensitive and spaces are allowed.
#' @details This function dynamically calls a service-specific function (e.g., `models_google_gemini`) to fetch the available models.
#'   The `service` parameter is converted to lowercase, spaces are replaced with underscores, and then prepended with "models_" to form the name of the service-specific function.
#'   The service-specific function is expected to return a list or data frame containing model information, including an "id" column.
#'   The function extracts and returns only the model IDs.
#'
#'   Usage patterns:
#'   - Ensure that a service-specific function (e.g., `models_google_gemini`) exists and is accessible in the environment.
#'   - The service-specific function should return a data structure with an "id" column representing the model IDs.
#' @return A character vector containing the IDs of the available LLM models for the specified service.
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' # Assuming a function 'models_google_gemini' exists that returns a data frame
#' # with an 'id' column containing model IDs.
#' # Example 'models_google_gemini' function (replace with your actual implementation):
#' models_google_gemini <- function() {
#'   data.frame(id = c("gemini-1.0-pro", "gemini-1.5-pro"),
#'              description = c("Gemini 1.0 Pro model", "Gemini 1.5 Pro model"))
#' }
#'
#' llm_models("Google Gemini")
#' llm_models("Huggingface")
#' # Returns: c("gemini-1.0-pro", "gemini-1.5-pro")
#'
#' # Example with a different service (assuming 'models_openai' exists):
#' # llm_models("OpenAI")
#' }
llm_models <- function(service){
  #service = 'Google Gemini'
  #service = "Huggingface"
  models_function_name <- 
    service |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(' ', '_') %>%
    base::paste0('models_', .)
  
  models_func <- 
    base::tryCatch(
      models_function_name |> 
        base::get()  ,
      error = \(e){
        warning(glue::glue('Error getting function `{models_function_name}``, ie no models for service `{service}`'))
        \()(tibble::tibble(id = character  ())  )
      }
    )
  
  models_func() |>magrittr::extract2('id')
    
}






#' List All Available services with language Models
#'
#' Retrieves a list of all available services models within the 'ellmer' package.
#' @description Lists all language models available in the 'ellmer' package.
#' @details This function identifies services models by listing objects within the 'ellmer' package that match the pattern "models\_". It then removes the "models\_" prefix, replaces underscores with spaces, and converts the resulting strings to title case for improved readability. This provides a user-friendly listing of available models.
#' @return character A character vector containing the names of all available language models.
#' @examples
#' \dontrun{
#' # List all available language models
#' llm_models_all()
#' }
llm_models_all <- function(){
  pkg = "ellmer"
  pattern = "^models_"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    base::stop(base::sprintf("Package '%s' is not installed.", pkg))
  }
  
  getNamespaceExports(pkg) |>
    stringr::str_subset(pattern = pattern) |>
    stringr::str_remove( pattern) |>
    stringr::str_replace_all('_', ' ') |>
    stringr::str_to_title()
  
  
}






#' Default LLM object
llm_default <- function(...){
  # TODO : hook upto config file
  pkg <- 'ellmer'
  pattern = '^chat_'
  base::paste0(
    pkg, '::',
    base::getNamespaceExports(pkg) |>
      stringr::str_subset(pattern = pattern) 
  )
  
  
  
  function_name <- 
    base::paste0('chat_', 
      read_vibe_coder_config('.default_llm_service.config') |>
        snakecase::to_snake_case()
    ) 
  
  
  
  
  func <- utils::getFromNamespace(function_name, pkg)
  func(...)
}


llm_request <- function(
    prompt, 
    chat_func_maker = llm_default,
    ...
){
  # prompt <- glue::glue('tell me a fictional story in less then {sample(seq(10,50), 1)} words, about {sample(c("history","science fiction","fantasy", "love", "life", "work", "comedy", "sports", "crime"), 1)}.')
  # prompt <- glue::glue('Pick a random famous {sample(c("male", "female"), 1)} from {sample(c("Canada", "USA", "Mexico", "Brasil", "UK", "Germany", "China", "India", "Rusia", "Japan"), 1)} in the area of {sample(c("Music", "Acting", "Sports","technology","Influencer", "Modeling", "Charitable Work", "Writing", "Politics"), 1)} and tell me their birthday.')
  chat <-  chat_func_maker(...)
  llm_response <- chat$chat(prompt)
  
  llm_response_cleaned <- 
    llm_response |> 
    stringr::str_trim() |>
    stringr::str_remove("^```(r|R|roxygen2|markdown)?\n") |>
    stringr::str_remove("\n```$") 
  llm_response_cleaned
}




# llm_request(prompt )





