
#' Generate a formatted prompt string with rules and optional headers/footers
#'
#' This function constructs a formatted text prompt by combining a prefix (e.g., instructions),
#' a list of rules (each optionally prefixed with a list item marker), and a postfix (e.g., 
#' a lead-in for code). Each component is joined with newline characters for readability.
#'
#' @param vector_rules A character vector of rule strings to include in the body of the prompt.
#' @param prefixes A character vector or single string to prepend before the rules (default: 'Follow these rules').
#' @param postfixes A character vector or single string to append after the rules (default: 'R code below:').
#' @param list_item A string used to prefix each rule item (default: ' - ').
#' @param collapse_char Collapse the different lines together (default : '\\n').
#'
#' @return A single character string containing the formatted prompt.
#' 
#' @examples
#' generate_pre_prompt(
#'   vector_rules = c("Do not use external libraries", "Use vectorized operations"),
#'   prefixes = "Please follow these coding rules:",
#'   postfixes = "Here is the R code:"
#' )
generate_pre_prompt <- function(
    vector_rules,
    prefixes = 'Follow these rules',
    postfixes ="R code below:",
    list_item = ' - ',
    collapse_char = '\n'
){
  c(
    prefixes |>
      paste(collapse = collapse_char),
    vector_rules |>
      (\(x) paste0(list_item, x))() |>
      paste(collapse = collapse_char),
    postfixes |>
      paste(collapse = collapse_char)
  ) |>
    paste(collapse = collapse_char)
}



#' Collect and Store Default Project Configuration Inputs
#'
#' This function creates and stores a set of default configuration files in a hidden `.VibeCodeR` directory within a project. These configurations include project goals, style guidelines, prompts for documentation and testing, and API service credentials. It presents a user-friendly dialog for input and saves responses to file for reuse.
#'
#' @description
#' Prompts the user (via a UI dialog) to input or confirm default values for various project configuration elements such as goals, style guides, roxygen prompts, unit test prompts, and preferred LLM service credentials. These values are saved as individual config files inside a `.VibeCodeR` directory within the given path.
#'
#' @details
#' The function ensures that consistent and reusable prompts or configuration values are collected at the start of a project. It checks for existing `.config` files, pre-populates the dialog fields with their contents if found, and writes new inputs back to those files. It's ideal for integration with LLM-based tooling or scaffolding systems.
#'
#' @param path Character. Path to the root of the project (default is `here::here()`).
#' @param dialogName Character. Title to be displayed on the user dialog box (default is `"Default values to use with this project"`).
#'
#' @return
#' (Invisibly) returns a named list of responses corresponding to the configuration fields.
#'
#' @examples
#' \dontrun{
#'   responses <- user_input_dot_project_files()
#'   responses$project_goals
#' }
#'
#' @export
#' @author Howard Swerdfeger
user_input_dot_project_files <- function(
    path = here::here(),  
    dialogName = 'Default values to use with this project',
    path_coder = path |> file.path('.VibeCodeR')
){
  
  
  
  default_pre_prompt <- paste0('You are a world leading expert in all aspects of R programming and all libraries in CRAN, you are currently running {R.version.string}, and are running from within RStudio IDE.')
  
  
  defualt_project_goals <- "Type the Goals of your project eg. [`A shiny app that ....`, 'Wrapper around the api ....', 'allow easy generation of synthetic data ... ']"
  
  
  
  default_style_guidelines <- c(
    "For the most part use the tidyverse style guide found at https://style.tidyverse.org/",
    "Use lowercase letters and underscores for variable names (snake_case).",
    "Indent code with two spaces, not tabs.",
    "Keep lines shorter than 80 characters.",
    "Add a space after commas and around operators (x + y, not x+y).",
    "Use TRUE and FALSE (not T and F).",
    "Put a space before and after control structures (if, for, while).",
    "Use consistent snake_case naming for functions",
    "Write comments starting with # and a space.",
    "Include a blank line between function definitions.",
    "Use function_name <- function(...) to define functions.",
    "Avoid deeply nested code when possible.",
    "Use NA for missing values, not NULL or ''.",
    "Load libraries at the top of the script, but only if needed.",
    "Keep one statement per line.",
    "Avoid using attach() and detach().",
    "Prefer lapply(), sapply(), and purrr functions over loops.",
    "When Documenting functions with comments or roxygen-style blocks.",
    "Use library() instead of require() in scripts.",
    "End scripts with a newline.",
    "Use fully qualified names (e.g., purrr::map()) to avoid conflicts between packages.",
    "Use native pipe |> over %>% but only when appropriate",
    'have a slight prefrence for commonly used packages over less used packages',
    'have a prefrence for already refrenced libraries over not loaded libraries'
  ) |>
    generate_pre_prompt(prefixes = "Follow these rules as generic style guideline when they make sense:",
                        postfixes = ""
    )
  
  
  
  
  default_roxygen_prompt <-
    c("Create a descriptive title based on the function name and purpose",
      "Write a detailed description explaining what the function does",
      "Add @description with a brief one-line summary",
      "Add @details with implementation notes and usage patterns",
      "Document ALL parameters with @param, including their types and detailed descriptions",
      "Add @return with specific return type and description",
      "Create realistic @examples with actual working R code (use \\\\dontrun{} wrapper)",
      #"Add @export tag",
      #"Add @author with placeholder",
      "Use this exact format and return ONLY the roxygen documentation:",
      "R Function Code:") |>
    generate_pre_prompt(prefixes = c("Generate complete roxygen2 documentation for this R function. ",
                                     "Follow these requirements exactly:\n"),
                        postfixes = "R Function Code:"
    )
  
  
  default_function_generation_prompt <-
    c("Output only valid R code, with an appropriate but minimal amout of comments using # ",
      "Do NOT include any roxygen2 documentation block block at the top",
      "Do NOT wrap the code in any human readable explaination or markup", 
      "Assume the libraries neede are already loaded",
      "if and only if external libraries are used add a comment near the top after <-function in the form # require(<<libarry_name>>)",
      "add a comment at the top AFTER <-function() summarizing the specification in concise and clear way",
      "The first line should be something like my_function_name <- function",
      "The produced code will be sent directy to a .R file", 
      "Give the R function a name that is appropriate for its purpose"
      ) |> 
    generate_pre_prompt(prefixes = c("Create an R function that Follows best practices:"),
                        postfixes = "R Function Specifications:"
    )
  
  
  default_test_that_prompt <- c(
    "Cover normal usage, edge cases, and failure conditions",
    "Use `test_that()` and `expect_` functions appropriately",
    "Provide at least 2–3 distinct tests",
    "Assume `library(testthat)` is loaded",
    "Return only valid R code with no explanation or markdown"
  ) |>
    generate_pre_prompt(prefixes = c("Generate unit tests for the following R code using the testthat framework.",
                                     "Follow best practices:"),
                        postfixes = "R Code:"
    )
  
  
  
  default_refactor_prompt <- 
    paste(
      c(
        "Try to keep changes minimal",
        "Return only valid R code with no explanation or markdown",
        "Return an exact replacement for the indicated code",
        "Your code will directly and exactly replace the indicated code"
      ) |>
      generate_pre_prompt(prefixes = c("Changes are required to the code below Generic Instructions are:"),
                          postfixes = ""
      ), 
      
      generate_pre_prompt(vector_rules = '{user_specific_instructions}',
                          prefixes = c("Specific instruction for change are:"),
                          postfixes = "R Code:"
      )  
    )
  
  
  
  service_choices <- llm_models_all()
  
  

  questions <- list(
    list(question = "🥅 Project Goals?", type = "textarea", default = defualt_project_goals),
    list(question = "🥻 Generic Project style guides?", type = "textarea", default = default_style_guidelines, rows = 10),
    list(question = "📝 Generate Tests prompt?", type = "textarea", default = default_test_that_prompt, rows = 10),
    list(question = "📝 Generate Roxygen prompt?", type = "textarea", default = default_roxygen_prompt, rows = 10),
    list(question = "📝 Generate Function prompt?", type = "textarea", default = default_function_generation_prompt, rows = 10),
    list(question = "🔧 Refactor Code Prompt?", type = "textarea", default = default_refactor_prompt, rows = 10),
    list(question = '🤖 Default LLM service?', choices = service_choices),
    list(question = 'Include Context Libraries in Memmory?',                        type = "logical", default = TRUE),
    list(question = 'Include Context Libraries Refrenced In Other Parts Of Code?',  type = "logical", default = TRUE),
    list(question = 'Include Context Currently Selected File?',                     type = "logical", default = TRUE),
    list(question = 'Include Context All Code in project?',                         type = "logical", default = FALSE),
    list(question = 'Include Context Functions In Memory?',                         type = "logical", default = FALSE)
  )
  
  
  questions_secrets <-
    service_choices |>
    purrr::map(~{
      paste(.x, 'Secret api key')
    }) |> unname()
  
  
  
  
  #all_questions <- c(questions)#, questions_secrets)
  

  
  questions2 <-
    questions |>
    purrr::map(~{
      #.x <- all_questions[[6]]
      .x <-
        if (! is.list(.x)){
          list(
            question = .x,
            type = "text",
            default = ''
          )
        }else{.x}
      curr_q <- .x$question
      curr_file <- file.path(path_coder, paste0('.', janitor::make_clean_names(curr_q), '.config'))
      
      
      .x$default <- read_vibe_coder_config_file(curr_file, default = .x$default)
      .x
    })
  
  if (!dir.exists(path_coder)) {
    dir.create(path_coder, recursive = TRUE)
  }
  

  
  responses <- user_input(questions = questions2,dialogName = dialogName, width = 800)
  
  
  responses |>
    purrr::iwalk(~{
      # .x <- responses[[7]]
      # .y = names(responses)[[7]]
      file_name = file.path(path_coder, paste0('.', .y, '.config'))
      write_vibe_coder_config_file(file_name = file_name, values = as.character(.x))
    })
  
  invisible(responses)
}
