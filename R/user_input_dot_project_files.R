

generate_pre_prompt <- function(
    vector_rules,
    prefixes = 'Follow these rules',
    postfixes ="R code below:",
    list_item = ' - '
){
  c(
    prefixes |>
      paste(collapse = '\n'),
    vector_rules |>
      (\(x) paste0(list_item, x))() |>
      paste(collapse = '\n'),
    postfixes |>
      paste(collapse = '\n')
  ) |>
    paste(collapse = '\n')
}




user_input_dot_project_files <- function(path = here::here(),  dialogName = 'Default values to use with this project'){
  
  
  
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
    "Use consistent naming for functions (snake_case or camelCase, pick one).",
    "Write comments starting with # and a space.",
    "Include a blank line between function definitions.",
    "Use function_name <- function(...) to define functions.",
    "Avoid deeply nested code when possible.",
    "Use NA for missing values, not NULL or ''.",
    "Load libraries at the top of the script.",
    "Keep one statement per line.",
    "Avoid using attach() and detach().",
    "Prefer lapply(), sapply(), and purrr functions over loops.",
    "Document functions with comments or roxygen-style blocks.",
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
      "Add @export tag",
      "Add @author with placeholder",
      "Use this exact format and return ONLY the roxygen documentation:",
      "R Function Code:") |>
    generate_pre_prompt(prefixes = c("Generate complete roxygen2 documentation for this R function. ",
                                     "Follow these requirements exactly:\n"),
                        postfixes = "R Function Code:"
    )
  
  
  
  default_test_that_prompt <- c(
    "Cover normal usage, edge cases, and failure conditions",
    "Use `test_that()` and `expect_` functions appropriately",
    "Provide at least 2–3 distinct tests",
    "Assume `library(testthat)` is loaded",
    "Return only valid R code with no explanation or markdown\n"
  ) |>
    generate_pre_prompt(prefixes = c("Generate unit tests for the following R function using the testthat framework.",
                                     "Follow best practices:"),
                        postfixes = "R Function Code:"
    )
  
  
  
  
  
  service_choices <- c("🔑 Gemini" = 'https://ai.google.dev/gemini-api/docs/pricing',
                       "🔑 Claude" = 'https://www.anthropic.com/pricing',
                       "🔑 ChatGPT" = 'https://openai.com/api/pricing/',
                       "🔑 Grok" = 'https://docs.x.ai/docs/models' ,
                       "🔑 Copilot" = 'https://www.microsoft.com/en-us/store/b/copilotpro')
  questions <- list(
    list(question = "🥅 Project Goals?", type = "textarea", default = defualt_project_goals),
    list(question = "🥻 Generic Project style guides?", type = "textarea", default = default_style_guidelines, rows = 10),
    list(question = "📝 generate Tests prompt?", type = "textarea", default = default_test_that_prompt, rows = 10),
    list(question = "📝 generate Roxygen prompt?", type = "textarea", default = default_roxygen_prompt, rows = 10),
    list(question = '🤖 Default LLM service?', choices = names(service_choices))
  )
  
  
  questions_secrets <-
    service_choices |>
    purrr::imap(~{
      paste(.y, 'Secret api key')
    }) |> unname()
  
  all_questions <- c(questions, questions_secrets)
  
  path_coder = path |> file.path('.VibeCodeR')
  
  
  all_questions2 <-
    all_questions |>
    purrr::map(~{
      #.x <- all_questions[[1]]
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
      
      .x$default <-
        if (file.exists(curr_file)){
          readLines(curr_file) |> paste0(collapse = '\n')
        }else if ( 'default' %in% names(.x) ){
          .x$default
        }else {'default'}
      .x
    })
  
  if (!dir.exists(path_coder)) {
    dir.create(path_coder, recursive = TRUE)
  }
  responses <- user_input(questions = all_questions2,dialogName = dialogName, width = 800)
  responses |>
    purrr::iwalk(~{
      print(.y)
      if (nchar(.x) > 0 ){
        writeLines(.x, file.path(path_coder, paste0('.', .y, '.config')))
      }
    })
  
  invisible(responses)
}
