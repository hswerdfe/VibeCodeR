

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
#'   and `libraries_extract_files()`). The context prompt is then constructed by
#'   concatenating relevant information with section splitters.
#' @param context A list containing context information, typically obtained from
#'   `rstudioapi::getSourceEditorContext()`. It must contain at least a
#'   `contents` element (a character vector representing the lines of the
#'   current document).
#' @param path The base path for the `.VibeCodeR` directory. Defaults to the
#'   project root directory using `here::here()`.
#' @param config_path The full path to the `.VibeCodeR` directory. Defaults to
#'   `here::here()/.VibeCodeR`.
#' @param section_splitter The character string used to split sections of
#'   context. Defaults to `\\n---------------------------------\\n`.
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
    config_path = path |> file.path('.VibeCodeR')  ,
    section_splitter = '\n---------------------------------\n'
){
  #context <- active_document_context()
  #generate_roxygen_comment()
  
  
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





#' Generate a Context String Representing the Current Environment
#'
#' This function creates a string that describes variables and their
#' properties within a specified environment. It's useful for providing
#' context in debugging or automated documentation.
#' @description Generates a string summarizing variables in an environment.
#' @details The function lists variables in the given environment (`.envir`),
#'   retrieving properties like class, length, dimensions (if applicable),
#'   and names (if applicable). For functions, it also includes the function
#'   header. The generated string includes a prefix and suffix for context.
#'   The final string is truncated to `max_context_length`.
#' @param .envir The environment to inspect. Defaults to the parent frame
#'   (the environment from which the function is called).
#' @param variable_seperator Character string used to separate information
#'   about different variables. Defaults to `\\n\\n`.
#' @param key_val_sep Character string used to separate keys and values in
#'   the property descriptions. Defaults to ` => `.
#' @param property_sep Character string used to separate properties of a
#'   single variable. Defaults to `\\n`.
#' @param prefix Character string to prepend to the context string. Defaults
#'   to a descriptive header.
#' @param suffix Character string to append to the context string. Defaults
#'   to a descriptive footer.
#' @param max_context_length Maximum length of the generated context string.
#'   Longer strings are truncated. Defaults to 10000.
#'
#' @return A character string containing a description of the variables
#'   and their properties in the specified environment.
#'
#' @examples
#' \dontrun{
#' # Generate context for the global environment
#' context_string <- environment_context(.envir = .GlobalEnv)
#' cat(context_string)
#'
#' # Generate context for a custom environment
#' my_env <- new.env()
#' my_env$x <- 1:10
#' my_env$y <- "hello"
#' context_string <- environment_context(.envir = my_env)
#' cat(context_string)
#' }
environment_context <- function(
    .envir = parent.frame(), 
    variable_seperator = '\n\n', 
    key_val_sep = ' => ',
    property_sep = '\n',
    prefix = "\n## For Context, below are variables and properties,in the environment: \n",
    suffix = '\n## End partial list of context variables from the envitonment.',
    max_context_length = 10000L
){
  #context <- active_document_context()
  #generate_roxygen_comment()
  nms <- ls(envir = .envir)
  
  vars_str <- 
    nms |> 
    vapply(\(.nm){
      #.nm <- "i"is_blank_comment"#sample(nms ,1)
      .obj <- .nm |> get( envir = .envir) 
      .cls <- .obj |> class()
      items <- 
        list(
          variable  =  .nm,
          class     = .cls[[1]],
          length    = length(.obj)
        )
      
      items[['dimensions']] <- if ( ! is.null(dim(.obj))) {paste0(dim(.obj), collapse = ' x ')}else{NULL}
      items[['names']] <- if ( ! is.null(names(.obj))) {paste0(names(.obj), collapse = ' , ')}else{NULL}
      
      
      if (is.function(.obj)){
        .fun_header <- 
          .obj |>
          deparse1(nlines = 2) |>
          trimws()
        
        items[['function header']] <- .fun_header
      }       
      

        
      
      
      Map(f = \(.x, .nm){
        paste0(.nm, key_val_sep , as.character(.x), '')  
        
      }, items, names(items)) |>
        unlist() |>
        unname() |>
        paste(collapse = property_sep)# |>
      #cat()
    },
    FUN.VALUE = character(1)
    ) |>
    paste(collapse = variable_seperator) 
  
  paste(
    prefix, 
    substr(vars_str, 1, max_context_length),
    suffix
  )
}


#' Generate a Contextual Error Message
#'
#' This function retrieves the last error message and formats it with a prefix and suffix.
#' @description Formats the last error message for inclusion in prompts.
#' @details This function uses `geterrmessage()` to obtain the last error
#'   message and then concatenates it with a specified prefix and suffix.
#'   The resulting string is then processed by `txt_single()` to ensure it is
#'   a single line. This is useful for providing context about recent errors
#'   in code generation or debugging scenarios.
#' @param prefix Character string to prepend to the error message.
#'   Defaults to "\\n## The most recent error message  is :\\n".
#' @param suffix Character string to append to the error message.
#'   Defaults to '\\n## End error message.'.
#' @return A character string containing the formatted error message.
#' @examples
#' \dontrun{
#' # Force an error
#' try(log("a"))
#'
#' # Get the formatted error message
#' error_message <- context_error_message()
#'
#' # Print the error message
#' cat(error_message)
#' }
context_error_message <- function(
    prefix = "\n## The most recent error message  is :\n",
    suffix = '\n## End error message.'
  ){
  paste(
    prefix, 
    geterrmessage()  ,
    suffix
  ) |>
    txt_single()
}





context_highlighted_message <- function(
      prefix = "\n## The text highlighted by the user that they think is important is :\n",
      suffix = '\n## text highlighted by the user.' ,
      clipboard_content = NULL
    ){
  
  highlighted_msg <-
    if ( ! clipr::clipr_available() & is.null(clipboard_content)){
      message("No content in clipboard. Please copy console text first (Ctrl+C)")
      return('')
    } else {
      if ( is.null(clipboard_content) ){
        clipr::read_clip()  
      }else{
        clipboard_content
      }
    }
  paste(
    prefix, 
    highlighted_msg  ,
    suffix
  ) |>
    txt_single()
  
}




#' Convert a Named List to a Context String
#'
#' This function converts a named list into a formatted string, useful for
#' providing context about data structures in debugging or documentation.
#' @description Converts a named list to a string representation.
#' @details The function takes a named list and formats it into a string
#'   where each element is represented as "key => value". The resulting
#'   string is prefixed and suffixed for context. `txt_single()` is used to
#'   ensure the output is a single line string.
#' @param lst A named list to convert to a string.
#' @param prefix Character string to prepend to the string. Defaults to
#'   "\\n## For Context this about a named list:\\n".
#' @param suffix Character string to append to the string. Defaults to
#'   '\\n## End context named list.'.
#' @param key_value_joiner Character string to join keys and values.
#'   Defaults to ' => '.
#' @param ... Additional arguments (currently not used).
#' @return A character string containing the formatted named list.
#' @examples
#' \dontrun{
#' # Create a named list
#' my_list <- list(a = 1, b = "hello", c = TRUE)
#'
#' # Convert the list to a context string
#' context_string <- named_lst_to_string(my_list)
#'
#' # Print the context string
#' cat(context_string)
#' }
named_lst_to_string <- function(
    lst,
    prefix = "\n## For Context this about a named list:\n",
    suffix = '\n## End context named list.',
    key_value_joiner = ' => ',
    ...
){
  paste0(
    prefix,
    lst |> 
      unlist() |>
      purrr::imap(\(.v, .k){
        paste0(.k, key_value_joiner, .v)
      }) |>
      unname() |>
      unlist() |>
      txt_single(),
    suffix
  )
}



#' Generate a Formatted Traceback String
#'
#' This function captures and formats the traceback of the most recent error
#' into a single-line string with specified prefix and suffix.
#' @description Captures the traceback and formats it as a single-line string.
#' @details This function calls `traceback()` to get the traceback of the last
#'   error, then uses `capture.output()` to convert the traceback information
#'   into a character vector.  The resulting character vector is collapsed into
#'   a single string using `txt_single()`, and then wrapped with the specified
#'   prefix and suffix. The purpose is to provide a concise, formatted
#'   traceback that can be easily included in context prompts or debugging
#'   messages.
#' @param prefix Character string to prepend to the traceback message.
#'   Defaults to "\\n## traceback of the most recent error is :\\n".
#' @param suffix Character string to append to the traceback message.
#'   Defaults to '\\n## End traceback message.'.
#' @return A character string containing the formatted traceback.
#' @examples
#' \dontrun{
#' # Generate an error to create a traceback
#' try(log("a"))
#'
#' # Get the formatted traceback
#' traceback_message <- context_traceback()
#'
#' # Print the traceback message
#' cat(traceback_message)
#' }
context_traceback <- function(
    prefix = "\n## traceback of the most recent error is :\n",
    suffix = '\n## End traceback message.'
){
  trace <- 
    traceback() |>
    utils::capture.output() |>
    txt_single() 
  paste(
    prefix, 
    trace ,
    suffix
  ) |>
    txt_single()
}








#' Generate a String Containing the Recent R Command History
#'
#' This function saves the R command history to a temporary file, retrieves the
#' most recent commands, and formats them into a single string, prepended with a
#' specified prefix and appended with a suffix.
#' @description Creates a string containing the recent R command history.
#' @details The function, extracts the `n_commands` most recent commands, and prepends each
#'   command with its index. The commands are then combined into a single
#'   string using `txt_single()` and wrapped with the specified `prefix` and
#'   `suffix`. This is useful for providing context about recent actions in the
#'   R environment.
#' @param prefix Character string to prepend to the command history.
#'   Defaults to "\\n## For Context the most recent list of commands run are:\\n".
#' @param suffix Character string to append to the command history.
#'   Defaults to '\\n## End history of commands run.'.
#' @param n_commands Integer specifying the number of recent commands to
#'   retrieve. Defaults to 10.
#' @return A character string containing the formatted recent R command history.
#' @examples
#' \dontrun{
#' # Get the recent command history
#' history_string <- context_history()
#'
#' # Print the history string
#' cat(history_string)
#' }
context_history <- function(
    prefix = "\n## For Context the most recent list of commands run are :\n",
    suffix = '\n## End history of commands run.',
    n_commands = 10  
  ){
  tmp_hist_file <- tempfile()
  
  utils::savehistory(tmp_hist_file)
  
  recent_cmds <- 
    paste(
      paste0(n_commands:1, '.'),
    tmp_hist_file |> 
    readLines() |>
    utils::tail(n_commands) 
    )
  
  
  paste0(
    prefix,
    recent_cmds |> txt_single(),
    suffix
  )
}




#' Generate a Context String Containing R Version Information
#'
#' This function extracts and formats the R version information into a string
#' suitable for providing context in debugging or code generation scenarios.
#' @description Creates a formatted string of R version details.
#' @details This function retrieves the `R.version` list, converts it into a
#'   formatted string using `named_lst_to_string()`, and wraps it with the
#'   specified `prefix` and `suffix`. This provides a concise way to include
#'   information about the R environment in context prompts or debugging
#'   messages.
#' @param prefix Character string to prepend to the R version information.
#'   Defaults to "\\n## For Context this information about the R version :\\n".
#' @param suffix Character string to append to the R version information.
#'   Defaults to '\\n## End of context for R version.'.
#' @param ... Additional arguments passed to `named_lst_to_string()`.
#' @return A character string containing the formatted R version information.
#' @examples
#' \dontrun{
#' # Get the R version context string
#' r_version_string <- context_r_version()
#'
#' # Print the R version string
#' cat(r_version_string)
#' }
context_r_version <- function(
    prefix = "\n## For Context this information about the R version :\n",
    suffix = '\n## End of context for R version.',
    ...
){
  R.version |> 
    named_lst_to_string(
      prefix = prefix,
      suffix = suffix,
      ...
    )
}



#' Generate a String Containing System Information
#'
#' This function retrieves system information using `Sys.info()` and formats
#' it into a string for providing context in debugging or logging scenarios.
#' @description Creates a formatted string of system information.
#' @details This function calls `Sys.info()` to get system details and then
#'   uses `named_lst_to_string()` to format this information into a string,
#'   wrapped with the specified `prefix` and `suffix`. This allows you to
#'   easily include system details (like operating system, user, and hostname)
#'   in context prompts, error messages, or log files.
#' @param prefix Character string to prepend to the system information.
#'   Defaults to "\\n## For Context this information about the system :\\n".
#' @param suffix Character string to append to the system information.
#'   Defaults to '\\n## End of context for system info.'.
#' @param ... Additional arguments passed to `named_lst_to_string()`.
#' @return A character string containing the formatted system information.
#' @examples
#' \dontrun{
#' # Get the system info context string
#' system_info_string <- context_system_info()
#'
#' # Print the system info string
#' cat(system_info_string)
#' }
context_system_info <- function(
    prefix = "\n## For Context this information about the system :\n",
    suffix = '\n## End of context for system info.',
    ...
    ){
  Sys.info() |>
    named_lst_to_string(
      prefix = prefix,
      suffix = suffix,
      ...
    )
}



#' Generate a String Containing Platform Information
#'
#' This function extracts and formats the `.Platform` list into a string
#' suitable for providing context about the operating platform.
#' @description Creates a formatted string of platform details.
#' @details This function retrieves the `.Platform` list, converts it into a
#'   formatted string using `named_lst_to_string()`, and wraps it with the
#'   specified `prefix` and `suffix`. This provides a concise way to include
#'   information about the platform the code is running on in context prompts or debugging
#'   messages.
#' @param prefix Character string to prepend to the platform information.
#'   Defaults to "\\n## For Context this information about the platform :\\n".
#' @param suffix Character string to append to the platform information.
#'   Defaults to '\\n## End of context for platform.'.
#' @param ... Additional arguments passed to `named_lst_to_string()`.
#' @return A character string containing the formatted platform information.
#' @examples
#' \dontrun{
#' # Get the platform context string
#' platform_string <- context_platform()
#'
#' # Print the platform string
#' cat(platform_string)
#' }
context_platform <- function(
    prefix = "\n## For Context this information about the platform :\n",
    suffix = '\n## End of context for platform.',
    ...
){
  .Platform |>
  named_lst_to_string(
    prefix = prefix,
    suffix = suffix,
    ...
  )
}




