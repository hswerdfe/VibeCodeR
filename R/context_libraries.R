




#' List Installed R Packages
#'
#' This function retrieves a list of all installed R packages.
#' @description Lists all installed R packages.
#' @details This function uses the `library()` function to get a list of installed packages and extracts the package names from the results. It returns a character vector containing the names of the installed packages. This function is useful for determining which packages are available for use in your R environment.
#' @return character A character vector containing the names of all installed R packages.
#' @examples
#' \dontrun{
#' # Get a list of installed packages
#' installed_packages <- libraries_installed()
#'
#' # Print the list of installed packages
#' print(installed_packages)
#'
#' # Check if a specific package is installed
#' if ("dplyr" %in% installed_packages) {
#'   print("dplyr is installed")
#' } else {
#'   print("dplyr is not installed")
#' }
#' }
libraries_installed <- function(){
  base::library()$results[,1]  
}






#' List Packages Loaded in Memory
#'
#' This function identifies and lists all packages currently loaded in the R environment.
#'
#' @param package_prefix 
#'
#' @description Returns a character vector of loaded package names.
#' @details The function achieves this by examining the search path and extracting entries that begin with "package:". It then removes the "package:" prefix to return a clean list of package names. This is useful for determining which packages are actively available for use without explicitly listing them in the code.
#' @return character A character vector containing the names of all packages currently loaded in memory.
#' @examples
#' \dontrun{
#' # List all packages currently loaded
#' loaded_packages <- libraries_in_memmory()
#' print(loaded_packages)
#'
#' # Check if a specific package is loaded
#' if ("dplyr" %in% libraries_in_memmory()) {
#'   print("dplyr is loaded.")
#' } else {
#'   print("dplyr is not loaded.")
#' }
#' }
libraries_in_memmory <- function(package_prefix = "^package:"){
  base::search() |>
    stringr::str_subset(package_prefix) |>
    stringr::str_remove_all(package_prefix)
}





#' Extract Package Names from R Code
#'
#' This function extracts the names of packages used in a given R code string.
#' @description Extracts package names from a string of R code.
#' @param code_string A character string containing R code.
#' @return A character vector containing the unique names of the packages used in the code.
#' @details
#' The function parses the input R code string to identify package dependencies. It searches for calls to `library()`, `require()`, and package::function calls.
#' It removes comments from the code before parsing. The function returns a vector of unique package names.
#'
#' @examples
#' \dontrun{
#' code_string <- "
#' # Load necessary libraries
#' library(dplyr)
#' require(ggplot2)
#' 
#' # Use functions from these packages
#' data <- dplyr::mutate(data.frame(x = 1:10), y = x^2)
#' ggplot2::ggplot(data, aes(x, y)) + ggplot2::geom_point()
#' "
#' 
#' libraries_extract(code_string)
#' # Returns: c("dplyr", "ggplot2")
#' 
#' code2 <- "
#' library(stats)
#' lm(formula = y ~ x, data = data.frame(x = 1:5, y = 2*(1:5) + rnorm(5)))
#' "
#' libraries_extract(code2)
#' # Returns: "stats"
#' 
#' code3 <- "
#' # No libraries loaded
#' x <- 1:10
#' y <- x^2
#' "
#' libraries_extract(code3)
#' # Returns: character(0)
#' }
libraries_extract <- function(code_string) {
  # Remove comments
  
  code_clean <- 
    code_string |>
    stringr::str_split('\n') |>
    base::unlist() |>
    stringr::str_remove_all("#.*$") |>
    purrr::keep(~{nchar(.x) > 0}) |>
    base::paste0(collapse = '\n')
  

  
  
  # Find all library/require calls with various formats
  patterns <- c(
    "library\\s*\\(\\s*(['\"]?)([^'\"\\),]+)\\1",
    "require\\s*\\(\\s*(['\"]?)([^'\"\\),]+)\\1",
    "([a-zA-Z0-9._]+)::([a-zA-Z0-9._]+)"  # package::function calls
  )
  
  packages <- c()
  
  for (pattern in patterns) {
    matches <- base::regmatches(code_clean, base::gregexpr(pattern, code_clean, perl = TRUE))[[1]]
    if (length(matches) > 0) {
      if (base::grepl("::", pattern)) {
        # Extract package names from package::function
        pkg_names <- base::gsub("([a-zA-Z0-9._]+)::.*", "\\1", matches)
      } else {
        # Extract from library/require calls
        pkg_names <- base::gsub(pattern, "\\2", matches, perl = TRUE)
      }
      packages <- c(packages, pkg_names)
    }
  }
  
  return(unique(packages[packages != ""]))
}







#' Extract Libraries from a File
#'
#' This function extracts R library names from a specified file.
#' @description Extracts R library names from a file.
#' @details The function reads the entire content of the specified file, collapses it into a single string, and then uses `libraries_extract_parse()` to extract the library names. This is useful for identifying the dependencies of an R script or project by analyzing its source code.  It assumes that the file contains R code and that library calls are made using `library()` or `require()`.
#' @param file_name (character) The name of the file to extract library names from. This should be a character string representing the path to the file.
#' @return (character) A character vector containing the names of the libraries extracted from the file.
#' @examples
#' \dontrun{
#' # Create a dummy R file
#' cat("library(dplyr)\nrequire(ggplot2)\n", file = "temp.R")
#'
#' # Extract libraries from the file
#' libraries <- libraries_extract_file("temp.R")
#' print(libraries)
#'
#' # Clean up the dummy file
#' file.remove("temp.R")
#' }
libraries_extract_file <- function(file_name){
  #file_name <- "C:/Users/swerdfeh/projects/VibeCodeR/R/context_libraries.R"
  code_string <- 
    file_name |>
    base::readLines() |>
    base::paste0(collapse = '\n') 
  
  code_string |>
    libraries_extract()
}






#' Extract Required Libraries from R Files in a Directory
#'
#' This function extracts the names of required R libraries from all R files within a specified directory.
#' @description Extracts library names from R files in a directory.
#' @details This function recursively searches for R files (defaulting to files ending with ".R") within a given directory. It then uses `libraries_extract_file` to extract library names from each file. The results are combined, duplicates are removed, and a vector of unique library names is returned.  It leverages `list.files` for file discovery and `purrr::map` for applying `libraries_extract_file` to each file.
#' @param dir character. The directory to search for R files. Defaults to the 'R' subdirectory of the project root (using `here::here()`).
#' @param pattern character. A regular expression pattern to match R files. Defaults to "\\.R$".
#' @return character. A character vector containing the unique names of all libraries required by the R files in the specified directory.
#' @examples
#' \dontrun{
#' # Extract libraries from the default 'R' directory
#' libraries <- libraries_extract_files()
#' print(libraries)
#'
#' # Extract libraries from a specific directory with a custom pattern
#' libraries <- libraries_extract_files(dir = "path/to/my/R/files", pattern = "\\.r$")
#' print(libraries)
#' }
libraries_extract_files <- function(
    dir =   here::here() |> file.path('R'),
    pattern = "\\.R$"
){
  
  dir |> 
    base::list.files(recursive = TRUE, 
             full.names = TRUE,
             pattern = pattern
  ) |>
    purrr::map(~{
      libraries_extract_file(.x)
    }) |>
    base::unlist() |>
    base::unique()
}







#' List Names of Functions Currently in Memory
#'
#' This function identifies and returns the names of all functions currently loaded in the R environment.
#' @description Returns a character vector of function names in memory.
#' @details
#' The function works by first listing all objects in the current environment using `base::ls()`.
#' It then iterates through these objects, checking if each one is a function using `base::is.function()`.
#' Finally, it returns the names of the objects that are identified as functions.
#' This function is useful for introspection and debugging, allowing you to see which functions are currently available for use.
#' It leverages `purrr` for concise iteration and filtering.
#' @param .env Environment to search for functions. Defaults to the global environment.
#' @return character A character vector containing the names of all functions currently in memory.
#' @examples
#' \dontrun{
#' # List all functions in the current environment
#' function_names <- functions_in_memory()
#' print(function_names)
#'
#' # Check if a specific function is in memory
#' if ("mean" %in% functions_in_memory()) {
#'   print("The 'mean' function is in memory.")
#' } else {
#'   print("The 'mean' function is not in memory.")
#' }
#' }
functions_in_memory <- function(.env = .GlobalEnv){
  base::ls(envir = .env) |>
    purrr::set_names() |>
    purrr::imap(~{
      .x |>
      base::get(envir = .env) |>
      base::is.function()
    }) |>
    purrr::keep(~{.x}) |>
    base::names()
}






#' List Functions and Libraries Loaded in Memory
#'
#' This function identifies and lists all functions currently loaded in memory from attached packages (excluding the base package).
#' @description Lists functions from loaded libraries in memory.
#' @details
#' The function iterates through the attached packages (excluding the base package), identifies the functions within each package's environment, and returns a vector of strings representing these functions in the format "package::function".
#'
#' Implementation notes:
#' \itemize{
#'   \item Uses `search()` to get the list of attached packages.
#'   \item Filters out the "package:base" environment.
#'   \item Uses `ls()` to list objects within each package environment.
#'   \item Employs `tryCatch` to handle potential errors when checking if an object is a function.
#'   \item Returns a unique vector of function names in the format "package::function".
#' }
#'
#' Usage patterns:
#' \itemize{
#'   \item To get a list of all functions loaded from attached packages.
#'   \item To identify which package a particular function is loaded from.
#'   \item To debug namespace issues and conflicts.
#' }
#'
#' @param None
#'
#' @return A character vector containing the names of functions loaded from attached packages, in the format "package::function".
#' @examples
#' \dontrun{
#' # Get a list of functions loaded from attached packages
#' loaded_functions <- functions_libraries_in_memmory()
#' print(loaded_functions)
#'
#' # Check if a specific function is loaded
#' if ("dplyr::mutate" %in% loaded_functions) {
#'   print("dplyr::mutate is loaded")
#' } else {
#'   print("dplyr::mutate is not loaded")
#' }
#' }
functions_libraries_in_memmory <- function(){
  search() |> 
    stringr::str_subset('^package:') |>
    stringr::str_subset('^package:base$', negate = TRUE) |>
    purrr::map(\(env_name){
      env_objects <- ls(env_name)
      functions_in_env <- env_objects[base::sapply(env_objects, function(x) {
        tryCatch(base::is.function(base::get(x, envir = base::as.environment(env_name))), 
                 error = function(e) FALSE)
      })]
      base::paste0(stringr::str_remove(env_name, '^package:'), '::', functions_in_env)
    }) |> base::unlist() |> 
    base::unique()
}





