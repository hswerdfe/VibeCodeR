#' Create a Vibe Project
#'
#' This function is called when creating a new Vibe project from the RStudio template.
#' For now, it just creates a standard R project structure.
#'
#' @param path Path where the project should be created
#' @param project_name Name of the project
#' @param include_readme Whether to include README
#' @param ... Additional parameters from the template form
#'
#' @export
create_vibe_project <- function(
    path,
    project_name = "MyVibeProject",
    include_readme = TRUE,
    ...
) {
  #path= here::here() 
  print(path)
  # Ensure the package namespace is available
  if (!requireNamespace("VibeCodeR", quietly = TRUE)) {
    stop("VibeCodeR package not found")
  }
  # Get the parameters passed from the template form
  params <- list(
    project_name = project_name,
    include_readme = include_readme,
    ...
  )

  # Create the project directory if it doesn't exist
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Create a basic R project file
  c(
    "Version: 1.0",
    "",
    "RestoreWorkspace: Default",
    "SaveWorkspace: Default",
    "AlwaysSaveHistory: Default",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8",
    "",
    "RnwWeave: Sweave",
    "LaTeX: pdfLaTeX"
  ) |>
    writeLines(file.path(path, paste0(basename(path), ".Rproj")))

  # Create a welcome R script
  welcome_content <- 
    paste0("# ",
      c(
        "Welcome to your Vibe Project!",
        "-----------------------------",
        paste("Created on: ", Sys.Date()),
        "",
        paste0("Project: `", project_name, "`"),
        "",
        "Access VibeCodeR functionality from either the `Addins ` drop down from the RStudio toolbar,",
        "Or from the pallet `Ctrl + Shift + p` the type `VibeCodeR`.",
        "to see options available in current version."
      ), collapse = '\n' 
    ) 




  writeLines(welcome_content, file.path(path, "welcome.R"))

  # Create a README if requested
  if (include_readme) {
    readme_content <- paste0(
      "# ", project_name, "\n\n",
      "## Project Goal\n\n",
      "## Project Details\n\n",
      "- **Created**: ", Sys.Date(), "\n\n",
      "This is a Vibe project created with VibeCodeR!\n\n",
      "## Getting Started\n\n",
      "1. Open `welcome.R` to start coding\n",
      "2. Feel the vibe!\n"
    )

    writeLines(readme_content, file.path(path, "README.md"))
  }


  default_results <- user_input_dot_project_files(path = path)

  path_coder <- path |> file.path('.VibeCodeR')
  if (!dir.exists(path_coder)) {
    dir.create(path_coder, recursive = TRUE)
  }
  default_results |>
    purrr::iwalk(~{
      #print(.y)
      if (nchar(.x) > 0 ){
        .x <- as.character(.x)
        writeLines(
            text = .x, 
            con = file.path(path_coder, paste0('.', .y, '.config'))
        )
      }
    })

  invisible(TRUE)
}

#' Package startup function
#' @param libname Library name
#' @param pkgname Package name
.onLoad <- function(libname, pkgname) {
  # Register the project template when the package loads
  invisible()
}
