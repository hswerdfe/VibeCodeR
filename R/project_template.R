#' Create a Vibe Project
#'
#' This function is called when creating a new Vibe project from the RStudio template.
#' For now, it just creates a standard R project structure.
#'
#' @param path Path where the project should be created
#' @param project_name Name of the project
#' @param project_goal Description of the project goal
#' @param include_readme Whether to include README
#' @param vibe_level The vibe level selected
#' @param music_genre Preferred music genre
#' @param ... Additional parameters from the template form
#'
#' @export
create_vibe_project <- function(
    path,
    project_name = "MyVibeProject",
    project_goal = "Describe what you want to achieve with this project...",
    include_readme = TRUE,
    vibe_level = "Moderate",
    music_genre = "Lo-fi",
    ...
) {

  # Ensure the package namespace is available
  if (!requireNamespace("VibeCodeR", quietly = TRUE)) {
    stop("VibeCodeR package not found")
  }
  # Get the parameters passed from the template form
  params <- list(
    project_name = project_name,
    project_goal = project_goal,
    include_readme = include_readme,
    vibe_level = vibe_level,
    music_genre = music_genre,
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
  welcome_content <- paste0(
    "# Welcome to your Vibe Project!\n",
    "# Created on: ", Sys.Date(), "\n",
    "#\n",
    "# Project: ", project_name, "\n",
    "# Goal: ", project_goal, "\n",
    "# Vibe Level: ", vibe_level, "\n",
    "# Music Genre: ", music_genre, "\n",
    "#\n"
  )

  # Add any custom parameters from the form
  if (length(params) > 5) {  # More than the standard 5 parameters
    welcome_content <- paste0(welcome_content, "# Additional parameters:\n")
    extra_params <- params[!names(params) %in% c("project_name", "project_goal", "include_readme", "vibe_level", "music_genre")]
    for (name in names(extra_params)) {
      welcome_content <- paste0(welcome_content, "# ", name, ": ", extra_params[[name]], "\n")
    }
    welcome_content <- paste0(welcome_content, "#\n")
  }

  welcome_content <- paste0(welcome_content, "\n\n# Start coding your vibe! 🎵\nprint('Welcome to VibeCodeR!')\n")

  writeLines(welcome_content, file.path(path, "welcome.R"))

  # Create a README if requested
  if (include_readme) {
    readme_content <- paste0(
      "# ", project_name, "\n\n",
      "## Project Goal\n\n",
      project_goal, "\n\n",
      "## Project Details\n\n",
      "- **Vibe Level**: ", vibe_level, "\n",
      "- **Music Genre**: ", music_genre, "\n",
      "- **Created**: ", Sys.Date(), "\n\n",
      "This is a Vibe project created with VibeCodeR!\n\n",
      "## Getting Started\n\n",
      "1. Open `welcome.R` to start coding\n",
      "2. Feel the vibe!\n"
    )

    writeLines(readme_content, file.path(path, "README.md"))
  }

  invisible(TRUE)
}

#' Package startup function
#' @param libname Library name
#' @param pkgname Package name
.onLoad <- function(libname, pkgname) {
  # Register the project template when the package loads
  invisible()
}
