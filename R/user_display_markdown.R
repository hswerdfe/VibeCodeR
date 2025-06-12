



#' Display Markdown Content in a Shiny Gadget with a Customizable Title
#'
#' This function displays markdown formatted text within a Shiny gadget,
#' allowing for a customizable title and a "Done" button to close the gadget.
#'
#' @description Presents markdown content in an interactive Shiny gadget.
#' @details The function creates a modal window using `shiny::runGadget` to
#'   display the markdown content, rendered using `markdown::markdownToHTML`.
#'   A customizable title is displayed at the top of the gadget. A "Done"
#'   button is provided to close the gadget. This function is useful for
#'   presenting information, warnings, or instructions formatted in markdown
#'   within an interactive R session.
#'
#'   The function constructs a minimal Shiny app UI consisting of a title panel,
#'   the formatted markdown content, and a "Done" button. The server part of
#'   the Shiny app listens for clicks on the "Done" button and stops the
#'   application.
#'
#' @param markdown_string A character string containing the markdown content
#'   to be displayed.  The string should be valid markdown syntax.
#' @param title_string A character string to be used as the title of the gadget.
#'   Defaults to 'Some of the information below was produced by an LLM.'.
#'
#' @return This function does not return a value. It opens a Shiny gadget in
#'   the viewer pane.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' error_message <- "# An Error Occurred\n\nThere was a problem processing your request."
#' display_markdown(error_message, title_string = "Error Message")
#'
#' # Another example with more detailed information:
#' detailed_info <- "
#' ## Detailed Steps
#'
#' 1. Check the input data.
#' 2. Verify the API key.
#' 3. Ensure the server is running.
#' "
#' display_markdown(detailed_info, title_string = "Detailed Instructions")
#' }
display_markdown <- function(
    markdown_string,
    title_string = 'Some of the information below was produced by an LLM.'
    ) {
  ui <- shiny::fluidPage(
    shiny::titlePanel(title_string),
    shiny::HTML(markdown::markdownToHTML(
      text = markdown_string,
      fragment.only = TRUE
    )),
    shiny::br(),
    shiny::actionButton("done_button", "Done", class = "btn-success")
  )
  
  server <- function(input, output, session) {
    shiny::observeEvent(input$done_button, {
      shiny::stopApp()
    })
  }
  
  shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}
