
# library(shiny)
# library(miniUI)
# library(rstudioapi)
# library(diffobj)
# library(htmltools)


#' Display a Diff Gadget for Comparing Text Changes
#'
#' This function launches a Shiny gadget that displays the differences between two
#' text strings using the `diffobj` package. It provides a user interface with
#' "Keep Original" and "Accept Refactored" buttons, allowing the user to choose
#' which version to retain.
#'
#' @description Launches a Shiny gadget to visualize and choose between two text versions.
#' @details The function takes two text strings, `original` and `refactored`, splits them into lines,
#' and uses `diffobj::diffChr` to compute the differences.  The differences are then
#' displayed in a Shiny gadget with custom CSS styling to highlight the changes.
#' The gadget includes "Keep Original" and "Accept Refactored" buttons. Clicking either button
#' closes the gadget and returns a string indicating the user's choice.
#'
#' The custom CSS styles override the default `diffobj` colors to use light green for
#' refactored (inserted) content and yellow for original (deleted) content.
#'
#' @param original A character string representing the original text.  Newlines (`\\n`)
#'   are used to separate lines.
#' @param refactored A character string representing the refactored text. Newlines (`\\n`)
#'   are used to separate lines.
#' @param dialog_title A character string specifying the title of the gadget's dialog window.
#'   Defaults to "🤖 Do you accept the changes? 🤔!".
#' @param instructions_given An optional character string containing instructions to display
#'   below the diff object. If NULL (default), no instructions are shown.
#'
#' @return A character string. Returns "keep" if the "Keep Original" button is clicked,
#'   and "accept" if the "Accept Refactored" button is clicked.
#'
#' @examples
#' \dontrun{
#' original_text <- "This is the original text.\\nIt has some lines.\\nAnd some more."
#' refactored_text <- "This is the refactored text.\\nIt has some modified lines.\\nAnd even more!"
#'
#' choice <- show_diff_gadget(original_text, refactored_text, 
#'                           dialog_title = "Review Changes",
#'                           instructions_given = "Please review the changes carefully before deciding.")
#' print(paste("User chose:", choice))
#'
#' # Example with shorter strings
#' original <- "Line 1\\nLine 2\\nLine 3"
#' refactored <- "Line 1\\nLine 2 modified\\nLine 3"
#'
#' choice2 <- show_diff_gadget(original, refactored)
#' print(paste("User chose:", choice2))
#' }
show_diff_gadget <- function(original, refactored,
                             dialog_title = "🤖 Do you accept the changes? 🤔!",
                             instructions_given = NULL) {
  
  Original <- base::strsplit(original, "\n")[[1]] 
  Refactored <- base::strsplit(refactored, "\n")[[1]]
  
  diff_result <- diffobj::diffChr(
    Original, 
    Refactored
  )
  
  ui <- 
    miniUI::miniPage(
    miniUI::miniTitleBar(dialog_title),
    # Add custom CSS styling
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML("
        /* Override diffobj default colors - targeting the correct classes */
        DIV.diffobj-container.light.yb SPAN.diffobj-word.insert,
        DIV.diffobj-container.light.yb DIV.diffobj-line>DIV.insert {
          background-color: #c8e6c9 !important; /* Light green for refactored word/line highlights */
        }
        DIV.diffobj-container.light.yb DIV.diffobj-text>DIV.insert {
          background-color: #f0f8f0 !important; /* Very light green background for refactored sections */
        }
        
        /* Keep the yellow for deleted/original content */
        DIV.diffobj-container.light.yb SPAN.diffobj-word.delete,
        DIV.diffobj-container.light.yb DIV.diffobj-line>DIV.delete {
          background-color: #e7e780 !important; /* Keep existing yellow for original */
        }
        DIV.diffobj-container.light.yb DIV.diffobj-text>DIV.delete {
          background-color: #fefee5 !important; /* Keep existing light yellow background */
        }
        
        /* Instructions styling */
        .instructions-container {
          background-color: #f5f5f5;
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 12px;
          margin: 15px 0;
          font-style: italic;
          color: #555;
        }
        
        /* Button container styling - positioned under diff sections */
        .button-container {
          display: flex;
          flex-direction: row;
          justify-content: space-between;
          align-items: center;
          margin-top: 20px;
          padding: 0 10px;
        }
        
        /* Individual button styling */
        .btn {
          min-width: 150px;
          padding: 8px 16px;
          border-radius: 4px;
          border: none;
          font-weight: bold;
          cursor: pointer;
        }
        
        /* Keep Original button - positioned under left (original) section */
        .btn-keep {
          background-color: #e7e780 !important;
          color: #333 !important;
          border: 2px solid #d4d470 !important;
          margin-right: auto;
        }
        .btn-keep:hover {
          background-color: #d4d470 !important;
          border-color: #c1c160 !important;
        }
        
        /* Accept Changes button - positioned under right (refactored) section */
        .btn-accept {
          background-color: #c8e6c9 !important;
          color: #333 !important;
          border: 2px solid #b0d6b2 !important;
          margin-left: auto;
        }
        .btn-accept:hover {
          background-color: #b0d6b2 !important;
          border-color: #98c69b !important;
        }
        
        /* Create a 50-50 split layout for buttons */
        .button-left {
          flex: 1;
          display: flex;
          justify-content: center;
          padding-right: 10px;
        }
        
        .button-right {
          flex: 1;
          display: flex;
          justify-content: center;
          padding-left: 10px;
        }
      "))
    ),
    miniUI::miniContentPanel(
      shiny::htmlOutput("diff"),
      # Conditionally display instructions if provided
      shiny::conditionalPanel(
        condition = "output.show_instructions",
        shiny::div(
          class = "instructions-container",
          shiny::htmlOutput("instructions")
        )
      ),
      shiny::br(),
      shiny::div(
        class = "button-container",
        shiny::div(
          class = "button-left",
          shiny::actionButton("keep", "❌ Keep Original", class = "btn btn-keep")
        ),
        shiny::div(
          class = "button-right",
          shiny::actionButton("accept", "✅ Accept Refactored", class = "btn btn-accept")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    output$diff <- shiny::renderUI({
      shiny::HTML(as.character(diff_result))
    })
    
    # Render instructions if provided
    output$instructions <- shiny::renderUI({
      if (!is.null(instructions_given) && instructions_given != "") {
        shiny::HTML(paste0("<strong>Instructions:</strong> ", instructions_given))
      }
    })
    
    # Control visibility of instructions panel
    output$show_instructions <- shiny::reactive({
      !is.null(instructions_given) && instructions_given != ""
    })
    shiny::outputOptions(output, "show_instructions", suspendWhenHidden = FALSE)
    
    shiny::observeEvent(input$keep, stopApp("keep"))
    shiny::observeEvent(input$accept, stopApp("accept"))
  }
  
  shiny::runGadget(ui, server)
}

# Example usage
# original <- "x <- 1\ny <- 2"
# refactored <- "x <- 1#x is one\ny <- 2"
# refactored <- "# Improved\nx <- 1\ny <- 2\nz <- x + y"
# result <- show_diff_gadget(original, refactored, instructions_given = "Please review the code changes. The refactored version adds a comment and calculates a new variable z.")
