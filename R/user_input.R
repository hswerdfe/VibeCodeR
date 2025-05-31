
library(shiny)
library(miniUI)
library(shinyWidgets)
library(janitor)
library(here)



#' Launch a Shiny Gadget to Prompt User Input via Dialog
#'
#' This function displays a miniUI dialog using Shiny's gadget framework,
#' dynamically generating input fields based on a list of questions. Each
#' question can be a simple string (which becomes a text input), or a list
#' with a `question` string and optional `choices` vector (which becomes
#' a dropdown if choices are present).
#'
#' @param questions A non-empty list of questions. Each element should be:
#'   - a character string (label for a text input), or
#'   - a list with at least a `question` field (character), and optionally
#'     a `choices` field (vector of choices for a dropdown input).
#' @param dialogName Title of the dialog window (default: "Questions.").
#' @param width Width of the dialog window in pixels (default: 10).
#' @param height Height of the dialog window in pixels (default: 10).
#'
#' @return A named list of user responses, where names correspond to
#'   the questions or their labels. Returns `NULL` if the user cancels.
#'
#' @examples
#' questions <- list(
#'   "Enter your name:",
#'   list(question = "Choose a color:", choices = c("Red", "Green", "Blue")),
#'   list(question = "Comments?")
#' )
#' responses <- user_input(questions)
#'
#'
#' questions <- list(
#'   "What's your name?",
#'   list(question = "Do you want to include fries with that?", type = "logical"),
#'   list(question = "Choose a drink:", choices = c("Water", "Soda", "Juice")),
#'   list(question = "Rate our service (1-10):", type = "numericRange", range = c(1, 10)),
#'   list(question = "Any other comments?")
#' )
#'
#' responses <- user_input(questions)
#'
#'
#' @import shiny
#' @import miniUI
#' @export
user_input <- function(questions, dialogName = "Questions", width = 800, height = 500) {
  if (!is.list(questions) || length(questions) == 0) {
    stop("questions must be a non-empty list")
  }

  ui_elements <- list()

  for (i in seq_along(questions)) {
    q <- questions[[i]]
    input_id <- paste0("q", i)

    if (is.character(q)) {
      ui_elements[[i]] <- textInput(input_id, label = q, width = "100%")
    } else if (is.list(q)) {
      if (is.null(q$question)) {
        stop(paste("Question", i, "must have a 'question' field"))
      }

      default <- q$default

      if (!is.null(q$type) && q$type == "logical") {
        ui_elements[[i]] <- checkboxInput(input_id, label = q$question, value = if (!is.null(default)) default else FALSE)
      } else if (!is.null(q$type) && q$type == "numericRange") {
        if (is.null(q$range) || length(q$range) != 2 || !is.numeric(q$range)) {
          stop(paste("Question", i, "with type 'numericRange' must have a numeric 'range' of length 2"))
        }

        slider_value <- if (!is.null(default)) default else mean(q$range)

        ui_elements[[i]] <- sliderInput(input_id,
                                        label = q$question,
                                        min = q$range[1],
                                        max = q$range[2],
                                        value = slider_value,
                                        width = "100%")
      } else if (!is.null(q$type) && q$type == "textarea") {
        ui_elements[[i]] <- textAreaInput(input_id,
                                          label = q$question,
                                          value = if (!is.null(default)) default else "",
                                          rows = ifelse(!is.null(q$rows), q$rows, 5),
                                          resize = "vertical",
                                          width = "100%")
      } else if (!is.null(q$choices)) {
        ui_elements[[i]] <- selectInput(input_id,
                                        label = q$question,
                                        choices = q$choices,
                                        selected = if (!is.null(default)) default else NULL,
                                        width = "100%")
      } else {
        ui_elements[[i]] <- textInput(input_id,
                                      label = q$question,
                                      value = if (!is.null(default)) default else "",
                                      width = "100%")
      }
    } else {
      stop(paste("Question", i, "must be a character string or list"))
    }
  }

  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        body, label, input, select, button, textarea { font-size: 12px; }
        .shiny-input-container { margin-bottom: 10px; }
      "))
    ),
    do.call(tagList, ui_elements),
    actionButton("done", "Done", class = "btn-primary")
  )

  server <- function(input, output, session) {
    observeEvent(input$done, {
      answers <- list()
      for (i in seq_along(questions)) {
        input_id <- paste0("q", i)
        answers[[i]] <- input[[input_id]]
        q <- questions[[i]]
        #names(answers)[i] <- if (is.character(q)) q else q$question
        names(answers)[i] <- if (is.character(q)) janitor::make_clean_names(q) else janitor::make_clean_names(q$question)
      }
      stopApp(answers)
    })
  }

  runGadget(ui, server, viewer = dialogViewer(dialogName = dialogName, width = width, height = height))
}







