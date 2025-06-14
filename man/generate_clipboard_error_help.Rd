% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_error_help.R
\name{generate_clipboard_error_help}
\alias{generate_clipboard_error_help}
\title{Generate Error Explanation from Clipboard Content Using LLMs}
\usage{
generate_clipboard_error_help()
}
\value{
None. The function displays the LLM's response in the RStudio Viewer.
}
\description{
Analyzes code copied to the clipboard and provides debugging help using an LLM.
}
\details{
This function reads content from the clipboard, assumes it's a code-related
error, and utilizes a language model to provide assistance and a potential
solution.

The function reads the clipboard content using `clipr::read_clip()`.
  It then constructs a prompt including the clipboard content and other
  contextual information (error messages, history, platform details,
  traceback, R version, environment context).  This prompt is sent to an
  LLM (via `llm_request()`) to generate a helpful explanation and solution.
  The LLM response and the original clipboard content are then displayed using
  `display_markdown()`. The prompt instructs the LLM to assist with the
  provided code, using markdown for clarity.
}
\examples{
\dontrun{
# Copy some problematic code to the clipboard (e.g., from an error message)
# Then run:
generate_clipboard_error_help()
}
}
