% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/context_prompts.R
\name{context_error_message}
\alias{context_error_message}
\title{Generate a Contextual Error Message}
\usage{
context_error_message(
  prefix = "\\n## The most recent error message  is :\\n",
  suffix = "\\n## End error message."
)
}
\arguments{
\item{prefix}{Character string to prepend to the error message.
Defaults to "\\n## The most recent error message  is :\\n".}

\item{suffix}{Character string to append to the error message.
Defaults to '\\n## End error message.'.}
}
\value{
A character string containing the formatted error message.
}
\description{
Formats the last error message for inclusion in prompts.
}
\details{
This function retrieves the last error message and formats it with a prefix and suffix.

This function uses `geterrmessage()` to obtain the last error
  message and then concatenates it with a specified prefix and suffix.
  The resulting string is then processed by `txt_single()` to ensure it is
  a single line. This is useful for providing context about recent errors
  in code generation or debugging scenarios.
}
\examples{
\dontrun{
# Force an error
try(log("a"))

# Get the formatted error message
error_message <- context_error_message()

# Print the error message
cat(error_message)
}
}
