% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{is_blank_comment}
\alias{is_blank_comment}
\title{Check if a String Contains Only Blank Roxygen Comments}
\usage{
is_blank_comment(x)
}
\arguments{
\item{x}{A character string to be checked.}
}
\value{
A logical value: `TRUE` if the string contains only blank or roxygen-commented lines,
  `FALSE` otherwise.
}
\description{
Checks if a string is effectively a blank roxygen comment.
}
\details{
This function determines whether a given string contains only blank lines
or lines consisting solely of roxygen comment markers (`#'`) and whitespace.

The function splits the input string by newline characters, then removes
  leading and trailing whitespace and roxygen comment markers (`#'`) from each line.
  It then counts the number of non-empty lines remaining. If no non-empty lines
  are found, the function returns `TRUE`, indicating that the input string
  consists only of blank or commented-out lines.
}
\examples{
# Example with a blank roxygen comment
\dontrun{
is_blank_comment("#'  \n#'   \n")

# Example with a non-blank roxygen comment
is_blank_comment("#' This is a comment\n#' Another line")

# Example with no roxygen comment markers
is_blank_comment("This is a comment\nAnother line")

# Example with only whitespace
is_blank_comment("   \n  \n")
}
}
