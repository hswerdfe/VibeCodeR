% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rstudio_api_fall_back_wrapper.R
\name{active_document_context}
\alias{active_document_context}
\title{Get Active RStudio Document Context}
\usage{
active_document_context()
}
\value{
A list representing the active document context. The structure includes:
  - \code{path}: The path to the active file, or \code{NULL} if unavailable.
  - \code{contents}: A character vector of the file contents.
  - \code{selection}: A list containing selection details from the document.
}
\description{
Retrieves information about the currently active document in RStudio, including its file path, full contents, and selection.
If the RStudio API is not available (e.g., when running outside of RStudio), a default placeholder context is returned.
}
\examples{
\dontrun{
  ctx <- active_document_context()
  if (!is.null(ctx$path)) {
    print(ctx$path)
  }
}
}
\keyword{context}
\keyword{document}
\keyword{rstudio}
