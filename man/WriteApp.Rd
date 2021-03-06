% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WriteApp.R
\name{WriteApp}
\alias{WriteApp}
\title{WriteApp}
\usage{
WriteApp(app, file = "app.R", edit = "file.edit", copyright = NULL,
  ...)
}
\arguments{
\item{app}{ShinyApp object}

\item{file}{character: name of file to write to (default: \code{app.R} in the current working directory)}

\item{edit}{function: a function to edit the file or \code{NULL} for no edit (default: \code{\link[utils]{file.edit}})}

\item{copyright}{character: some copyright information (default: \code{NULL}). The default copyright notice is 
\preformatted{
 shiny::tags$div(align="center",
   shiny::tags$hr(),
   shiny::tags$a(href = "https://github.com/sigbertklinke/shinyApp", "Created with shinyApp"),
   shiny::tags$br(),
   shiny::tags$a(target="_blank", href="https://www.wihoforschung.de/de/flipps-1327.php",  "Supported by BMBF")
 )
}}

\item{...}{unused}
}
\value{
invisibly the ShinyApp object
}
\description{
Writes a ShinyApp object to a file.
}
\examples{
\dontrun{ShinyApp() \%>\% PlotOutput('plot', text='plot(runif(10), runif(10))') \%>\% WriteApp()}
}
