#' ShinyApp
#'
#' @import shiny shinyExample
#' @export
ShinyApp <- function(app, ...) { UseMethod("ShinyApp") }

#' ShinyApp
#'
#' Creates a shiny app using \href{https://rstudio.github.io/shinydashboard/}{shinydashboard}.
#'
#' @param app character: name of app template file (default: \code{inst/app/app.tmpl} in the \code{shinyExample} library)
#' @param ... unused 
#'
#' @return an empty ShinyApp object
#' @rdname ShinyApp
#' @export
#'
#' @examples
#' ShinyApp()
ShinyApp.default <- function(app, ...) {
  if (missing(app)) app <- paste0(find.package('shinyApp'), '/app/app.tmpl')
  prg <- list(Global=c(),
              output=list(),
              input=list(),
              DashboardHeader=list(),
              DashboardSidebar=list(),
              template=app) 
  class(prg) <- 'ShinyApp'
  prg %>% DashboardHeader('MM*Stat') %>% DashboardSidebar()
}
  