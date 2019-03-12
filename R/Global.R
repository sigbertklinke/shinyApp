#' Global
#'
#' Allows to enter global code into the app. Please note that in shiny server not all commands allowed. 
#' For example using \code{install.packages} will 
#'
#' @param app ShinyApp object
#' @param expr expression: code to add to the app
#' @param file chracter: file name of a file which contains the code
#'
#' @return an updated ShinyApp object
#' @export
#'
#' @examples
#' \dontrun{
#'   ShinyApp() %>% 
#'     Global({
#'       library("rio")
#'       x <- import("mydata")
#'     })
#' }
Global <- function(app, expr, file=NULL) {
  if (!inherits(app, 'ShinyApp')) stop("No shiny app")
  if (missing(expr)) {
    if (is.null(file)) stop("Either 'expr' of 'file' must be used")
    app$Global <- c(app$Global, readLines(file))
  } else {
    app$Global <- c(app$Global, deparse(substitute(expr)))
  }
  app    
}