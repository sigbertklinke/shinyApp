#' TextOutput
#'
#' Render a reactive output variable as text within an application page. 
#' Either \code{file} or \code{text} must be set.
#'
#' @param app a ShinyApp object
#' @inheritParams shiny::textOutput
#' @param file character: name of file with R code to execute
#' @param text character: text with R code to execute
#' @param expr expression: R code to execute
#'
#' @return an updated ShinyApp object
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% TextOutput('plot', text='"Hello, World!"' }
TextOutput<- function(app, outputId, container = if (inline) span else div, inline = FALSE, file, text, expr) {
  div <- function(...) { shiny::div(...) }
  span <- function(...) { shiny::span(...) }
  # browser()
  # error handling
  if (missing(outputId)) stop('"outputId" missing')
  pfile <- ''
  if (!missing(file)) {
    pfile <- normalizePath(file, mustWork = TRUE)
    text  <- readLines(pfile)
  }
  if (!missing(expr)) text <- deparse(substitute(expr))
  text <- paste0(text, collapse="\n")
  #
  args  <- as.list(match.call())
  fargs <- formals(shiny::textOutput)
  #
  for (arg in names(fargs)) if(!is.null(args[[arg]])) fargs[[arg]] <- args[[arg]]
  # handling container
  fargs$container <- eval(fargs$container)
  app$output[[1+length(app$output)]] <- list(Type   = 'textOutput', 
                                             Id     = outputId,
                                             Body   = str_call('textOutput', fargs, lib='shiny'),
                                             Server = paste0('output$', outputId, " <- shiny::renderText({\n{{Language}}#", pfile, "\n", 
                                                             paste0(text, collapse="\n"), "\n})", '')
  )
  app
}
