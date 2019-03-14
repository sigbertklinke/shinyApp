#' plotOut
#'
#' Render a plot within an application page. 
#' Either \code{file} or \code{text} must be set.
#' 
#' @param app a ShinyApp object
#' @inheritParams shiny::plotOutput
#' @param file character: name of file with R code to execute
#' @param text character: text with R code to execute
#' @param expr expression: R code to execute
#'
#' @return an updated ShinyApp object
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% PlotOutput('plot', text='plot(runif(10), runif(10))' }
PlotOutput <- function(app, outputId, width = "100%", height = "400px", click = NULL,
                       dblclick = NULL, hover = NULL, hoverDelay = NULL,
                       hoverDelayType = NULL, brush = NULL, clickId = NULL, hoverId = NULL,
                       inline = FALSE,
                       file, text, expr) {
  if (!inherits(app, 'ShinyApp')) stop("No shiny app")
  # error handling
  if (missing(outputId)) stop('"outputId" missing')
  if (missing(file)+missing(text)+missing(expr)!=2) stop ("One of 'file', 'text' or 'expr' must be given")
  pfile <- ''
  if (!missing(file)) {
    pfile <- normalizePath(file, mustWork = TRUE)
    text  <- readLines(pfile)
  }
  if (!missing(expr)) text <- deparse(substitute(expr))
  text <- paste0(text, collapse="\n")
  #
  args  <- as.list(match.call())
#  add_ID(outputId, 'plotOutput')
  fargs <- formals(shiny::plotOutput)
  eargs <- list()
  #browser()
  for (arg in names(fargs)) eargs[[arg]] <- if(is.null(args[[arg]])) eval(fargs[[arg]]) else eval(args[[arg]])
  app$output[[1+length(app$output)]] <- list(Type   = 'plotOutput', 
                                             Id     = outputId,
                                             Body   = str_call('plotOutput', eargs, lib='shiny'),
                                             Server = c(paste0('output$', outputId, " <- shiny::renderPlot({\n{{Language}}\n#", pfile, "\n", text, "\n})", ''))
  )
  app
}
