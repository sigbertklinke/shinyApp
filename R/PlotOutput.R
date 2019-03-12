#' plotOut
#'
#' Render a plot within an application page. 
#' Either \code{file} or \code{text} must be set.
#' 
#' @param app a ShinyApp object
#' @inheritParams shiny::plotOutput
#' @param file character: name of file with R code to execute
#' @param text character: text with R code to execute
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
                       file=NULL, text=NULL) {
  if (!inherits(app, 'ShinyApp')) stop("No shiny app")
  # error handling
  if (missing(outputId)) stop('"outputId" missing')
  if (is.null(text)) {
    if (is.null(file)) stop('Either "text" or "file" must be set')
    pfile <- normalizePath(file, mustWork = TRUE)
    text  <- paste0(readLines(pfile), collapse="\n")
  } else {
    pfile <- ''
  }
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
