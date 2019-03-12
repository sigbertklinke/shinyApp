#' ActionButton
#'
#' Creates an action button element.
#'
#' @param app ShinyApp object
#' @inheritParams shiny::actionButton
#'
#' @return an updated app
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% ActionButton('button', 'Button label') }
ActionButton <- function (app, inputId, label, icon = NULL, width = NULL, ...) {
  # error handling
  if (missing(inputId)) stop('"inputId" missing')
  if (missing(label)) stop('"label" missing')
  #
  ui <- paste0("UI", inputId)
  args  <- as.list(match.call())
  fargs <- formals(shiny::actionButton)
  eargs <- list()
  for (arg in names(fargs)) {
    if (arg!='...') {
      eargs[[arg]] <- if(is.null(args[[arg]])) fargs[[arg]] else args[[arg]]
      if (class(eargs[[arg]])=='if') eargs[[arg]] = eval(eargs[[arg]])
    }
  }
  if ('...' %in% names(fargs)) {
    ellipsis <- list(...)
    for (arg in names(ellipsis)) eargs[[arg]] <- ellipsis[[arg]]
  }
  app$input[[1+length(app$input)]] <- list(Type = c('actionButton', 'uiOutput'),
                                           Id=c(inputId, ui),
                                           Server=paste0('output$', ui, "<- renderUI({\n", str_call(args[[1]], eargs, lib='shiny'), "\n})"),
                                           Sidebar=str_call('uiOutput', list(outputId=ui))
  )
  app
}
