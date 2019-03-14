#' CheckboxInput
#'
#' Creates a checkbox element.
#' 
#' @param app ShinyApp object
#' @inheritParams shiny::checkboxInput
#'
#' @return an updated app
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% CheckboxInput('rug', 'Show observations') }
CheckboxInput <- function (app, inputId, label, value = FALSE, width = NULL) {
  if (!inherits(app, 'ShinyApp')) stop("No shiny app")
  # error handling
  if (missing(inputId)) stop('"inputId" missing')
  if (missing(label)) stop('"label" missing')
  #
  #browser()
  ui <- paste0("UI", inputId)
  args  <- as.list(match.call())
  fargs <- formals(shiny::checkboxInput)
  for (arg in names(fargs)) if(!is.null(args[[arg]])) fargs[[arg]] <- args[[arg]]
  fargs$label <- I(paste0('getText("', eval(fargs$label), '")'))
  #
  uargs <- list(session=I('session'), inputId=fargs$inputId, label=fargs$label, value=I('sel'))
  app$input[[1+length(app$input)]] <- list(Type    = c('checkboxInput', 'uiOutput'),
                                           Id      = c(inputId, ui),
                                           Value   = template('if(param=="input${{ID}}") { v<-toLog(val); if(is.na(v)) return({{VAL}}) else return(val) }',
                                                             ID=inputId, VAL=value),
                                           Server  = template("output${{UI}} <- renderUI({\n{{FUN}}\n})", 
                                                             UI=ui, FUN=str_call('checkboxInput', fargs, lib='shiny')),
                                           Observer = template("observe({
                                                               {{LANGUAGE}}
                                                               sel<-value(isolate(input${{ID}}))
                                                               {{FUN}}
                                                              })", 
                                                              LANGUAGE='{{Language}}', ID=inputId, FUN=str_call('updateCheckboxInput', uargs, lib='shiny')),
                                           Sidebar = str_call('uiOutput', list(outputId=ui))
  )
  app
}