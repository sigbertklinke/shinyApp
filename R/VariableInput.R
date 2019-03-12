#' DataInput
#'
#' @param app ShinyApp object
#' @inheritParams shiny::selectInput
#' @param ... list of named data sets
#' @param is function: returns for each variable of a data set if it is selectable or not, default: all variables arr selectable
#'
#' @return an updated ShinyApp object
#' @export
#'
#' @examples
VariableInput <- function(app, inputId, label, choices = NULL, selected = NULL,
                          multiple = FALSE, selectize = TRUE, width = NULL, size = NULL, is=NULL) {
  if (is.null(is)) is <- function(x) { TRUE }
  data <- 0
  for (i in seq(app$input)) {
    grp <- app$input[[i]]$Group 
    if (!is.null(grp) && (grp == 'DataInput')) data <- i
  }
  if (data==0) stop('No "DataInput" found')
  if (class(is)=='function') is <- substitute(is)
  args  <- as.list(match.call())
  fargs <- as.list(formals(SelectInput))
  for (arg in names(fargs)) if(!is.null(args[[arg]])) fargs[[arg]] <- args[[arg]]
  fargs$app        <- app
  #browser()
  if (missing(label)) fargs$label <- 'Choose variable'
  fargs['choices'] <- list(NULL)
  app <- do.call('SelectInput', fargs)       
  lai <- length(app$input)
  uargs <- list(session=I('session'), inputId=fargs$inputId, choices=I('choices'), 
                selected=I('sel'))
  uargs$label <- I(paste0('getText("', eval(fargs$label), '")'))
  app$input[[lai]]$Global <- paste0('is.', inputId, '<-', paste0(as.character(is), collapse="\n"))
  #browser()
  app$input[[lai]]$Observer <- template("observe({
                                           {{LANGUAGE}}
                                           sel     <- value(isolate(input${{ID}}))
                                           sattr   <- getSelectionAttr(value(input${{DATA}}))
                                           choices <- asChoices(rownames(sattr), sattr[,'{{VAR}}']>0)
                                           {{FUN}}
                                          })", 
                                          ID=inputId, DATA=app$input[[data]]$Id[1], 
                                          LANGUAGE='{{Language}}', VAR=inputId,
                                          FUN=str_call('updateSelectInput', uargs))
  app$input[[lai]]$Variable <- c(app$input[[lai]]$Variable, paste0('setSelection("', inputId, '", is.', inputId, ")\n"))
  app                  
}