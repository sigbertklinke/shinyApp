#' SelectInput
#'
#' Constructs a text selection input element.
#'
#' @param app ShinyApp object
#' @inheritParams shiny::selectInput
#'
#' @return an updated ShinyApp object
#' @export
#' 
#' @examples
#' ShinyApp() %>% SelectInput('lang', 'Choose', c('German', 'English'))
SelectInput <- function (app, inputId, label, choices, selected = NULL, multiple = FALSE,
                         selectize = TRUE, width = NULL, size = NULL) {
  # error handling
  if (missing(inputId)) stop('"inputId" missing')
  if (missing(label)) stop('"label" missing')
  if (missing(choices)) stop('"choices" missing')
  #
  ui    <- paste0("UI", inputId)
  args  <- as.list(match.call())
  fargs <- formals(shiny::selectInput)
  #
  for (arg in names(fargs)) if(!is.null(args[[arg]])) fargs[[arg]] <- args[[arg]]
  # handling choices
  #browser()
  if (!('AsIs' %in% class(fargs$choices))) {
    if ('call' %in% class(fargs$choices)) fargs$choices <- eval(fargs$choices)
    if ('character' %in% class(fargs$choices)) fargs$choices <- asChoices(fargs$choices)
    if ('name' %in% class(fargs$choices)) fargs['choices'] <- list(NULL)
    if (!is.null(fargs$choices)) {
      clist <- paste0('"', names(fargs$choices), '"=', unlist(fargs$choices), collapse = ',')
      fargs$choices <- I(paste0('getList(', clist, ')'))
    }
  }
  # handling labels
  fargs$label <- I(paste0('getText("', eval(fargs$label), '")'))
  # updateSelectInput
  uargs <- list(session=I("session"), inputId=fargs$inputId, label=fargs$label, choices=fargs$choices, selected=I('sel'))
  ret           <- list (Type     =  c('selectInput','uiOutput'),
                         Id       = c(inputId, ui),
                         #                            Global  = paste0('choices', inputId, ' <- ', str_call('c', fargs$choices)),
                         Server   = template("output${{UI}} <- renderUI({
                                                {{FUN}}
                                              })", 
                                              UI=ui, FUN=str_call('selectInput', fargs, lib='shiny')),
                         Observer = template("observe({
                                                {{LANGUAGE}}
                                                sel<-value(isolate(input${{ID}}))
                                                 {{FUN}}
                                              })", 
                                              LANGUAGE='{{Language}}', ID=inputId, FUN=str_call('updateSelectInput', uargs, lib='shiny')),
                         Sidebar  = str_call('uiOutput', list(outputId=ui))
  )
  selected <- toInt(selected)
  if (is.na(selected)) selected <- 1
  ret$Value <- paste0('if(param=="input$', inputId, '") { v<-toInt(val, min=1); if(is.na(v)) return(', selected, ') else return(v) }')
  app$input[[1+length(app$input)]] <- ret
  app
}
