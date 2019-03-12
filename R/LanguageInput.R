#' DataInput
#'
#' @param app ShinyApp object
#' @inheritParams shiny::selectInput
#' @param ... named list of po translation files
#'
#' @return an updated ShinyApp object
#' @export
#'
#' @examples
LanguageInput <- function(app, inputId, label='Choose language', choices = NULL, selected = NULL,
                      multiple = FALSE, selectize = TRUE, width = NULL, size = NULL, ...) {
  #browser()
  for (i in seq(app$input)) {
    grp <- app$input[[i]]$Group 
    if (!is.null(grp) && (grp == 'LanguageInput')) stop('Only one "LanguageInput" allowed')
  }
  dargs <- match.call(expand.dots = FALSE)$`...`
  nargs <- names(dargs)
  nargs <- nargs[nargs!='']
  data <- c()
  for (arg in nargs) data <- c(data, paste0(arg, '=', deparse(dargs[[arg]])))
  #browser()
  args  <- as.list(match.call())
  fargs <- as.list(formals(SelectInput))
  for (arg in names(fargs)) if(!is.null(args[[arg]])) fargs[[arg]] <- args[[arg]]
  if (missing(label)) fargs$label <- 'Choose language'
  fargs$app     <- app
  fargs$choices <- I('asChoices(getMMstat("lang", "pon"))')
  app <- do.call('SelectInput', fargs)
  #if (length(data)==1) app$input[[length(app$input)]]$Sidebar = NULL
  app$input[[length(app$input)]]$Group <-'LanguageInput'
  app$input[[length(app$input)]]$Language <- template("selectLanguage(value(input${{ID}}))",
                                                      ID=inputId)
  app$input[[length(app$input)]]$SetLang  <- c(app$input[[length(app$input)]]$SetLang , paste0('setLanguage(', paste(data, collapse=","), ')'))
  app                  
}