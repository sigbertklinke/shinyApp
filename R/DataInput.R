#' DataInput
#'
#' @param app ShinyApp object
#' @inheritParams shiny::selectInput
#' @param ... list of named data sets
#'
#' @return an updated ShinyApp object
#' @export
#'
#' @examples
#' \dontrun{
#' library("MASS")
#' ShinyApp() %>%
#'   DataInput('data', BOSTON=Boston) %>%
#'   WriteApp()
#' }
DataInput <- function(app, inputId, label='Choose data set', choices = NULL, selected = NULL,
                      multiple = FALSE, selectize = TRUE, width = NULL, size = NULL, ...) {
  #browser()
  for (i in seq(app$input)) {
    grp <- app$input[[i]]$Group 
    if (!is.null(grp) && (grp == 'DataInput')) stop('Only one "DataInput" allowed')
  }
  #browser()
  dargs <- match.call(expand.dots = FALSE)$`...`
  nargs <- names(dargs)
  nargs <- nargs[nargs!='']
  data <- c()
  for (arg in nargs) data <- c(data, paste0(arg, '=', deparse(dargs[[arg]])))
  #browser()
  args  <- as.list(match.call())
  fargs <- as.list(formals(SelectInput))
  for (arg in names(fargs)) if(!is.null(args[[arg]])) fargs[[arg]] <- args[[arg]]
  if (missing(label)) fargs$label <- 'Choose data set'
  fargs$app     <- app
  fargs$choices <- asChoices(nargs)
  app <- do.call('SelectInput', fargs)
  if (length(data)==1) app$input[[length(app$input)]]$Sidebar = NULL
  app$input[[length(app$input)]]$Group <-'DataInput'
  app$input[[length(app$input)]]$Dataset <- c(app$input[[length(app$input)]]$Dataset, paste0('setData(', paste(data, collapse=","), ')'))
  app                  
}