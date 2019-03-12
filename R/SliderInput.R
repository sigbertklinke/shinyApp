#' SliderInput
#' 
#' Constructs a slider widget to select a numeric value from a range. 
#' 
#' @param app ShinyApp object
#' @inheritParams shiny::sliderInput
#'
#' @return an updated ShinyApp object 
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% SliderInput('slider', 'Slider label', 1, 10, 5)} 
SliderInput <- function (app, inputId, label, min, max, value, step = NULL, round = FALSE,
                         format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                         width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                         timezone = NULL, dragRange = TRUE) {
  # error handling
  if (missing(inputId)) stop('"inputId" missing')
  if (missing(label)) stop('"label" missing')
  if (missing(min)) stop('"min" missing')
  if (missing(max)) stop('"max" missing')
  if (missing(value)) stop('"value" missing')
  #
  ui <- paste0("UI", inputId)
  args  <- as.list(match.call())
  fargs <- formals(shiny::sliderInput)
  if (compareVersion("0.10.2.2", as.character(packageVersion("shiny")))<0) {
    fargs$format <- NULL
    fargs$locale <- NULL
  }
  #
  for (arg in names(fargs)) if(!is.null(args[[arg]])) fargs[[arg]] <- args[[arg]]
  # handling label
  fargs$label <- I(paste0('getText("', eval(fargs$label), '")'))
  uargs <- list(session=I('session'), inputId=fargs$inputId, label=fargs$label, value=I('sel'), 
                min=fargs$min, max=fargs$max, step=fargs$step)
  app$input[[1+length(app$input)]] <- list(Type    = c('sliderInput', 'uiOutput'),
                                           Id      = c(inputId, ui),
                                           Value   = template('if(param=="input${{ID}}") { v<-toNum(val, min={{MIN}}, max={{MAX}}); if(is.na(v)) return({{VALUE}}) else return(v) }',
                                                              ID=inputId, MIN=min, MAX=max, VALUE=value),
                                           Server  = template('output${{UI}}<- renderUI({\n{{FUN}}\n})',
                                                              UI=ui, FUN=str_call('sliderInput', fargs, lib='shiny')),
                                           Observer = template("observe({
                                                                  {{LANGUAGE}}
                                                                  sel  <- value(isolate(input${{ID}}))
                                                                  {{FUN}}
                                                                })", 
                                                               LANGUAGE='{{Language}}', ID=inputId, FUN=str_call('updateSliderInput', uargs, lib='shiny')),
                                           
                                           Sidebar = str_call('uiOutput', list(outputId=ui))
  )
  app
}

#' AssociationInput
#'
#' Constructs a slider widget to select a association value with following defaults: \code{min=0}, \code{max=+1}, \code{value=0}, and 
#'\code{step=0.01}.
#'
#' @param app ShinyApp object
#' @inheritParams shiny::sliderInput
#'
#' @return an updated ShinyApp object
#' @importFrom utils compareVersion packageVersion
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% AssociationInput('r', value=0)}
AssociationInput <- function (app, inputId, label='Association', min=0, max=+1, value=0, step=0.01, 
                              round = FALSE, format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                              timezone = NULL, dragRange = TRUE) {
  args  <- as.list(match.call())
  fargs <- as.list(formals('AssociationInput'))
  for (arg in names(args)) fargs[[arg]] <- args[[arg]]
  fargs$app <- app
  do.call("SliderInput", fargs)
}

#' ConfidenceLevelInput
#'
#' Constructs a slider to select a confidence level with the following defaults:
#' \describe{
#' \item{if \code{percent==TRUE}}{\code{label="Confidence level (\%)"}, \code{min=80}, \code{max=99.99}, \code{value=95}, and \code{step=0.1}}  
#' \item{if \code{percent==FALSE}}{\code{label="Confidence level"}, \code{min=0.8}, \code{max=0.9999}, \code{value=0.95}, and \code{step=0.001}}  
#' }
#' 
#' @param app ShinyApp object
#' @inheritParams shiny::sliderInput
#' @param percent logical: use 95 or 0.95 (default: FALSE)
#' 
#' @return an updated ShinyApp object
#' @import magrittr
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% ConfidenceLevelInput('cl')}
ConfidenceLevelInput <- function (app, inputId, label, min, max, value, step, round = FALSE,
                                  format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                                  width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                                  timezone = NULL, dragRange = TRUE, percent=FALSE) {
  if (percent) {
    if (missing(label)) label <- 'Confidence level (%)'
    if (missing(min))   min   <- 80.0
    if (missing(max))   max   <- 99.9
    if (missing(value)) value <- 95.0
    if (missing(step))  step  <- 0.1
    app <- SliderInput(app, inputId, label, min, max, value, step, round , format, locale, ticks, animate,
                       width, sep, pre, post, timeFormat, timezone, dragRange)
  } else {
    if (missing(label)) label <- 'Confidence level'
    if (missing(min))   min   <- 0.8
    if (missing(max))   max   <- 0.999
    if (missing(value)) value <- 0.95
    if (missing(step))  step  <- 0.001
    app <- SliderInput(app, inputId, label, min, max, value, step, round , format, locale, ticks, animate,
                       width, sep, pre, post, timeFormat, timezone, dragRange)
  }
  app
}

#' CorrelationInput
#'
#' Constructs a slider widget to select a correlation value with the following defaults: \code{min=-1}, \code{max=+1}, 
#' \code{value=0}, and \code{step=0.01}.
#'
#' @param app ShinyApp object
#' @inheritParams shiny::sliderInput
#'
#' @return an updated ShinyApp object
#' 
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% CorrelationInput('r', value=0)}
CorrelationInput <- function (app, inputId, label='Correlation', min=-1, max=+1, value=0, step=0.01, 
                              round = FALSE, format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                              timezone = NULL, dragRange = TRUE) {
  args  <- as.list(match.call())
  fargs <- as.list(formals('CorrelationInput'))
  #browser()
  for (arg in names(args)) fargs[[arg]] <- args[[arg]]
  fargs$app <- app
  do.call("SliderInput", fargs)
}

#' SampleSizeInput
#'
#' Constructs a slider widget to select a sample size with the following defaults
#' \describe{
#' \item{\code{label}}{\emph{Sample size}}
#' \item{\code{min}}{30}
#' \item{\code{max}}{500}
#' \item{\code{value}}{100}
#' \item{\code{step}}{10}
#' }
#' 
#' @param app ShinyApp object
#' @inheritParams shiny::sliderInput
#' @param log logical: use a log-scale (not yet implemented)
#'
#' @return  an updated ShinyApp object
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% SampleSizeInput('n', value=50)}
SampleSizeInput <- function (app, inputId, label='Sample size', min=30, max=500, value=100, step=10, round = TRUE,
                             format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                             width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                             timezone = NULL, dragRange = TRUE, log=FALSE) {
  args  <- as.list(match.call())
  fargs <- as.list(formals('SampleSizeInput'))
  for (arg in names(args)) fargs[[arg]] <- args[[arg]]
  fargs$app <- app
  fargs$log <- NULL
  #browser()
  do.call("SliderInput", fargs)
}
