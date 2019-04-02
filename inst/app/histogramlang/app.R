library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyExample)

{
  data(Boston, package = "MASS")
}
is.var1<-is.numeric

setLanguage(GERMAN="GERMAN.po",ENGLISH="ENGLISH.po")

setData(BOSTON=Boston,MTCARS=mtcars)

setSelection("var1", is.var1)


ui <- dashboardPage(
  dashboardHeader("title"="MM*Stat",
                  "titleWidth"=NULL,
                  "disable"=FALSE),
  dashboardSidebar("disable"=FALSE,
                   "width"=NULL,
                   "collapsed"=FALSE,
                   sidebarMenuOutput("UImmstatTabs"),
                   uiOutput("outputId"="UIbreaks"),
                   uiOutput("outputId"="UIrug"),
                   uiOutput("outputId"="UIdata"),
                   uiOutput("outputId"="UIvar1"),
                   uiOutput("outputId"="UIlang"),
                   
                   shiny::tags$div(align="center",
                                   shiny::tags$hr(),
                                   shiny::tags$a(href = "https://github.com/sigbertklinke/shinyApp", "Created with shinyApp"),
                                   shiny::tags$br(),
                                   shiny::tags$a(target="_blank", href="https://www.wihoforschung.de/de/flipps-1327.php",  "Supported by BMBF"))),
  dashboardBody(tabItems(tabItem("tabName"="mmstatItem1",
                                 shiny::plotOutput("outputId"="Histogram",
                                                   "width"="100%",
                                                   "height"="400px",
                                                   "inline"=FALSE)),
                         tabItem("tabName"="mmstatItem2",
                                 shiny::plotOutput("outputId"="Boxplot",
                                                   "width"="100%",
                                                   "height"="400px",
                                                   "inline"=FALSE))))
)

server <- function(input, output, session) {
  seed <- list(inBookmark=FALSE)
  
  onBookmark(function(state) {
    state$seed <- seed
  })
  
  onRestore(function(state) {
    seed <- state$seed
    seed$inBookmark <- TRUE
  })
  
  onRestored(function(state) {
    seed$inBookmark <- FALSE
  })
  
  onStop(function() {
    if (isLocal()) {
      count <- getMMstat('lang', 'stats', 'count')
      cat(sprintf('gettext("%s"); // %.0f\n', names(count), count))
    }
  })
  
  value <- function(val) {
    param <- substitute(val)
    if(param=="input$breaks") { v<-toNum(val, min=1, max=50); if(is.na(v)) return(10) else return(v) }
    if(param=="input$rug") { v<-toLog(val); if(is.na(v)) return(FALSE) else return(val) }
    if(param=="input$data") { v<-toInt(val, min=1); if(is.na(v)) return(1) else return(v) }
    if(param=="input$var1") { v<-toInt(val, min=1); if(is.na(v)) return(1) else return(v) }
    if(param=="input$lang") { v<-toInt(val, min=1); if(is.na(v)) return(1) else return(v) }
    return(val)
  }
  
  observe({
    selectLanguage(value(input$lang))
    sel  <- value(isolate(input$breaks))
    shiny::updateSliderInput("session"=session,
                             "inputId"="breaks",
                             "label"=getText("Number of bins"),
                             "value"=sel,
                             "min"=1,
                             "max"=50,
                             "step"=NULL)
  })
  observe({
    selectLanguage(value(input$lang))
    sel<-value(isolate(input$rug))
    shiny::updateCheckboxInput("session"=session,
                               "inputId"="rug",
                               "label"=getText("Show observations"),
                               "value"=sel)
  })
  observe({
    selectLanguage(value(input$lang))
    sel<-value(isolate(input$data))
    shiny::updateSelectInput("session"=session,
                             "inputId"="data",
                             "label"=getText("Choose data set"),
                             "choices"=getList("BOSTON"=1,"MTCARS"=2),
                             "selected"=sel)
  })
  observe({
    selectLanguage(value(input$lang))
    sel     <- value(isolate(input$var1))
    sattr   <- getSelectionAttr(value(input$data))
    choices <- asChoices(rownames(sattr), sattr[,'var1']>0)
    updateSelectInput("session"=session,
                      "inputId"="var1",
                      "choices"=choices,
                      "selected"=sel,
                      "label"=getText("Choose variable"))
  })
  observe({
    selectLanguage(value(input$lang))
    sel<-value(isolate(input$lang))
    shiny::updateSelectInput("session"=session,
                             "inputId"="lang",
                             "label"=getText("Choose language"),
                             "choices"=asChoices(getMMstat("lang", "pon")),
                             "selected"=sel)
  })
  
  output$Histogram <- shiny::renderPlot({
    selectLanguage(value(input$lang))
    #C:\Users\sk\Desktop\syncthing\projekte\R\shinyApp\inst\app\histogramlang\hist9.R
    # shinyApp/inst/app/histogram/hist9.R
    x <- getDataSelection(value(input$data), value(input$var1))
    b <- seq(min(x), max(x), length.out=value(input$breaks)+1)
    hist(x, breaks=b, main=getText(attr(x, "varnames")))
    if(value(input$rug)) rug(x)
  })
  output$Boxplot <- shiny::renderPlot({
    selectLanguage(value(input$lang))
    #C:\Users\sk\Desktop\syncthing\projekte\R\shinyApp\inst\app\histogramlang\box9.R
    # shinyApp/inst/app/histogram/box9.R
    x <- getDataSelection(value(input$data), value(input$var1))
    boxplot(x, horizontal = TRUE, main=getText(attr(x, "varnames")))
    if(value(input$rug)) rug(x)
  })
  output$UImmstatTabs <- renderMenu({
    selectLanguage(value(input$lang))
    
    sidebarMenu("id"="mmstatTabs",
                menuItem("text"=getText("Histogram"),
                         "tabName"="mmstatItem1"),
                menuItem("text"=getText("Boxplot"),
                         "tabName"="mmstatItem2"))
  })
  output$UIbreaks<- renderUI({
    shiny::sliderInput("inputId"="breaks",
                       "label"=getText("Number of bins"),
                       "min"=1,
                       "max"=50,
                       "value"=10,
                       "step"=NULL,
                       "round"=FALSE,
                       "ticks"=TRUE,
                       "animate"=FALSE,
                       "width"=NULL,
                       "sep"=",",
                       "pre"=NULL,
                       "post"=NULL,
                       "timeFormat"=NULL,
                       "timezone"=NULL,
                       "dragRange"=TRUE)
  })
  output$UIrug <- renderUI({
    shiny::checkboxInput("inputId"="rug",
                         "label"=getText("Show observations"),
                         "value"=FALSE,
                         "width"=NULL)
  })
  output$UIdata <- renderUI({
    shiny::selectInput("inputId"="data",
                       "label"=getText("Choose data set"),
                       "choices"=getList("BOSTON"=1,"MTCARS"=2),
                       "selected"=NULL,
                       "multiple"=FALSE,
                       "selectize"=TRUE,
                       "width"=NULL,
                       "size"=NULL)
  })
  output$UIvar1 <- renderUI({
    shiny::selectInput("inputId"="var1",
                       "label"=getText("Choose variable"),
                       "choices"=NULL,
                       "selected"=NULL,
                       "multiple"=FALSE,
                       "selectize"=TRUE,
                       "width"=NULL,
                       "size"=NULL)
  })
  output$UIlang <- renderUI({
    shiny::selectInput("inputId"="lang",
                       "label"=getText("Choose language"),
                       "choices"=asChoices(getMMstat("lang", "pon")),
                       "selected"=NULL,
                       "multiple"=FALSE,
                       "selectize"=TRUE,
                       "width"=NULL,
                       "size"=NULL)
  })
}

shinyApp(ui, server)
