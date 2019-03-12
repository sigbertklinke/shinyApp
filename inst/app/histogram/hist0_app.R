# shinyApp/inst/app/histogram/hist0_app.R
library("shinyApp")
ShinyApp() %>%
  PlotOutput('plot', file='hist0.R') %>%
  WriteApp()