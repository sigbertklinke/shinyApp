# shinyApp/inst/app/correlation/corr_app.R
library("shinyApp")
ShinyApp() %>%
  PlotOutput('plot', file='corr.R') %>%
  CorrelationInput('r') %>%
  SampleSizeInput('n') %>%
  WriteApp()  