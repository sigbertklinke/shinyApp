# shinyApp/inst/app/histogram/hist1_app.R
library("shinyApp")
ShinyApp() %>%
  PlotOutput('plot', file='hist1.R') %>%
  SliderInput('breaks', 'Number of bins', min=1, max=50, value=10) %>%
  WriteApp()  