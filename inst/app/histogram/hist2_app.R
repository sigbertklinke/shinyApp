# shinyApp/inst/app/histogram/hist2_app.R
library("shinyApp")
ShinyApp() %>%
  PlotOutput('plot', file='hist2.R') %>%
  SliderInput('breaks', 'Number of bins', min=1, max=50, value=10) %>%
  WriteApp()  