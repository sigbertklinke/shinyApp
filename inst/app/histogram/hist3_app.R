# shinyApp/inst/app/histogram/hist3_app.R
library("shinyApp")
ShinyApp() %>%
  PlotOutput('plot', file='hist3.R') %>%
  SliderInput('breaks', 'Number of bins', min=1, max=50, value=10) %>%
  CheckboxInput('rug', 'Show observations') %>%
  WriteApp()  
