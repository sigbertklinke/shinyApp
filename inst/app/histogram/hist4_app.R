# shinyApp/inst/app/histogram/hist4_app.R
library("shinyApp")
ShinyApp() %>%
  PlotOutput('plot', file='hist4.R') %>%
  SliderInput('breaks', 'Number of bins', min=1, max=50, value=10) %>%
  CheckboxInput('rug', 'Show observations') %>%
  DataInput('data', BOSTON=MASS::Boston) %>%
  WriteApp() 
