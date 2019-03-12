# shinyApp/inst/app/histogram/hist5_app.R
library("shinyApp")
ShinyApp() %>%
  PlotOutput('plot', file='hist5.R') %>%
  SliderInput('breaks', 'Number of bins', min=1, max=50, value=10) %>%
  CheckboxInput('rug', 'Show observations') %>%
  Global({ library("MASS") }) %>%
  WriteApp()  
