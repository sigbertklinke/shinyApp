# shinyApp/inst/app/histogram/hist6_app.R
library("shinyApp")
ShinyApp() %>%
  PlotOutput('plot', file='hist6.R') %>%
  SliderInput('breaks', 'Number of bins', min=1, max=50, value=10) %>%
  CheckboxInput('rug', 'Show observations') %>%
  DataInput('data', BOSTON=Boston) %>%
  VariableInput('var1', is=is.numeric) %>%
  Global({ data(Boston, package="MASS") }) %>%
  WriteApp()  

