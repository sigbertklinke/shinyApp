# shinyApp/inst/app/histogram/hist7_app.R
library("shinyApp")
ShinyApp() %>%
  PlotOutput('plot', file='hist7.R') %>%
  SliderInput('breaks', 'Number of bins', min=1, max=50, value=10) %>%
  CheckboxInput('rug', 'Show observations') %>%
  DataInput('data', BOSTON=Boston, MTCARS=mtcars) %>%
  VariableInput('var1', is=is.numeric) %>%
  Global({ data(Boston, package="MASS") }) %>%
  WriteApp()  
