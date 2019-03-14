# shinyApp/inst/app/histogram/hist9_app.R
library("shinyApp")
ShinyApp() %>%
  PlotOutput('Histogram', file='hist9.R') %>%
  PlotOutput('Boxplot', file='box9.R') %>%
  SliderInput('breaks', 'Number of bins', min=1, max=50, value=10) %>%
  CheckboxInput('rug', 'Show observations') %>%
  DataInput('data', BOSTON=Boston, MTCARS=mtcars) %>%
  VariableInput('var1', is=is.numeric) %>%
  Global({ data(Boston, package="MASS") }) %>%
  WriteApp(file="~/app.R")  
