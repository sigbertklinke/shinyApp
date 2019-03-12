# shinyApp/inst/app/dbscan/dbscan_app.R
library("shinyApp")
ShinyApp() %>%
  SliderInput('eps', 'Core distance', min=0, max=1, step=0.01, value=0.5) %>%
  SliderInput('pts', 'Minimal neighbour ', min=2, max=10, step=1, value=5) %>%
  PlotOutput('plot', file='dbscan.R') %>%
  WriteApp()  
