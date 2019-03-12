# shinyApp/inst/app/scagnostics/scagnostics_app.R 
library("shinyApp")
ShinyApp() %>%
  PlotOutput('plot', file='scagnostics.R') %>%
  SelectInput("coef", "Select coefficient", 
               c("Outlying", "Skewed", "Clumpy", "Sparse", "Striated", 
               "Convex", "Skinny", "Stringy", "Monotonic")) %>%
  SliderInput("index", "Plot number", min=1, max=91, value=1) %>%
  Global({
    library("scagnostics")
    data(Boston, package="MASS")
    scag <- scagnostics(Boston)
  }) %>%
  WriteApp()  
