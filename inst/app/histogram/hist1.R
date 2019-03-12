# shinyApp/inst/app/histogram/hist1.R
library("MASS")
x <- Boston$medv
hist(x, breaks=value(input$breaks))