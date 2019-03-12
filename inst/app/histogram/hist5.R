# shinyApp/inst/app/histogram/hist5.R
x <- Boston$medv
b <- seq(min(x), max(x), length.out=value(input$breaks)+1)
hist(x, breaks=b)
if(value(input$rug)) rug(x)