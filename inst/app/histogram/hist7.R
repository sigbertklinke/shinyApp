# shinyApp/inst/app/histogram/hist7.R
x <- getDataSelection(value(input$data), value(input$var1))
b <- seq(min(x), max(x), length.out=value(input$breaks)+1)
hist(x, breaks=b, main=attr(x, "varnames"))
if(value(input$rug)) rug(x)