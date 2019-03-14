# shinyApp/inst/app/histogram/box9.R
x <- getDataSelection(value(input$data), value(input$var1))
boxplot(x, horizontal = TRUE, main=attr(x, "varnames"))
if(value(input$rug)) rug(x)