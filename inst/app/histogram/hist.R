browser()
x <- getDataSelection(value(input$data),value(input$var1))
hist(x, breaks=seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=value(input$breaks)+1),
     sub=sprintf("Data: %.0f, Var: %.0f", value(input$data), value(input$var1)),
     main=value(input$lang))
if (value(input$rug)) rug(x)
