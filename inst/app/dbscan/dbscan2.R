# shinyApp/inst/app/dbscan/dbscan2.R
db  <- dbscan(x[,c(4,6)], value(input$eps), value(input$pts))
col <- c('grey', rainbow(max(db$cluster)))
plot(x[,c(4,6)], col=col[1+db$cluster], pch=19, asp=TRUE)