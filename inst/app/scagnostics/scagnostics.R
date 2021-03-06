# shinyApp/inst/app/scagnostic/scagnostics.R
vlist   <- strsplit(colnames(scag), ' * ', fixed=TRUE)
coef    <- value(input$coef)
pnr     <- value(input$index)
o       <- order(scag[coef,])
main    <- c("Outlying", "Skewed", "Clumpy", "Sparse", "Striated", "Convex", "Skinny", "Stringy", "Monotonic")
main    <- sprintf("%s(%.0f): %.3f", main[coef], pnr, scag[coef,o[pnr]])
par(mfrow=c(1,2))
plot(Boston[,vlist[[o[pnr]]]], pch=19, main=main, cex=0.5)
plot(Boston[,rev(vlist[[o[pnr]]])], pch=19, main=main, cex=0.5)
