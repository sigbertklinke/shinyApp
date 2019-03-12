.onLoad <- function(libname, pkgname) {
  pkg <- c('shinyExample', 'shinyApp')
  ver <- sapply(pkg, packageVersion)
  if (length(unique(ver))>1) stop('Package version do not match', sprintf('Package "%s" has version %s', pkg, ver))
}