#' DashboardHeader
#'
#' @param app a ShinyApp object
#' @inheritParams shinydashboard::dashboardHeader
#'
#' @return an updated ShinyApp object
#' @export
#'
#' @examples
#' \dontrun{ShinyApp %>% DashboardHeader('MM*Stat')}
DashboardHeader <- function(app, title = NULL, titleWidth = NULL, disable = FALSE) {
  if (!inherits(app, 'ShinyApp')) stop("No shiny app")
  app$DashboardHeader <- str_call('dashboardHeader', 
                                  list(title=title, titleWidth=titleWidth, disable=disable))
  app
}

#' DashboardSidebar
#'
#' @param app a ShinyApp object
#' @inheritParams shinydashboard::dashboardSidebar
#'
#' @return an updated ShinyApp object
#' @export
#'
#' @examples
#' #' \dontrun{ShinyApp %>% DashboardSidebar(collapsed = TRUE)}
DashboardSidebar<- function(app,  disable = FALSE, width = NULL, collapsed = FALSE) {
  if (!inherits(app, 'ShinyApp')) stop("No shiny app")
  app$DashboardSidebar <- list(disable=disable, width=width, collapsed=collapsed)
  app
}