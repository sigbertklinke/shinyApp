#' WriteApp
#'
#' Writes a ShinyApp object to a file. 
#'
#'
#' @param app ShinyApp object
#' @param file character: name of file to write to (default: \code{app.R} in the current working directory)
#' @param edit function: a function to edit the file or \code{NULL} for no edit (default: \code{\link[utils]{file.edit}}) 
#' @param copyright character: some copyright information (default: \code{NULL}). The default copyright notice is 
#' \preformatted{
#'  shiny::tags$div(align="center",
#'    shiny::tags$hr(),
#'    shiny::tags$a(href = "https://github.com/sigbertklinke/shinyApp", "Created with shinyApp"),
#'    shiny::tags$br(),
#'    shiny::tags$a(target="_blank", href="https://www.wihoforschung.de/de/flipps-1327.php",  "Supported by BMBF")
#'  )
#' }
#' @param ...  unused
#'
#' @return invisibly the ShinyApp object
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% PlotOutput('plot', text='plot(runif(10), runif(10))') %>% WriteApp()}
WriteApp <- function (app, file='app.R', edit='file.edit', copyright=NULL, ...) {
  #browser()
  mergeLines <- function (code, collapse="\n", final='') {
    if (is.null(code) || (code=='')) return('')
    paste0(paste0(code, collapse=collapse), final)
  }
  #
  check_ID <- function(prg, id, txt='') {
    if (any(id %in% prg$Id)) {
      prg$Id <- rbind(prg$id, cbind(id, rep(txt, length(id))))
      oid <- order(prg$Id)
      print(cbind(prg$Id[oid], prg$Type[oid]))
      stop('duplicate inputId/outputID/id')
    }
    prg$Id <- rbind(prg$id, cbind(id, rep(txt, length(id))))
    prg
  }
  #
  if (!inherits(app, 'ShinyApp')) stop("No shiny app")
  prg <- list(Server=c(), Language=c(),
              Value=c(), Final=c(), Dataset=c(), Variable=c(),
              Global=app$Global, Observer=c(), SetLang=c(),
              Id = matrix('', ncol=2, nrow=0)
  )
  #browser()
  if (length(app$output)>1) { # create menu items
    onames <- sapply(app$output, function(e) { e[['Id']][1] })
    prg <- check_ID(prg, 'mmstatTabs', 'sidebarMenu')
    prg <- check_ID(prg, paste0('mmstatItem', seq(onames)), 'menuItem')
    menuItemList <- list(id="mmstatTabs")
    tabItemList  <- list()
    for (i in seq(app$output)) {
      prg <- check_ID(prg, app$output[[i]]$Id, app$output[[i]]$Type)
      prg <- appendToPrg(prg, app$output[[i]])
      #menuItemList <- c(menuItemList, list(str_call('menuItem', list(text=onames[i], tabName=paste0("mmstatItem", i)))))
      menuItemList[[1+length(menuItemList)]]  <- I(unclass(str_call('menuItem', list(text=I(sprintf("getText(\"%s\")", onames[i])), tabName=paste0("mmstatItem", i)))))
      tabItemList[[1+length(tabItemList)]]  <- I(unclass(str_call('tabItem',  list(tabName=paste0("mmstatItem", i), app$output[[i]]$Body))))
    }
    app$DashboardSidebar[[1+length(app$DashboardSidebar)]] <- I('sidebarMenuOutput("UImmstatTabs")')
    prg$Server <- c(prg$Server, "output$UImmstatTabs <- renderMenu({\n{{Language}}\n", str_call('sidebarMenu', menuItemList), "})")
    #DashboardSidebar[[1+length(app$DashboardSidebar)]] <- I(unclass(str_call('sidebarMenu', menuItemList)))
    app$DashboardBody[[1+length(app$DashboardBody)]] <- I(unclass(str_call('tabItems', tabItemList)))
    
    #browser()
    #prg <- check_ID(prg, 'UImmstatTabs', 'sidebarMenuOutput')
    #app$DashboardSidebar[[1+length(app$DashboardSidebar)]] <- str_call('sidebarMenuOutput', list(outputId='UImmstatTabs'))
    #prg <- appendToPrg(prg,
    #                   list(Server  = paste0("output$UImmstatTabs <- renderMenu({\n", str_call('sidebarMenu', menuItemList), "\n})"),
    #                        Body    = str_call('tabItems', tabItemList)
    #                   )
    #)
  } else {
    prg <- check_ID(prg, app$output[[1]]$Id, app$output[[1]]$Type)
    app$DashboardBody[[1+length(app$DashboardBody)]] <- app$output[[1]]
  }
  if(length(app$input)) {
    for (i in seq(app$input)) {
      prg <- check_ID(prg, app$input[[i]]$Id, app$input[[i]]$Type)
      prg <- appendToPrg(prg, app$input[[i]]) 
      if(!is.null(app$input[[i]]$Sidebar)) {
        app$DashboardSidebar[[length(app$DashboardSidebar)+1]] <- I(unclass(app$input[[i]]$Sidebar))   
      }
    }
  }
  # setup prg
  #browser()
  for (obj in names(prg)) prg[[obj]] <-  mergeLines(prg[[obj]])
  prg$DashboardBody    <- str_call('dashboardBody', app$DashboardBody)
  prg$DashboardHeader  <- app$DashboardHeader
  prg$DashboardSidebar <- app$DashboardSidebar
  if (is.null(copyright)) { copyright <- '
    shiny::tags$div(align="center",
                    shiny::tags$hr(),
                    shiny::tags$a(href = "https://github.com/sigbertklinke/shinyApp", "Created with shinyApp"),
                    shiny::tags$br(),
                    shiny::tags$a(target="_blank", href="https://www.wihoforschung.de/de/flipps-1327.php",  "Supported by BMBF"))'      
  }
  prg$DashboardSidebar[[length(prg$DashboardSidebar)+1]] <- I(unclass(copyright))
  prg$DashboardSidebar <- str_call('dashboardSidebar', prg$DashboardSidebar)
  prg$text <- readLines(app$template)
  prg$text <- do.call('template', prg)
  prg      <- do.call('template', prg) 
  file <- normalizePath(file)
  writeLines(prg, file)
  if (!is.null(edit)) {
    fun <- match.fun(edit)
    fun(file)
  }
  invisible(app)
}