#' WriteApp
#'
#' Writes a ShinyApp object to a file
#'
#' @param app ShinyApp object
#' @param file character: name of file to write to (default: \code{app.R} in the current working directory)
#' @param edit function: a function to edit the file or \code{NULL} for no edit (default: \code{\link[utils]{file.edit}}) 
#' @param ...  unused
#'
#' @return invisibly the ShinyApp object
#' @export
#'
#' @examples
#' \dontrun{ShinyApp() %>% PlotOutput('plot', text='plot(runif(10), runif(10))') %>% WriteApp()}
WriteApp <- function (app, file='app.R', edit='file.edit', ...) {
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
  prg <- list(Sidebar=c(), Server=c(), Body=c(), Language=c(),
              Value=c(), Final=c(), Dataset=c(), Variable=c(),
              Global=app$Global, Observer=c(), SetLang=c(),
              Id = matrix('', ncol=2, nrow=0)
  )
  if (length(app$output)>1) {
    onames <- sapply(app$output, function(e) { e[['Id']][1] })
    prg <- check_ID(prg, 'mmstatTabs', 'sidebarMenu')
    prg <- check_ID(prg, paste0('mmstatItem', seq(onames)), 'menuItem')
    menuItemList <- list(id="mmstatTabs")
    tabItemList  <- list()
    for (i in seq(app$output)) {
      prg <- check_ID(prg, app$output[[i]]$Id, app$output[[i]]$Type)
      menuItemList <- c(menuItemList, list(str_call('menuItem', list(text=onames[i], tabName=paste0("mmstatItem", i)))))
      tabItemList  <- c(tabItemList, list(str_call('tabItem',  list(tabName=paste0("mmstatItem", i), app$output[[i]]$Body))))
      if (!is.null(app$output[[i]]$Server)) prg$Server <- c(prg$Server, app$output[[i]]$Server, '')
    }
    #browser()
    prg <- check_ID(prg, 'UImmstatTabs', 'sidebarMenuOutput')
    prg <- appendToPrg(prg,
                       list(Server  = paste0("output$UImmstatTabs <- renderMenu({\n", str_call('sidebarMenu', menuItemList), "\n})"),
                            Sidebar = str_call('sidebarMenuOutput', list(outputId='UImmstatTabs')),
                            Body    = str_call('tabItems', tabItemList)
                       )
    )
  } else {
     if(length(app$output)) {
       prg <- check_ID(prg, app$output[[1]]$Id, app$output[[1]]$Type)
       prg <- appendToPrg(prg, app$output[[1]])
     }
  }
  if(length(app$input)) {
    for (i in seq(app$input)) {
      prg <- check_ID(prg, app$input[[i]]$Id, app$input[[i]]$Type)
      prg <- appendToPrg(prg, app$input[[i]]) 
    }
  }
  # setup prg
  #browser()
  prg$Sidebar <- mergeLines(prg$Sidebar, ",\n", ",")
  for (obj in names(prg)) prg[[obj]] <-  mergeLines(prg[[obj]])
  prg$DashboardHeader  <- app$DashboardHeader
  prg$DashboardSidebar <- app$DashboardSidebar
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