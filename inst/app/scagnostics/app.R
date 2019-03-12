library(shiny)
library(shinydashboard)
library(shinyWidgets)

readData <- function (file, ...) 
{
  vartype <- list(...)
  nvartype <- length(vartype)
  data <- readRDS(file)
  tdata <- names(data)
  ndata <- length(data)
  selectable <- list()
  for (i in 1:ndata) {
    selectable[[i]] <- matrix(FALSE, nrow = length(data[[i]]), 
                              ncol = nvartype)
    for (j in 1:nvartype) selectable[[i]][, j] <- sapply(data[[i]], 
                                                         vartype[[j]])
    if (any(colSums(selectable[[i]]) == 0)) 
      stop(sprintf("Data set \"%s\" has no valid selection", 
                   tdata[i]))
    attr(data[[i]], "varnames") <- names(data[[i]])
    colnames(selectable[[i]]) <- names(vartype)
    attr(data[[i]], "selectable") <- selectable[[i]]
  }
  data
}
translations <- function (path = ".") 
{
  po <- new.env()
  po$local <- (Sys.getenv("SHINY_PORT") == "")
  po$files <- list.files(path = path, pattern = "*.po$", full.names = TRUE)
  po$msgs <- list()
  po$stats <- list(count = numeric())
  po$sel <- 0
  if (length(po$files)) {
    for (i in seq(po$files)) {
      pon <- sapply(strsplit(po$files[i], ".", fixed = T), 
                    function(elem) {
                      elem[1]
                    })
      msg <- paste(readLines(paste(path, po$files[i], sep = "/")), 
                   collapse = " ")
      msgid <- regmatches(msg, gregexpr("msgid\\s*\".*?\"", 
                                        msg))
      tmp <- strsplit(msgid[[1]], "\"")
      msgid <- sapply(tmp, function(vec) {
        paste0(vec[2:length(vec)])
      })
      msgstr <- regmatches(msg, gregexpr("msgstr\\s*\".*?\"", 
                                         msg))
      tmp <- strsplit(msgstr[[1]], "\"")
      msgstr <- sapply(tmp, function(vec) {
        paste0(vec[2:length(vec)])
      })
      po$msgs[[pon]] <- list(id = msgid, str = msgstr)
      po$stats$count[msgid] <- 0
      po$stats[[pon]] <- msgid
    }
    po$sel <- 1
  }
  return(po)
}
getText <- function (msg) 
{
  if (mmstat$msg$local) {
    mmstat$msg$stats$count[msg] <- mmstat$msg$stats$count[msg] + 
      1
    mmstat$msg$stats$count[msg][is.na(mmstat$msg$stats$count[msg])] <- 1
  }
  if (mmstat$msg$sel == 0) 
    return(msg)
  ret <- msg
  pos <- match(msg, mmstat$msg$msgs[[mmstat$msg$sel]]$id)
  ind <- (1:length(pos))[!is.na(pos)]
  ret[ind] <- mmstat$msg$msgs[[mmstat$msg$sel]]$str[pos[ind]]
  return(ret)
}
as.choices <- function (txt, inc = NULL) 
{
  if (is.null(inc)) 
    inc <- rep(TRUE, length(txt))
  ret <- as.list((1:length(txt))[inc])
  names(ret) <- txt[inc]
  ret
}
getDataSelection <- function (dindex, ..., simplify = TRUE) 
{
  sel <- getSelection(attr(mmstat$data[[dindex]], "selectable"), 
                      as.list(...))
  nsel <- ncol(sel)
  ret <- list()
  for (i in 1:nsel) ret[[i]] <- as.data.frame(mmstat$data[[dindex]][, 
                                                                    which(sel[, i] == 2)])
  if (simplify) {
    if (nsel > 1) {
      retdf <- ret[[1]]
      for (i in 2:nsel) retdf <- cbind(retdf, ret[[i]])
      ret <- retdf
    }
    else {
      ret <- ret[[1]][, 1]
    }
  }
  attr(ret, "selectable") <- attr(mmstat$data[[dindex]], "selectable")
  ret
}
getSelection <- function (selectable, input, duplicates.ok = FALSE) 
{
  nvar <- nrow(selectable)
  ninp <- ncol(selectable)
  if (length(input) != ninp) 
    stop("input length does not fit")
  newsel <- selectable
  for (i in 1:ninp) {
    for (j in 1:length(input[[i]])) {
      if (newsel[input[[i]][j], i] == 1) 
        newsel[input[[i]][j], i] <- 2
    }
  }
  if (!duplicates.ok) {
    for (i in 1:nvar) {
      pos <- which(newsel[i, ] == 2)
      if (length(pos) > 1) 
        newsel[i, pos[-1]] <- 0
      if (length(pos)) {
        newsel[i, ] <- 0
        newsel[i, pos[1]] <- 2
      }
    }
  }
  for (i in 1:ninp) {
    pos <- which(newsel[, i] == 2)
    if (length(pos) == 0) {
      pos1 <- which(newsel[, i] == 1)
      newsel[pos1[1], i] <- 2
    }
  }
  newsel
}

mmstat     <- new.env()
mmstat$msg <- translations()
if (file.exists('mmstat.RDS')) mmstat$data <- readRDS('mmstat.RDS')



ui <- dashboardPage(
  dashboardHeader(title="MM*Stat", titleWidth=, disable=FALSE),
  dashboardSidebar(collapsed=FALSE, width=, disable=FALSE,
                   uiOutput("outputId"="UIcoef"),
                   uiOutput("outputId"="UIindex"),
                   shiny::tags$div(align="center",
                                   shiny::tags$hr(),
                                   shiny::tags$a(href = 'https://www.sigbertklinke.de', 'Created with shinyExample'),
                                   shiny::tags$br(),
                                   shiny::tags$a(target="_blank", href="https://www.wihoforschung.de/de/flipps-1327.php",  'Supported by BMBF')
                   )
  ),
  dashboardBody(
    shiny::plotOutput("outputId"="plot",
                      "width"="100%",
                      "height"="400px",
                      "inline"=FALSE)
  )
)

server <- function(input, output, session) {
  seed <- list(inBookmark=FALSE)
  
  onBookmark(function(state) {
    state$seed <- seed
  })
  
  onRestore(function(state) {
    seed <- state$seed
    seed$inBookmark <- TRUE
  })
  
  onRestored(function(state) {
    seed$inBookmark <- FALSE
  })
  
  onStop(function() {
    if (mmstat$msg$local) cat(sprintf('gettext("%s"); // %.0f\n', 
                                      names(mmstat$msg$stats$count),
                                      mmstat$msg$stats$count))
  })
  
  value <- function(val) {
    param <- substitute(val)
    if(param=="input$coef") { if(is.null(val)) return(1) else return(as.numeric(val)) }
    if(param=="input$index") { if(is.null(val)||(val< 1)||(val> 91)) return(1) else return(val) }
    return(val)
  }
  
  output$plot <- shiny::renderPlot({
    #/home/sigbert/syncthing/projekte/R/shinyExample/inst/examples/app/scagnostics/scagnostics.R
    library("MASS")
    Boston  <- mmstat$data$data
    scag    <- attr(Boston, 'scagnostics')
    vlist   <- strsplit(colnames(scag), ' * ', fixed=TRUE)
    coef    <- value(input$coef)
    pnr     <- value(input$index)
    o       <- order(scag[coef,])
    main    <- c("Outlying", "Skewed", "Clumpy", "Sparse", "Striated", "Convex", "Skinny", "Stringy", "Monotonic")
    main    <- sprintf("%s(%.0f): %.3f", main[coef], pnr, scag[coef,o[pnr]])
    par(mfrow=c(1,2))
    plot(Boston[,vlist[[o[pnr]]]], pch=19, main=main, cex=0.5)
    plot(Boston[,rev(vlist[[o[pnr]]])], pch=19, main=main, cex=0.5)
  })
  output$UIcoef<- renderUI({
    #RENDERUI
    shiny::selectInput("inputId"="coef",
                       "label"=getText("Select coefficient"),
                       "choices"=list("Outlying"=1,
                                      "Skewed"=2,
                                      "Clumpy"=3,
                                      "Sparse"=4,
                                      "Striated"=5,
                                      "Convex"=6,
                                      "Skinny"=7,
                                      "Stringy"=8,
                                      "Monotonic"=9),
                       "selected"=NULL,
                       "multiple"=FALSE,
                       "selectize"=TRUE,
                       "width"=NULL,
                       "size"=NULL)
  })
  output$UIindex<- renderUI({
    #RENDERUI
    shiny::sliderInput("inputId"="index",
                       "label"=getText("Plot number"),
                       "min"=1,
                       "max"=91,
                       "value"=1,
                       "step"=NULL,
                       "round"=FALSE,
                       "format"=NULL,
                       "locale"=NULL,
                       "ticks"=TRUE,
                       "animate"=FALSE,
                       "width"=NULL,
                       "sep"=",",
                       "pre"=NULL,
                       "post"=NULL,
                       "timeFormat"=NULL,
                       "timezone"=NULL,
                       "dragRange"=TRUE)
  })
}

shinyApp(ui, server)
