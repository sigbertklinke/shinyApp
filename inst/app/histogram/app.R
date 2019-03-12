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
    uiOutput("outputId"="UIbreaks"),
uiOutput("outputId"="UIrug"),
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
    if(param=="input$breaks") { if(is.null(val)||(val< 1)||(val> 50)) return(10) else return(val) }
if(param=="input$rug") { if(is.null(val)) return(FALSE) else return(val) }
    return(val)
  }

  output$plot <- shiny::renderPlot({
#/home/sigbert/syncthing/projekte/R/shinyExample/inst/examples/app/histogram/hist3.R
library("MASS")
x <- Boston$medv
b <- seq(min(x), max(x), length.out=value(input$breaks)+1)
hist(x, breaks=b)
if(value(input$rug)) rug(x)
})
output$UIbreaks<- renderUI({
#RENDERUI
shiny::sliderInput("inputId"="breaks",
"label"=getText("Number of bins"),
"min"=1,
"max"=50,
"value"=10,
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
output$UIrug<- renderUI({
#RENDERUI
shiny::checkboxInput("inputId"="rug",
"label"=getText("Show observations"),
"value"=FALSE,
"width"=NULL)
})
}

shinyApp(ui, server)
