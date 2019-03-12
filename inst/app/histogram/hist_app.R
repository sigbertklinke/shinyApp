setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # RStudio only
# data set
CARS      <- readRDS("/home/sk/syncthing/buecher_papers/mmstat_en/mmstat_en/quantnet/MMSTAT_20150622/+histogram_simple/CARS.rds")
DECATHLON <- readRDS("/home/sk/syncthing/buecher_papers/mmstat_en/mmstat_en/quantnet/MMSTAT_20150622/+histogram_simple/DECATHLON.rds")
USCRIME   <- readRDS("/home/sk/syncthing/buecher_papers/mmstat_en/mmstat_en/quantnet/MMSTAT_20150622/+histogram_simple/USCRIME.rds")
library("shinyExample")
dataOut("mmstat-histogram.rds", CARS=CARS, DECATHLON=DECATHLON, USCRIME=USCRIME)

# app
data   <- dataIn("data", "Choose data set", file="mmstat-histogram.rds",
                 varIn("var1", "Choose variable", type=is.numeric)
                 )
#lang   <- langIn("lang", "Choose language")
lang  <- selectIn("lang", "Select language", c("German", "English"))
#
breaks <- sliderIn("breaks", "Number of bins", min=1, max=50, value=30)
rug    <- checkboxIn("rug", "Show observations")
makeShinyApp(input=list(breaks, rug, data, lang),
             output=plotOut('plot', file='hist.R')
)

