
source("FunctionHelperFPA.R")

require(RMySQL)
# mydb = dbConnect(MySQL(), user='root', password='', dbname='fastforum', host='10.100.244.108')
## FPA-SVR
#library(FPASVR)
library(xts)
library(plotly)


library(shiny)
library(knitr)
library(R.utils)
library(markdown)
library(ggplot2)
library(DT)
library(datasets)
library(GGally)
library(nortest)
library(xtable)
library(shinyBS)
library(googleVis)
library(clValid)
library(cluster)
library(gdata)
library(foreign)
library(Hmisc)
library(zoo)
library(rmarkdown)
library(corrplot)
library(ridge)
library(frbs) #prima
library(survival) #bayu
library(KMsurv) #bayu
library(randomForest) #ezi
library(gtable) #ezi
library(GPArotation)
library(magrittr)
library(dygraphs)
library(Rcpp)
library(shinyjs)
library(lmtest)
library(DAAG)
library(maptools)
library(ggplot2)
library(scatterD3)
library(MASS)
library(gtools)
library(jsonlite)
library(pastecs)
library(rmarkdown)
library(biclust)
library(arules)
library(arulesViz)


# library(arulesViz)
# library(arules)

# library(rgdal)
# library(ggplot2)

# library(rgdal)
# library(ggplot2)
# library(rmarkdown)
#================= 

#library(forecast)
#library(censReg)
# #library(tseries)
# #library(writeXLS)
# # library(shinyIncubator)
# # library(fpc) #prima
# # library(rgdal) #fikri
# # library(rgeos) #fikri
#library(ape)
# library(spdep)
# library(RcppArmadillo)
#======================

max_plots <- 10
#source("tools/script/FRBS.pmml.R")
#source("tools/script/pmml.frbs.R")
#source("tools/script/pmml.R")
shinyServer(function(input, output, session){
  
  # fungsi source untuk meload fungsi yang dipakai bersama
  source('fast.R', local = TRUE)
  setInitValues <- function() {
    # initialize state list and reactive values
    if(testingFast) {
      # load previous state for testing
      
    } else {
      
      state_list <<- list()
      values <<- reactiveValues()

      #initial data for soft computing fts-aco
      #elan
      valuesFTSACO <<- reactiveValues()
      valuesFTSACO$FTSACOlearnBtn <- 0
      
      # initial plot height and width
      values$plotHeight <- 650
      values$plotWidth <- 650
      
      values$modellist <- list()
      
      nilaiTabel <<- reactiveValues()
      nilaiTabel[["tmp"]] <- data.frame()
      nilaiTabel[["flag"]] <- 0
      
      values$report <- list()
      
      nilaiTabel[["frequency"]] <- 12
      nilaiTabel[["start"]] <- 2004
      
      #initial data for soft computing neuro-fuzzy
      #prima
      #values$modelFuzzy <- list()
      #tabelHasilFuzzy <<- reactiveValues()
      #tabelHasilFuzzy[["tmp"]] <- data.frame()
      errorKFoldFuzzy <- list()
      valuesANFIS <<- reactiveValues()
      valuesANFIS$counter <- 0
      valuesANFIS$butt1 <- 0
      valuesANFIS$butt2 <- 0
      valuesANFIS$butt3 <- 0
      valuesANFIS$butt4 <- 0
      valuesANFIS$butt5 <- 0
      valuesANFIS$nullbutt <- 0
      valuesANFIS$learnButton <- 0
      #end initial data for soft computing neuro-fuzzy
      
      # Datasets can change over time (i.e. the changedata function). Therefore,
      # the data need to be a reactive value so the other reactive functions
      # and outputs that depend on these datasets will know when they are changed.
      # robj <- load("../base/data/data_init/diamonds.rda") 
      
            robj <- load("data/data_init/diamonds.rda") 
            df <- get(robj)
            values[["diamonds"]] <- df
            values[["diamonds_descr"]] <- attr(df,'description')
            values$datasetlist <- c("diamonds")
#       robj <- load("data/data_init/IHK_TahunDasar2012.rda") 
#       df <- get(robj)
#       values[["IHK_TahunDasar2012"]] <- df 
#       values[["IHK_TahunDasar2012_descr"]] <- attr(df,'description')
#       values$datasetlist <- c("IHK_TahunDasar2012")
    }
  }
  
  setInitValues()   # using a function here so it can also be called from state.R to reset the app
  
  
  # source dari data & alat analisis
  #...
  R.utils::sourceDirectory('tools/analysis', recursive = TRUE, modifiedOnly = FALSE) 
  R.utils::sourceDirectory('tools/data', recursive = TRUE, modifiedOnly = FALSE)
  R.utils::sourceDirectory('tools/app', recursive = TRUE, modifiedOnly = FALSE)

observe ({

    if(is.null(input$startClusteringFgwcGsa)) return()
  if(input$startClusteringFgwcGsa){
      updateTabsetPanel(session,'tabPanelFgwcGsa',selected = 'tabSummary')
      updateCollapse(session,'summaryPanel',open = 'Summary Clustering')
  }

  if(is.null(input$shareok)) return()
  if(input$shareok<1) return ()
  isolate ({
    toggleModal(session, "mo3")
    
    
  })	})

observeEvent(input$startClusteringFgwcGsa,{
  result <<- calculateFgwcGsa()
})

})