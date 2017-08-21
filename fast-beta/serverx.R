require(RMySQL)

options(shiny.deprecation.messages=FALSE)
options(shiny.port = 6962)

library(shiny)
library(knitr)
library(R.utils)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(wordcloud)
library(AlgDesign)
library(datasets)
library(GGally)
library(xtable)
library(shinyBS)
library(googleVis)
library(gdata)
library(foreign)
library(Hmisc)
library(rmarkdown)
library(markdown)
library(shinyAce)
library(rgdal)
library(rgeos)
library(maptools)
library(xlsx)
library(xlsxjars)
library(dygraphs)

max_plots <- 10
#source("tools/script/FRBS.pmml.R")
#source("tools/script/pmml.frbs.R")
#source("tools/script/pmml.R")
shinyServer(function(input, output, session){
  
  # fungsi source untuk meload fungsi yang dipakai bersama
  getClientData <- function(){
    return(session$clientData)
  }
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
      
#       #initial data for soft computing neuro-fuzzy
#       #prima
#       #values$modelFuzzy <- list()
#       #tabelHasilFuzzy <<- reactiveValues()
#       #tabelHasilFuzzy[["tmp"]] <- data.frame()
#       errorKFoldFuzzy <- list()
#       valuesANFIS <<- reactiveValues()
#       valuesANFIS$counter <- 0
#       valuesANFIS$butt1 <- 0
#       valuesANFIS$butt2 <- 0
#       valuesANFIS$butt3 <- 0
#       valuesANFIS$butt4 <- 0
#       valuesANFIS$butt5 <- 0
#       valuesANFIS$nullbutt <- 0
#       valuesANFIS$learnButton <- 0
#       #end initial data for soft computing neuro-fuzzy
      
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
  R.utils::sourceDirectory('tools/analysis', recursive = TRUE, modifiedOnly = FALSE)
  R.utils::sourceDirectory('tools/data', recursive = TRUE, modifiedOnly = FALSE)
  R.utils::sourceDirectory('tools/app', recursive = TRUE, modifiedOnly = FALSE)

observe ({
  
  if(is.null(input$startClusteringFgwcGsa)) return()
  if(input$startClusteringFgwcGsa){
      result <<- calculateFgwcGsa()
      updateTabsetPanel(session,'tabPanelFgwcGsa',selected = 'tabSummary')
      updateCollapse(session,'summaryPanel',open = 'Summary Clustering')
  }
  
  if(input$shareFgwcForum){
    src <- normalizePath('test.Rmd')
    
    library(rmarkdown)
    out <- render('test.Rmd',"html_document",output_dir = getwd(),output_file = "okeh.html")
    
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

