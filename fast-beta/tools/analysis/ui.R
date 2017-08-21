# untuk memindahkan panel kembali ke data setiap berpindah menu
observe({
  if(!identical(input$nav_fast, "Eclat")){
    updateTabsetPanel(session, "eclattab", selected = "Data")
  }})
  
  #Mengambil data
  getData <- reactive({
    getdata()
  })
  
  #Mengambil nama variabel
  vars <- reactive({
    if (!is.null(getData())){
      var <- names(getData())
      }
    else {
      var <- NULL
      }
    return(var)
  })
  
  #Mengambil list nama variabel
  variable <- reactive({
    if (!is.null(vars())){
      var <- as.list(vars()[])
    }
    else {
      var <- NULL
    }
    return(var)
  })
  
  #Mengambil nama variabel jika ada id
  id <- reactive({
    if (!is.null(vars())) {
      var <- as.list(vars()[which(!(vars() %in% input$chooseVar))])
    }
    else {
      var <- NULL
    }
    return(var)
  })
  
  #original data
  rawdata <- reactive({
    dfVardata <- as.matrix(getData()[,input$chooseVar])
    dfVarId <- as.matrix(getData()[,input$chooseId])
    df <- as.data.frame(cbind(dfVarId,dfVardata))
    return(df)
  })
  
  #konvert data
  dataTrans <- function(){
    tr_basket <- rawdata()
    if (!is.null(input$chooseId)){
      #if(ncol(rawdata())>2){
      #  df_itemList <- ddply(tr_basket,input$chooseId, function(df1)paste(tr_basket[,3], collapse = ","))
      #  } else {
      tr_basket <- split(tr_basket[,2],tr_basket[,1])
      
      #if (input$removeDataEclat == TRUE) { #seems unnecessary
      #  tr_basket2 <- sapply(1:length(tr_basket), function(x) unique(tr_basket[[x]])) #rm.duplicated
      #  # tr_basket<-as.data.frame(tr_basket)
      #  ap<-sapply(1:length(tr_basket2), function(x) if(length(tr_basket2[[x]])>1) paste(tr_basket2[[x]]))
      #  names(ap)<-names(tr_basket)
      #  tr_basket<-ap[!sapply(ap,is.null)]
      #}
      #  } 
    }
    hasil <- as(tr_basket,"transactions")
    return(hasil)
  }
  
  #transaksi data
  outDataTrans <- reactive({
    odt <- as(dataTrans(), "data.frame")
    dtfram <- as.data.frame(cbind(odt[2],odt[1]))
    colnames(dtfram) <- c("transaction ID","itemsets")
    return(dtfram)
  })
  
  #transaksi vertikal
  transver <- reactive({
    tl <- as(dataTrans(), "tidLists")
    return(tl)
  })
  
  #chooseFreq <- function() {
  # frequency <- itemFrequency(dataTrans())
  #frequency <- as.data.frame(frequency)
  #  item<-rownames(frequency)
  #  datfram <- as.data.frame(cbind(item,frequency))
  #  minsup <- minSupEclat()
  #  minsupCount <- minsup 
  #  datfram <- datfram[datfram$frequency >= minsupCount,] #prune!
  #  return(datfram)
  #}
  
  #tableItemset <- function(itemset) {
  #  d <- inspect(dataTrans())[1]  #item pada transaksi
  #  f <- sapply(1:nrow(d), function(x) gsub("\\{|\\}","",as.character(d[x,1])))
  #  item2 <- chooseFreq()[,1]
  #  b <- combn(item2,itemset)
  #  combination <- sapply(1:ncol(b),function(i)  paste(b[,i], collapse = ","))
  #  names(data) <- NULL
  #  dfs <- lapply(list(f, combination), function(x)  melt(strsplit(x, ",")))
  #  m <- merge(dfs[[2]], dfs[[1]], by = 1)
  #  f <- function(n) sum(aggregate(value ~ L1.y, m[m$L1.x == n,], function(x) length(unique(x)) == itemset)$value)
  #  frequency <- sapply(1:length(combination), f)
  #  frequency <- as.data.frame(as.numeric(frequency/nrow(dataTrans())))
  #  datfram <- as.data.frame(cbind(combination, frequency))
  #  colnames(datfram) <- c("combination","frequency")
  #  minsupCount <- minSupEclat() 
  #  datfram <- datfram[datfram$frequency >= minsupCount,]
  #  return(datfram)
  #}
  
  #fungsi frequent itemsets
  frequent <- function (){
    f <- ecl()
    #tl <- tidLists(f)
    #tl2 <- inspect(tl)
    #tl2 <- inspect(f)
    return(f)
  }
  
  #fungsi minimum support
  minSupEclat <- function() {
    minSup <- input$minSupEclat
    if (minSup < 0.001) {
      minSup <- 0.1
      updateNumericInput(session,"minSupEclat", value = 0.1)
    }
    return(minSup)
  }
  
  #fungsi minimum confidence
  minConfEclat <- function() {
    minConf <- input$minConfEclat
    if (minConf < 0.0001) {
      minConf <- 0.1
      updateNumericInput(session,"minConfEclat", value = 0.01)
    }
    return(minConf)
  }
  
  #fungsi minimum item
  minIEclat <- function() {
    minI <- input$minIEclat
    if (minI < 1) {
      minI <- 1
      updateNumericInput(session,"minIEclat",value = 1)
    }
    return(minI)
  }
  
  #fungsi maksimum item
  maxIEclat <- function() {
    maxI <- input$maxIEclat
    if (maxI < minIEclat()) {
      maxI <- minIEclat()+1
      updateNumericInput(session,"maxIEclat",value = minIEclat()+1)
    }
    return(maxI)
  }
  
  #fungsi eclat
  ecl <- reactive({
    itemsets <-eclat(dataTrans(), 
                     parameter = list(
                       supp = minSupEclat(), 
                       minlen = minIEclat(), 
                       maxlen = maxIEclat()
                     )
    )
    return(itemsets)
  })
  
  #fungsi rules
  fungsi <- function() {
    #if (length(ecl())>0){
    rules<- ruleInduction(ecl(),
                          dataTrans(),
                          confidence = minConfEclat()
    )
    if(input$removeRedundant==TRUE){
      rules.pruned <- rules[!is.redundant(rules)]
      rules<-rules.pruned
      #} else {
      #  rules < NULL
      #}
    }
    return(rules)
  }
  
  #fungsi rules dgn interest 
  dataRules <- function() {
    if (length(ecl())>0) {
      data<- fungsi()
      if (!is.null(input$intmeasure)) {
        quality(data) <- cbind(
          quality(data), 
          otherin()
          #interestMeasure(data,c(input$intmeasure), dataTrans()), 
          #RPF=RPF()
        )}
      data2 <- inspect(data)
      data2 <- data2[,-7]
    } else {
      data2 <- data.frame()
    }
    return(data2)
  }
  
  #RPF
  RPF <- function(){
    rp <- interestMeasure(
      fungsi(), "support", dataTrans()
    ) * interestMeasure(
      fungsi(), "confidence", dataTrans()
    )
    return(rp)
  }
  
  #other interest
  otherin <- function(){
    rpf      <- "RPF"       %in% input$intmeasure
    chi      <- "chiSquared"       %in% input$intmeasure
    kul      <- "kulczynski"       %in% input$intmeasure
    imb      <- "imbalance"       %in% input$intmeasure
    
    interest <- NULL
    if(rpf){
      interest <- cbind(interest, RPF=RPF())
    }
    if(chi) {
      interest <- cbind(interest, chisquare = interestMeasure(fungsi(),"chiSquared", dataTrans()))
    }
    if(kul) {
      interest <- cbind(interest, kulczynski=interestMeasure(fungsi(),"kulczynski", dataTrans()))
    }
    if(imb) {
      interest <- cbind(interest, imbalance=interestMeasure(fungsi(),"imbalance", dataTrans()))
    }
    return(interest)
  }
  
  #input jumlah top freq
  numberFreqEclat <- function() {
    na <- input$numberFreqEclat
    if (na > nrow(transver())) {
      na <- nrow(transver())
      updateNumericInput(session,"numberFreqEclat", value = na)
    }
    return(na)
  }
  
  #input jumlah rules
  numberRuleEclat <- function() {
    nr <- input$numberRuleEclat
    if (nr > length(fungsi())) {
      nr <- length(fungsi())
      updateNumericInput(session,"numberRuleEclat", value = length(fungsi()))
    }
    return(nr)
  }
  
  #sampling rule
  sampel <- function(){
    sam <- sample(fungsi(),numberRuleEclat())
    return(sam)
  }
  
  #frequency plot
  fplot<- function(){
    plot <- itemFrequencyPlot(dataTrans(), 
                              topN = numberFreqEclat(), 
                              xlab = "items" ,
                              ylab = "support", 
                              main = paste("Frequency Plot for",  numberFreqEclat(), "Items", sep=" "))
    return(plot)
  }
  
  #scatter plot
  splot<- function(){
    plot <- plot(sampel())
    return(plot)
  }
  
  #grouped plot
  goplot<- function(){
    plot <- plot(sampel(), method = "grouped")
    return(plot)
  }
  
  #graph plot
  gaplot<- function(){
    plot <- plot(sampel(), method = "graph")
    return(plot)
  }
  
  #paracoord plot
  #pplot<- function(){
  #  plot <- plot(sampel(), method = "paracoord", control=list(reorder=TRUE))
  #  return(plot)
  #}
  
  #download report
  output$downloadReportEclat <- downloadHandler(
    filename = function() {
      paste('Association Rules ECLAT Algorithm Report', sep = '.', 
            switch(
              input$formatEclat, PDF = 'pdf', HTML = 'html', Word = 'docx'
            )
      )
    },
    content = function(file) {
      src <- normalizePath('eclat-report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'eclat-report.Rmd')
      
      library(rmarkdown)
      out <- render('eclat-report.Rmd', switch(
        input$formatEclat,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
        )
      )
      file.rename(out, file)
    }
  )
  
  #tampilan Utama
  output$eclat <- renderUI({
    sidebarLayout(
      sidebarPanel(
        div(class="busy", 
            p("Calculation in progress..."), 
            img(src="ajaxloaderq.gif")
        ),
        uiOutput(outputId = 'nav_menu'),
        bsCollapse(
          open = "Data Properties",
          bsCollapsePanel("Data Properties",
                          uiOutput(outputId = 'chooseVarEclat'),
                          conditionalPanel(condition = "input.chooseVar.length <2",
                                           checkboxInput(
                                             "singleToBasket","Aggregate item", FALSE),
                                           conditionalPanel(condition = "input.singleToBasket==true",
                                                            uiOutput("chooseId")
                                           )
                                           #checkboxInput("removeDataEclat","Remove duplicated item", FALSE)
                          )
          ),
          
          bsCollapsePanel("Parameter Properties",
                          numericInput(
                            "minSupEclat","Minimum support :", min = 0.0001, max = 1, value =0.1, step = 0.0001
                          ),
                          numericInput(
                            "minConfEclat","Minimum confidence :", min = 0.0001, max = 1.0001, value = 0.1,step =0.0001
                          ),
                          numericInput(
                            "minIEclat","Minimum item(s):", min = 1,value = 1, step = 1
                          ),
                          numericInput(
                            "maxIEclat","Maximum items:", min = 2,value = 10, step = 1
                          ),
                          checkboxInput(
                            "removeRedundant", "Remove redundant", FALSE
                          ),
                          #actionButton("getRules","Get Results")
                          checkboxInput(
                            "intMea","Add Interest Measure", FALSE
                          ),
                          conditionalPanel(condition = "input.intMea==true",
                                           checkboxGroupInput(
                                             "intmeasure", "Choose interest measure(s):",
                                             c(
                                               "chi-squared" = "chiSquared",
                                               "kulczynski" = "kulczynski",
                                               "imbalance" = "imbalance",
                                               "RPF" = "RPF"
                                             )
                                           )
                          )
          ),
          
          bsCollapsePanel("Plot Properties",
                          uiOutput("sidePlotFreq"),
                          uiOutput("sidePlotRules")
          ),
          
          bsCollapsePanel("Report",
                          strong("Generate Your Report"),
                          wellPanel(radioButtons("formatEclat", "Document format:", c("Word","PDF" ,"HTML")),
                                    downloadButton("downloadReportEclat")),br(),
                          strong("Share Your Report"),
                          bsButton("ShareForumEclat", "Share to Forum", style = "primary", type = "action"),br(),br(),
                          bsModal2("popShareForumEclat", "Share Your Analysis", trigger = "ShareForumEclat",
                                   uiOutput("modalShareEclat")
                          )
          )
        )
      ),
      
      mainPanel(
        div(class="busy", 
            p("Calculation in progress..."), 
            img(src="ajaxloaderq.gif")
        ),
        tabsetPanel(id = 'eclattab',
                    tabPanel(
                      title = "Data",
                      tabsetPanel(id = 'DataEclat',
                                  tabPanel(
                                    title = "Original Data",
                                    uiOutput('preprop')
                                  ),
                                  tabPanel(
                                    title = "Transaction Data",
                                    uiOutput('transaksiEclat')
                                  ),
                                  tabPanel(
                                    title = "Vertical Transaction Data",
                                    uiOutput('transaksiVertikal')
                                  )
                      )
                    ),
                    
                    tabPanel(
                      title = "Result",
                      tabsetPanel(
                        id = 'SummaryEclat',
                        tabPanel(
                          title = "Frequent Itemsets",
                          uiOutput('iFreq')
                          #wellPanel(
                          #  style = "overflow-y:scroll; max-width: 960px; max-height : 600px",
                          #  dataTableOutput("iFreq")
                          #  )
                        ),
                        tabPanel(
                          title = "Rules",
                          uiOutput('rulesEclat')
                          #wellPanel(
                          #  style = "overflow-y:scroll; max-width: 960px; max-height : 600px",
                          #  dataTableOutput("rulesEclat")
                          #  )
                        )
                      )
                      #uiOutput('tampil_summary')
                    ),
                    
                    tabPanel(
                      title = "Plot",
                      tabsetPanel(id = 'PlotEclat',
                                  tabPanel(
                                    title = "Frequency Plot",
                                    plotOutput("FreqPlot")
                                  ),
                                  tabPanel(
                                    title = "Scatter Plot",
                                    plotOutput("ScatterPlot")
                                  ),
                                  tabPanel(
                                    title = "Grouped Plot",
                                    plotOutput("GroupedPlot")
                                  ),
                                  tabPanel(
                                    title = "Graph Plot",
                                    plotOutput("GraphPlot")
                                  )
                                  #tabPanel(
                                  #  title = "Paracoord Plot",
                                  #  plotOutput("ParacoordPlot")
                                  #  )
                      )
                    )
        )
      )
    )
  })
  
  #menunjukkan status modul
  output$nav_menu <- renderUI({
    wellPanel(
      HTML(paste("<label><strong>Menu:","Association Rule","</strong></label>")),
      HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label><br>")),
      HTML(paste("<label><strong>Data:",input$datasets,"</strong></label><br>")),
      helpAndReport2(
        'Eclat','eclatHelp', inclMD2("tools/help/eclat.md")
      )
    ) 
  })
  
  #UI button select
  #output$selectDeselect <- renderUI({
  #  vars<- upload()
  #if (!is.null(variable())) {
  #    actionButton("selectDeselect", "Select/Deselect all", class = "btn-block btn-primary")
  #}
  #})
  
  #tampilan pilih variabel
  output$chooseVarEclat <- renderUI({
    #vars<- upload()
    #if (!is.null(input$selectDeselect)) {
    #  if (input$selectDeselect %% 2 == 0) {
    #    selectInput(
    #      "chooseVar","Select one or more variables:",names(vars),names(vars),multiple =T
    #    )
    #  } else {
    selectInput(
      "chooseVar","Select variable(s):",variable(),selected = variable(),multiple =T, selectize= F
    )
    #  }
    #}
  })
  
  #Tampilan pilih id agregate
  output$chooseId <- renderUI({
    selectInput(
      "chooseId","Aggregate by:",id(),selected = NULL,multiple= T, selectize= T)
  })
  
  #tampilan input parameter plot frequency
  output$sidePlotFreq<-renderUI({
    numericInput(
      "numberFreqEclat", "Number of top item :", nrow(transver()), min=1, step=1)
  })
  
  #tampilan input parameter plot rules
  output$sidePlotRules<-renderUI({
    numericInput(
      "numberRuleEclat","Number of rules :", nrow(dataRules()) ,min = 1, step = 1)
  })
  
  #tampilan tab original data
  output$preprop <- renderUI({
    list(
      wellPanel(
        style = "overflow-y:scroll; max-width: 960px; max-height : 600px",
        renderDataTable({
          shiny::validate(
            need(input$chooseVar !="", "Please Select one or more Variable(s) of Data from Data Properties Sidebar")
          )
          rawdata()
        },
        options = list(
          orderClasses = TRUE, bCaseInsensitive = TRUE,
          lengthMenu = c(100, 200, 500), pageLength = 100
        )
        )
      )
    )
  })
  
  #tampilan tab transaction data
  output$transaksiEclat <- renderUI({
    list(
      wellPanel(
        style = "overflow-y:scroll; max-width: 960px; max-height : 600px",
        renderDataTable({
          shiny::validate(
            need(input$chooseVar !="", "Please Select one or more Variable(s) of Data from Data Properties Sidebar")
          )
          outDataTrans()
        },
        options = list(
          orderClasses = TRUE, bCaseInsensitive = TRUE,
          lengthMenu = c(100, 200, 500), pageLength = 100
        )
        )
      )
    )
  })
  
  #tampilan tab transaksi Vertikal
  output$transaksiVertikal <- renderUI({
    list(
      wellPanel(
        style = "overflow-y:scroll; max-width: 960px; max-height : 600px",
        renderDataTable({
          shiny::validate(
            need(input$chooseVar !="", "Please Select one or more Variable(s) of Data from Data Properties Sidebar")
          )
          inspect(transver())
        },
        options = list(
          orderClasses = TRUE, bCaseInsensitive = TRUE,
          lengthMenu = c(50,100, 200, 500), pageLength = 100
        )
        )
      )
    )
  })
  
  #tampilan tab frequent itemset
  output$iFreq <- renderUI({
    list(
      wellPanel(
        style = "overflow-y:scroll; max-width: 960px; max-height : 600px",
        renderDataTable({
          shiny::validate(
            need(input$chooseVar !="", "Please Select one or more Variable(s) of Data from Data Properties Sidebar"),
            need(length(frequent()) >0, "Zero frequent items generated. Please adjust minimum support from Parameter Properties Sidebar"))
          inspect(frequent())
        }, 
        options = list(
          orderClasses = TRUE, bCaseInsensitive = TRUE,
          lengthMenu = c(10, 20, 30), pageLength = 10
        )
        )
      )
    )
  })
  
  #tampilan tab rules
  output$rulesEclat <- renderUI({
    list(
      wellPanel(
        style = "overflow-y:scroll; max-width: 960px; max-height : 600px",
        renderDataTable({
          shiny::validate(
            need(input$chooseVar !="", "Please Select one or more Variable(s) of Data from Data Properties Sidebar"),
            #need(length(ecl())>0, "Zero rules generated. Please adjust minimum support and/or minimum confidence from Parameter Properties Sidebar"),
            need(length(dataRules())>0 , "Zero rules generated. Please adjust minimum support and/or minimum confidence from Parameter Properties Sidebar"))
          dataRules()
        },  
        options = list(
          orderClasses = TRUE, bCaseInsensitive = TRUE,
          lengthMenu = c(5, 10, 15), pageLength = 5
        )
        )
      )
    )
  })
  
  #tampilan tab scatter plot
  output$ScatterPlot <- renderPlot({                
    shiny::validate(
      need(input$numberRuleEclat >0, "Please input the number of rules from Plot Properties Sidebar")
      )
    #need(, "Please increase the number of rules from Plot Properties Sidebar"))
    splot()
  })
  
  #tampilan tab grouped plot
  output$GroupedPlot <- renderPlot({
    shiny::validate(
      need(input$numberRuleEclat >0, "Please input the number of rules from Plot Properties Sidebar")
      )
    #need(sampel()=0, "Please increase the number of rules from Plot Properties Sidebar"))
    goplot()
  })
  
  #tampilan tab graph plot
  output$GraphPlot <- renderPlot({
    shiny::validate(
      need(input$numberRuleEclat >0, "Please input the number of rules from Plot Properties Sidebar"))
    #need(sampel()=0, "Please increase the number of rules from Plot Properties Sidebar"))
    gaplot()
  })
  
  #tampilan tab paracoord plot
  #output$ParacoordPlot <- renderPlot({
  #  shiny::validate(need(input$numberRuleEclat >0, "Please input the number of rules from Plot Properties Sidebar"))
    # need(sampel()=0, "Please increase the number of rules from Plot Properties Sidebar"))
  #  pplot()
  #})
  
  #tampilan tab frequency plot
  output$FreqPlot <- renderPlot({
    shiny::validate(
      need(input$numberFreqEclat >0, "Please input the number of rules from Plot Properties Sidebar"))
    #need(sampel()=0, "Please increase the number of rules from Plot Properties Sidebar"))
    fplot()
  })
  
  