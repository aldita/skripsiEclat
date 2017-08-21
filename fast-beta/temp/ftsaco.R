#fungsi untuk berpindah ke tab awal ketika user berpindah fungsi
observe({
  if(!identical(input$nav_fast, "FTS-ACO")){
    print(input$nav_fast)
    updateTabsetPanel(session,"FTSACOMainTab", selected = "FTSACOfirstTab")
  }
})

#reset ketika user sudah keluar dari menu ftaco
observe({
  if(identical(input$nav_fast, "Data")){
    print("Masuk sini")
    valuesFTSACO$FTSACOlearnBtn <- 0
    valuesFTSACO$FTSACOForecastBtn <- 0
    valuesFTSACO$FTSACOSetSeedStatus <- 0
    valuesFTSACO$FTSACOSetSeedValue <- 123456
    valuesFTSACO$datatesting <- NULL
  }
})

#menunjukkan status variabel
output$statusUIFTSACO <- renderUI({
  wellPanel(
    HTML(paste("<label><strong>Menu:", "FORECAST","</strong></label></br>")),
    HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label></br>")),
    HTML(paste("<label><strong>Data:",paste(toupper(input$datasets)),"</strong></label>")),
    style = "background : white; border-style: solid; border-color: #266EAC; border-width: medium;"
  )
})

#mengeset posisi tab tampilan samping kembali kesemula
observe({
  if(identical(input$nav_fast, "FTS-ACO")){
    
    #desain tampilan panel samping
    output$parameterFTSACO <- renderUI({
      list(
        bsCollapse(multiple = FALSE, open = "colFACO1", id = "collapseFACO", 
                   bsCollapsePanel("Forecast Properties",
                                   radioButtons('algoritmaFTS_FTSACO', 'Forecasting Method : ', c('FTS', 'FTS - ACO'), selected = "FTS"),
                                   uiOutput("pilihVarFTSACO"),
                                   hr(),
                                   conditionalPanel(
                                     condition = "input.algoritmaFTS_FTSACO == 'FTS - ACO'",
                                     numericInput("varFTSACO_maxIter", "Max Iterations, min is 1 :", 3, min = 1, step = 1),
                                     numericInput("varFTSACO_maxMSE", "Max MSE, min is 0 :", 2, min = 0, step = .5),
                                     numericInput("varFTSACO_ant", "Number Of Ant, min is 10 :", 20, min = 10, step = 1),
                                     sliderInput("varFTSACO_step", label = "Ant Step Width :", min = 1, max = 9, value = 7, width = "100%", step = 1),
                                     selectInput(inputId = "varFTSACOBasedOn", label = "Selecting Model Based On : ", c("MSE", "BIC"), 
                                                 selected = "MSE", multiple = FALSE)
                                   ),
                                   value="colFACO1"),
                   bsCollapsePanel("Generate Your Report", 
                                   radioButtons('formatFTSACO', 'Document format', c('Word', 'PDF', 'HTML')),
                                   downloadButton('downloadReportFTSACO'),
                                   value="colFACO2")
        ),
        helpText(strong("Share your report: ")),
        bsButton("FtsAcoShareForum", "SHARE to Forum", style = "primary", type = "action"),br(),br(),
        bsModal2("popFtsAcoShareForum", "Share Your Analysis", trigger = "FtsAcoShareForum",
                 uiOutput("modalShareFtsAco")
                 ),
        helpAndReport2('FTS-ACO','ftsaco', inclMD2("tools/help/ftsaco-help.md"))
      )
    })
  }
  
  #tampilan pilih variabel
  output$pilihVarFTSACO <- renderUI({
    isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
    vars <- varnames()[isNum]
    if(length(vars) == 0) return()
    selectInput(inputId = "FTSACOVarSelected", label = "Select one variables to forecast", choices = vars, 
                selected = vars[1], multiple = FALSE)
  })
})

################################### tampilan utama fts aco #########################################
# ==================================================================================================
output$ftsaco <- renderUI({
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$link(rel = "stylesheet", type="text/css", href="loading123.css")),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div(class="loading"), 
                       div(
                         class = "busy",
                         p("Calculation in progress ..."),
                         img(src="ajaxloaderq.gif"))
      ),
      uiOutput("statusUIFTSACO"),
      wellPanel(uiOutput("parameterFTSACO"),
                style = "background : white; border-style: solid; border-color: #266EAC; border-width: medium;")
    ),
    mainPanel(
      tabsetPanel(
        id = "FTSACOMainTab",
        tabPanel(
          title = "Data",
          tabsetPanel(
            id = "subMenuData",
            tabPanel("Select Data",
                     uiOutput(outputId = "ftsacoSelectData"),
                     value = "ftsacoselectdata",
                     uiOutput(outputId = "ftsacoSelectDataTT"),
                     withTags(
                       bsCollapse(multiple = TRUE, open = c("ftsacoplotSD1"), id = "ftsacodataCollapseSD",
                                  bsCollapsePanel("Series Plot", 
                                                  dygraphOutput(outputId = "ftsacoplotSD1", height = "450px"),
                                                  id = "ftsacoplotSD1", value = "ftsacoplotSD1"),
                                  bsCollapsePanel("Difference Series Plot", 
                                                  dygraphOutput(outputId = "ftsacoplotDiffPercent", height = "450px"),
                                                  id = "ftsacoplotIden1", value = "ftsacoplotIden1")
                       )
                     )
            )
          ),
          value = "FTSACOfirstTab"
        ),
        tabPanel(
          title = "Forecast",
          tabsetPanel(
            id = "subMenuForc",
            tabPanel("Create Model",
                     uiOutput(outputId = "ftsacoCreateModel"),
                     value = "ftsacoCreateModel"),
            tabPanel("Forecasting",
                     uiOutput(outputId = "ftsacoForecasting"),
                     value = "ftsacoForecast")
          )
        ),
        
        tabPanel("Summary",
                 uiOutput(outputId = "ftsacoSummary"),
                 value = "ftsacoSumm")
      )
    )
  )
})

#membuat plot dari data yang terpilih
output$ftsacoplotSD1 <- renderDygraph({
  if(is.null(FTSACOgetFirstNumData()) || is.null(FTSACOgetLastNumData()) || is.null(FTSACOgetVar()[FTSACOgetFirstNumData():FTSACOgetLastNumData()])){
    return()
  }else{
    if(length(FTSACOgetSecondNumIndex) == 0){
      batasTT <- FTSACOjumlah3per4Data()
    }else{
      batasTT <- FTSACOgetSecondNumIndex() - 0.5
    }
    no <- seq.default(0, (length(FTSACOgetVar()[FTSACOgetFirstNumData():FTSACOgetLastNumData()]) + 1), 1)
    data <- data.frame(no, c(NA, FTSACOgetVar()[FTSACOgetFirstNumData():FTSACOgetLastNumData()], NA))
    colnames(data) <- c("no","series")
    VarPlot <- dygraph(data, main = "Series Plot") %>%
      dyLegend(width = 480) %>%
      dyAxis("x", label = "Obsevation") %>%
      dyAxis("y", label = "Value") %>%
      dySeries(strokeWidth = 2.5 ) %>%
      dyOptions(drawGrid = FALSE, colors = "blue") %>%
      dyRangeSelector(height = 30) %>%
      dyShading(from=paste(batasTT), to=paste(FTSACOgetJumlahDataTerpilih()), color="#CDCDC1")
    return(VarPlot)
  }
})

#membuat plot percentage change dari data terpilih
output$ftsacoplotDiffPercent <- renderDygraph({
  if(length(FTSACOgetSecondNumIndex) == 0){
    batasTT <- FTSACOjumlah3per4Data()
  }else{
    batasTT <- FTSACOgetSecondNumIndex() - 0.5
  }
  no <- seq(0,length(FTSACOPercentageChange(FTSACOgetTheData()[,1])[FTSACOgetFirstNumData():(FTSACOgetLastNumData()-1)]) + 1, 1)
  data <- data.frame(no, c(NA, FTSACOPercentageChange(FTSACOgetTheData()[,1])[FTSACOgetFirstNumData():(FTSACOgetLastNumData()-1)], NA))
  colnames(data) <- c("no","change")
  varPlot <- dygraph(data, main = "Percentage Change (%)") %>%
    dyLegend(width = 500) %>%
    dyAxis("x", label = "Obsevation") %>%
    dyAxis("y", label = "Value") %>%
    dySeries(strokeWidth = 2.5 ) %>%
    dyLimit(0, color = "red") %>%
    dyOptions(drawGrid = FALSE, colors = "blue") %>%
    dyRangeSelector(height = 30) %>%
    dyShading(from=paste(batasTT), to=paste(FTSACOgetJumlahDataTerpilih()), color="#CDCDC1")
  
  return(varPlot)
})

#membuat box plot data terpilih (training&testing)
output$ftsacoboxplot.train_test<- renderPlot({
  if(FTSACOTrainingLength() == 0 && FTSACOTestingLength() == 0){
    varplot <- NULL
  }else if(FTSACOTrainingLength() == 0){
    varplot <- 
      par(mfrow = c(1,2))
    plot(FTSACOgetDataTesting(), xlab="time observation", ylab=input$FTSACOVarSelected, 
         main="Plot Of Testing Data", type="l")
    boxplot(FTSACOgetDataTesting(), 
            xlab = "value", ylab = "time observation", main = "Box Plot Of Testing Data", horizontal = TRUE)
  }else if(FTSACOTestingLength() == 0){
    varplot <- 
      par(mfrow = c(1,2))
    plot(FTSACOgetDataTraining(), xlab="time observation", ylab=input$FTSACOVarSelected, 
         main="Plot Of Training Data", type="l")
    boxplot(FTSACOgetDataTraining(), 
            xlab = "value", ylab = "time observation", main = "Box Plot Of Training Data", horizontal = TRUE)
  }else{
    varplot <- 
      par(mfrow = c(2,2))
    data <- FTSACOPercentageChange(FTSACOgetVar()[FTSACOgetFirstNumData():FTSACOgetLastNumData()])
    datatrain <- data[(FTSACOgetFirstNumData()-1):(FTSACOgetFirstNumData()+FTSACOgetSecondNumIndex()-2)]
    datatest <- data[(FTSACOgetFirstNumData()+FTSACOgetSecondNumIndex()-1):(FTSACOgetLastNumData()-1)]
    
    plot(datatrain, xlab="time observation", ylab=input$FTSACOVarSelected, 
         main="Percentage Change Of Training Data", type="l")
    plot(datatest, xlab="time observation", ylab=input$FTSACOVarSelected, 
         main="Percentage Change Of Testing Data", type="l")
    boxplot(datatrain, 
            xlab = "value", ylab = "time observation", main = "Box Plot Of Training Data", horizontal = TRUE)
    boxplot(datatest, 
            xlab = "value", ylab = "time observation", main = "Box Plot Of Testing Data", horizontal = TRUE)
  }
  return(varplot)
})

#membuat plot hasil memodelkan
output$FTSACOPlotTraining <- renderDygraph({
  if(valuesFTSACO$FTSACOlearnBtn == 0 || is.null(FTSACOgetDataPred()[[12]]) || FTSACOTrainingLength() == 0){
    return();
  }else{
    no <- seq(0,(length(FTSACOgetDataPred()[[12]])), 1)
    data1 <- c(NA, FTSACOgetDataTraining()[-1], NA)
    data2 <- c(NA, FTSACOgetDataPred()[[12]][-1], NA)
    data <- data.frame(no, data1, data2)
    colnames(data) <- c("no","True Value", "Forecast")
    varPlot <- dygraph(data, main = "Modeling Plot") %>%
      dyLegend(width = 550) %>%
      dyAxis("x", label = "Obsevation") %>%
      dyAxis("y", label = "Value") %>%
      dySeries(strokeWidth = 2, strokePattern = "dashed") %>%
      dySeries(strokeWidth = 2 ) %>%
      dyOptions(colors = c("blue", "red"), drawGrid = FALSE) %>%
      dyRangeSelector(height = 30)
    return(varPlot);
  }
})

#membuat plot data peramalan
output$FTSACOPlotFitting <- renderDygraph({
  if(valuesFTSACO$FTSACOlearnBtn == 0 || is.null(FTSACOgetDataFitting()[[4]][-(1:2)])){
    return();
  }else{
    no <- seq(0, (length(FTSACOgetDataFitting()[[4]]) + 1), 1)
    data1 <- c(NA, FTSACOgetDataFitting()[[4]], NA)
    data2 <- c(NA, FTSACOgetDataFitting()[[5]], NA)
    data <- data.frame(no, data1, data2)
    colnames(data) <- c("no","True value","Forecast")
    varPlot <- dygraph(data, main = "Fitting Plot") %>%
      dyLegend(width = 550) %>%
      dyAxis("x", label = "Obsevation") %>%
      dyAxis("y", label = "Value") %>%
      dySeries(strokeWidth = 2, strokePattern = "dashed") %>%
      dySeries(strokeWidth = 2) %>%
      dyOptions(colors = c("blue", "red"), drawGrid = FALSE) %>%
      dyRangeSelector(height = 30) 
    return(varPlot);
  }
})

#membuat plot data peramalan
output$FTSACOPlotTesting <- renderDygraph({
  if(valuesFTSACO$FTSACOlearnBtn == 0 || FTSACOTestingLength() == 0 || is.null(FTSACOgetDataPred()[[2]][-(1:2)])){
    return();
  }else{
    no <- seq(0, (length(FTSACOgetDataPred()[[2]][-(1:2)]) +  FTSACOTrainingLength() + 1), 1)
    datatrue1 <- c(rep(NA, 1), FTSACOgetDataTraining(), rep(NA, length(FTSACOgetDataPred()[[2]][-1])))
    datatrue2 <- c(rep(NA, FTSACOTrainingLength()), FTSACOgetDataPred()[[2]][-1], NA)
    perantarapred <- c(rep(NA, FTSACOTrainingLength()), FTSACOgetDataPred()[[3]][2:3], rep(NA, (length(FTSACOgetDataPred()[[2]][-(1:3)]) + 1)))
    datapred <- c(rep(NA, FTSACOTrainingLength()+1), FTSACOgetDataPred()[[3]][-(1:2)], NA)
    data <- data.frame(no, datatrue1, datatrue2, perantarapred, datapred)
    colnames(data) <- c("no","Data Training", "True Value", "_", "Forecast")
    varPlot <- dygraph(data, main = "Predicting Plot") %>%
      dyLegend(width = 550) %>%
      dyAxis("x", label = "Obsevation") %>%
      dyAxis("y", label = "Value") %>%
      dySeries(strokeWidth = 2) %>%
      dySeries(strokeWidth = 2, strokePattern = "dashed") %>%
      dySeries(strokeWidth = 2, strokePattern = "dashed") %>%
      dySeries(strokeWidth = 2) %>%
      dyOptions(colors = c("blue", "blue", "red", "red"), drawGrid = FALSE) %>%
      dyRangeSelector(height = 30)
    return(varPlot);
  }
})

#membuat plot data hasil peramalan
output$FTSACOPlotForecast <- renderDygraph({
  if(valuesFTSACO$FTSACOlearnBtn == 0 || valuesFTSACO$FTSACOForecastBtn == 0){
    return();
  }else{
    #get mse model
    indexmin <- FTSACOgetindexminmse(FTSACOgetDataPred()[[5]])
    rmse <- sqrt(FTSACOgetDataPred()[[5]][indexmin])
    n <- length(valuesFTSACO$dataForecast[[1]][-(1:2)])
    se <- rmse / sqrt(n)
    e <- qt(.975, df = n-1) * se
    no <- seq(0, (length(valuesFTSACO$dataForecast[[1]][-(1:2)]) + FTSACOgetJumlahDataTerpilih() + 1), 1)
    dataprediksi <- as.matrix(valuesFTSACO$dataForecast[[1]][-(1:2)], ncol=1)
    data0 <- c(rep(NA, FTSACOgetJumlahDataTerpilih()+1), apply(dataprediksi, 1, function(x) (x-e)), NA)
    data1 <- c(rep(NA, FTSACOgetJumlahDataTerpilih()+1), dataprediksi, NA)
    data2 <- c(rep(NA, FTSACOgetJumlahDataTerpilih()+1), apply(dataprediksi, 1, function(x) (x+e)), NA)
    datatrue <- c(NA, FTSACOgetVar()[FTSACOgetFirstNumData():FTSACOgetLastNumData()], rep(NA, (length(dataprediksi) + 1)))
    dataantara <-  c(rep(NA, FTSACOgetJumlahDataTerpilih()), FTSACOgetVar()[FTSACOgetLastNumData()], dataprediksi[1], rep(NA, length(dataprediksi)))
    #data <- data.frame(no, data0, data1, data2, datatrue, dataantara)
    data <- data.frame(no, data1, datatrue, dataantara)
    #colnames(data) <- c("no","data0", "Forecasting Value", "data2", "Data True", "_")
    colnames(data) <- c("no","Forecasting Value", "Data True", "_")
    varPlot <- dygraph(data, main = "Forecasting Plot") %>%
      dyLegend(width = 550) %>%
      dyAxis("x", label = "Observation") %>%
      dyAxis("y", label = "Value") %>%
      dySeries("Data True", strokeWidth = 2) %>%
      dySeries("_", strokeWidth = 2, strokePattern = "dashed") %>%
      #dySeries(c("data0", "Forecasting Value", "data2"), strokeWidth = 2) %>%
      dySeries("Forecasting Value", strokeWidth = 2) %>%
      dyOptions(colors = c("blue", "grey", "grey"), drawGrid = FALSE) %>%
      dyRangeSelector(height = 30)
    return(varPlot);
  }
})

#meNULLpilkan panel error
output$FTSACOPanelError <- renderUI({
  if(valuesFTSACO$FTSACOlearnBtn == 0 && FTSACOTestingLength() == 0){
    return()
  }else{
    panel <- list(
      helpText(strong("Selected Model in Iteration : "), FTSACOgetindexminmse(FTSACOdataerror()[[2]])),
      dataTableOutput(outputId = "ftsacoIterationError"),
      br(),
      br()
    )
    return(panel)
  }
})

#menampilkan tabel nilai error setiap iterasi
output$ftsacoIterationError <- renderDataTable({
  if(valuesFTSACO$FTSACOlearnBtn == 0){
    return()
  }
  data <- FTSACOdataerror()
  indexmin <- FTSACOgetindexminmse(data[[2]])
  Model <- rep(paste("  "), length(data[[2]]))
  if(valuesFTSACO$FTSACOlearnBtn == 1){
    Model[indexmin] <- "Selected"
  }
  tabel <- cbind(Model, data)
  tabel
} , options = list(searching = FALSE, 
                   pageLength = 10#,
#                   rowCallback = I(
#                     'function(row, data) {
#                     if (data[0] == "Selected")
#                     $("td", row).css("background", "orange");
#                     }'
#                   )
)
)

#menampilkan tabel nilai hasil ramalan
output$ftsacoSummaryResult <- renderTable({
  if(valuesFTSACO$FTSACOForecastBtn == 0){
    return()
  }else{
    nilai_prediksi <- data.frame(valuesFTSACO$dataForecast[[1]][-(1:2)])
    persen_prediksi <- data.frame(valuesFTSACO$dataForecast[[2]][-(1:2)])
    table <- cbind(nilai_prediksi, persen_prediksi)
    colnames(table) <- c("Forcasting Value", "Percentage Value")
    return(table)
  }
})

#menampilkan tabel nilai hasil ramalan
output$ftsacoSummaryErrormodel <- renderTable({
  if(valuesFTSACO$FTSACOForecastBtn == 0){
    return()
  }else{
    indexmin <- FTSACOgetindexminmse(FTSACOgetDataPred()[[5]])
    pred <- c(FTSACOgetDataPred()[[5]][indexmin], sqrt(FTSACOgetDataPred()[[5]][indexmin]), FTSACOgetDataPred()[[6]][indexmin], FTSACOgetDataPred()[[7]][indexmin])
    fitting <- c(FTSACOgetDataFitting()[[1]][[1]], sqrt(as.numeric(FTSACOgetDataFitting()[[1]][[1]])), FTSACOgetDataFitting()[[2]][[1]], FTSACOgetDataFitting()[[3]][[1]])
    tabel <- cbind(fitting, pred)
    colnames(tabel) <- c("Fitting Error", "Predicting Error")
    rownames(tabel) <- c("MSE", "RMSE", "MAPE(%)", "MAE")
    return(tabel)
  }
})

# mengambil data terpilih dalam bentuk matriks karakter
FTSACOgetVar <- reactive({
  input$FTSACOVarSelected
  if(is.null(input$FTSACOVarSelected)) return()
  else {
    valuesFTSACO$learnButton <- 0
    theData <- getdata()
    return(theData[,as.character(input$FTSACOVarSelected)])
  }
})

#tampilan dari tab select data (memisahkan slider agar tidak crash)
output$ftsacoSelectData <- renderUI({
  list(br(),
       strong(helpText("SELECT DATA")),
       helpText("By default, Data are used in this section is all"),
       helpText("Minimum data that should be used is 20 series"),
       sliderInput("ftsaconumber_data", label = "Select range data:", min = 1, max = as.numeric(FTSACOgetJumlahData()), value = c(1,as.numeric(FTSACOgetJumlahData())), width = "80%"),
       br(),br()
  )
})

#tampilan dari tab select training testing data (memisahkan slider agar tidak crash)
output$ftsacoSelectDataTT <- renderUI({
  list(
    helpText("By default, training data and testing data are set with ratio 3:1."),
    helpText("Training data should be more than 3 series"),
    sliderInput("ftsaconumber_datatraining", label = "Select range data:", min = 1, max = FTSACOgetJumlahDataTerpilih(), value = FTSACOjumlah3per4Data(), width = "80%", step = 1),
    br(),br()
  )
})

#tampilan dari tab forecast (create model)
output$ftsacoCreateModel <- renderUI({
  if(is.null(FTSACOgetVar()) || FTSACOgetJumlahDataTerpilih() < 20 || FTSACOTrainingLength() < 4) {
    return(list(
      br(),
      strong(helpText("CREATE MODEL")),
      br(),
      helpText(strong("Requirement Conditions :")),
      helpText(strong("1."), " Select one variable numeric or integer"),
      helpText(strong("2."), " Minimum data is 20 series"),
      helpText(strong("3."), " Training data should be more than 3 series"),
      helpText(strong("4."), " Parameter value must be positive")
    ))}
  list(br(),
       strong(helpText("CREATE MODEL")),
       helpText("Then you can tune its parameters in the sidebar."),
       helpText("After that, you can proceed to start learning using learn button below."),
       conditionalPanel(
         condition = "input.algoritmaFTS_FTSACO == 'FTS - ACO'",
         checkboxInput(inputId = "FTSACOSetSeed", label = "Set Seed", value = valuesFTSACO$FTSACOSetSeedStatus),
         textInput(inputId = "FTSACOSetSeedInput", label = NULL, value = valuesFTSACO$FTSACOSetSeedValue)
       ),
       actionButton(inputId = "FTSACOlearnBtn", label = "Start Learning", icon = icon("fa fa-spinner")),
       br(),
       uiOutput(outputId = "FTSACOStartLearning"),
       br(),
       tabsetPanel(
         id = "plot",
         tabPanel("Modeling Plot",
                  br(),
                  dygraphOutput(outputId = "FTSACOPlotTraining", height = "450px")),
         tabPanel("Fitting Plot",
                  br(),
                  dygraphOutput(outputId = "FTSACOPlotFitting", height = "450px"),
                  br(),
                  hr(),
                  br(),
                  fluidRow(
                    column(6,
                           helpText(strong("Mean Squared Error")),
                           wellPanel(class = "FA_error",
                                     helpText(strong(FTSACOgetDataFitting()[[1]][[1]])),
                                     style = "max-width : 350px; height: 40px; padding: 2px; text-align : center;"
                           )
                    ),
                    column(6,
                           helpText(strong("Mean Absolute Error")),
                           wellPanel(class = "FA_error",
                                     helpText(strong(FTSACOgetDataFitting()[[3]][[1]])),
                                     style = "max-width : 350px; height: 40px; padding: 2px; text-align : center;"
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           helpText(strong("Root Mean Squared Error")),
                           wellPanel(class = "FA_error",
                                     helpText(strong(sqrt(as.numeric(FTSACOgetDataFitting()[[1]][[1]])))),
                                     style = "max-width : 350px; height: 40px; padding: 2px; text-align : center;"
                           ),
                           style = "align-content: center;"
                    ),
                    column(6,
                           div(
                             helpText(strong("Mean Absolute Percentage Error")),
                             wellPanel(class = "FA_error",
                                       helpText(strong(FTSACOgetDataFitting()[[2]][[1]], paste("  %"))),
                                       style = "max-width : 350px; height: 40px; padding: 2px; text-align : center;"
                             )
                           )
                    )
                  ),
                  br(),
                  br(),
                  br()),
         tabPanel("Predicting Plot",
                  br(),
                  dygraphOutput(outputId = "FTSACOPlotTesting", height = "450px"),
                  br(),
                  br(),
                  checkboxInput(inputId = 'FTSACOErrorPanel', label = "Show Table Error", value = FALSE),
                  hr(),
                  br(),
                  conditionalPanel(
                    condition = "input.FTSACOErrorPanel",
                    uiOutput(outputId = "FTSACOPanelError")
                  ))
       )
  )
})

#tampilan dari tab forecast (Forecast)
output$ftsacoForecasting <- renderUI({
  if(is.null(valuesFTSACO$datatesting) || valuesFTSACO$FTSACOlearnBtn == 0) {
    return(list(
      br(),
      strong(helpText("FORECASTING DATA")),
      helpText("You Haven't A Model For Forecasting Data", style = "text-color : red"),
      helpText("Creating Model first To Start Forecasting"))
    )}
  list(
    br(),
    strong(helpText("FORECASTING DATA")),
    helpText("You can proceed to start forecast data with button below"),
    numericInput("ftsaco_numberForecastdata", "Number of data to be forecasted, min is 1:", 1, min = 1, max = 50, step = 1),
    actionButton(inputId = "FTSACOForecastData", label = "Start Process", icon = icon("fa fa-spinner")),
    br(),
    uiOutput(outputId = "FTSACOStartForecast"),
    br(),
    dygraphOutput(outputId = "FTSACOPlotForecast", height = "450px")
  )
})

#tampilan dari tab summary
output$ftsacoSummary <- renderUI({
  deskription <- 
    list(br(),
         helpText(strong("SUMMARY")),
         helpText("Forecasting is effort to get value in future time.", 
                  "Forecast analysis is determine value in the future time with a model which constructed from current series data.", 
                  strong("Good forecasting"), "when forecasting value have ", 
                  strong("small error."), 
                  br(),
                  br(),
                  strong("SUMMARY OF FUZZY TIME SERIES - ANT COLONY OPTIMIZATION FORECAST ANALYSIS :"), br()
         ),
         br(),
         fluidRow(
           column(6,
                  helpText(strong("ERROR MODEL IN TRAINING AND TESTING SET :"))
           ),
           column(6,
                  helpText(strong("FORECASTING RESULT :"))
           )
         )
    )
  if(valuesFTSACO$FTSACOForecastBtn == 0){
    return(deskription);
  }else{
    indexmin <- FTSACOgetindexminmse(FTSACOgetDataPred()[[5]])
    pred <- c(FTSACOgetDataPred()[[5]][indexmin], sqrt(FTSACOgetDataPred()[[5]][indexmin]), FTSACOgetDataPred()[[6]][indexmin], FTSACOgetDataPred()[[7]][indexmin])
    fitting <- c(FTSACOgetDataFitting()[[1]][[1]], sqrt(as.numeric(FTSACOgetDataFitting()[[1]][[1]])), FTSACOgetDataFitting()[[2]][[1]], FTSACOgetDataFitting()[[3]][[1]])
    iteration <- length(FTSACOgetDataPred()[[5]])
    tabelerr <- cbind(fitting, pred)
    colnames(tabelerr) <- c("Fitting Error", "Testing Error")
    rownames(tabelerr) <- c("MSE", "RMSE", "MAPE(%)", "MAE")
    hasil <- list(
      deskription,
      fluidRow(
        column(6,
               wellPanel(
                 uiOutput(outputId = "ftsacoSummaryErrormodel"), 
                 style = "overflow-x:scroll; max-width: 500px; background-color: white;"
               )
        ),
        column(6,
               wellPanel(
                 uiOutput(outputId = "ftsacoSummaryResult"),
                 style = "overflow-x: scroll; max-width: 500px; overflow-y: scroll; max-height: 600px; background-color: white;"
               )
               
        )
      )
    )
    return(hasil);
  }
})

#fungsi bantuan learn button
output$FTSACOStartLearning <- renderUI({
  input$FTSACOlearnBtn
  isolate({
    if(input$FTSACOlearnBtn == 0 || is.null(input$FTSACOlearnBtn)) return(list())
    else{
      print("masuk activator")
      FTSACOdoLearn()
      return(list())
    }
  })
})

# fungsi bantuan process
output$FTSACOStartForecast<- renderUI({
  input$FTSACOForecastData
  isolate({
    if(input$FTSACOForecastData == 0 || is.null(input$FTSACOForecastData)) return(list())
    else{
      print("masuk activator")
      FTSACOdoForecast()
      return(list())
    }
  })
})

#mendapatkan data dari variabel yang terpilih dalam bentuk data.frame
FTSACOgetTheData <- reactive({
  if(is.null(input$FTSACOVarSelected)){
    return()
  }  
  theData <-data.frame(FTSACOgetVar())
  theData <- na.omit(theData)
  names(theData)[1] <- paste(input$FTSACOVarSelected)
  return(theData)
})

#mendapatkan panjang data yang variabel terpilih
FTSACOgetJumlahData <- reactive({
  return(length(FTSACOgetTheData()[,1]))
})

#mendapatkan awal data yang terpilih
FTSACOgetFirstNumData <- reactive({
  return(input$ftsaconumber_data[1])
})

#mendapatkan akhir data yang terpilih
FTSACOgetLastNumData <- reactive({
  return(input$ftsaconumber_data[2])
})

#mendapatkan data yang terpilih yang panjangnya dari First to Last
FTSACOgetJumlahDataTerpilih <- reactive({
  if(length(FTSACOgetFirstNumData()) == 0){
    return(FTSACOgetJumlahData())
  }else{
    return(length(FTSACOgetVar()[FTSACOgetFirstNumData():FTSACOgetLastNumData()]))
  }
})

#mendapatkan panjang datatraining sebesar 3/4 dari panjang data
FTSACOjumlah3per4Data <- reactive({
  return(FTSACOgetJumlahDataTerpilih()*3/4)
})

#mendapatkan index awal dari datatrainning
FTSACOgetFirstNumIndex <- reactive({
  return(1)
})

#mendapatkan index akhir dari datatrainning
FTSACOgetSecondNumIndex <- reactive({
  return(input$ftsaconumber_datatraining[1])
})

#mendapatkan datatrainning
FTSACOgetDataTraining <- reactive({
  if(is.null(FTSACOgetFirstNumData()) || is.null(FTSACOgetSecondNumIndex())){
    return()
  }
  return(FTSACOgetTheData()[FTSACOgetFirstNumData():(FTSACOgetFirstNumData()+FTSACOgetSecondNumIndex()-1), 1])
})

#mendapatkan datatesting
FTSACOgetDataTesting <- reactive({
  if(FTSACOgetSecondNumIndex() == FTSACOgetJumlahDataTerpilih()){
    return(NULL)
  }else{
    return(FTSACOgetTheData()[(FTSACOgetFirstNumData()+FTSACOgetSecondNumIndex()):(FTSACOgetFirstNumData()+FTSACOgetJumlahDataTerpilih()-1), 1]) 
  }
})

#mendapatkan panjang datatrainning
FTSACOTrainingLength <- reactive({
  return(length(FTSACOgetDataTraining()))
})

#mendapatkan panjang datatesting
FTSACOTestingLength <- reactive({
  return(length(FTSACOgetDataTesting()))
})

#mendapatkan maksimum iterasi
FTSACOgetMaxIter <- reactive({
  if(is.null(input$varFTSACO_maxIter) || !is.numeric(input$varFTSACO_maxIter) || input$varFTSACO_maxIter < 1){
    return(NaN)
    helpText("must be numeric")
  }else{
    return(as.integer(input$varFTSACO_maxIter)) 
  }
})

#mendapatkan maksimum mse yang diinginkan
FTSACOgetMaxMSE <- reactive({
  if(is.null(input$varFTSACO_maxMSE) || !is.numeric(input$varFTSACO_maxMSE) || input$varFTSACO_maxIter < 1){
    return(NaN)
  }else{
    return(as.numeric(input$varFTSACO_maxMSE))
  }
})

#mendapatkan jumlah semut untuk setiap siklus iterasi
FTSACOgetJmlAnt <- reactive({
  if(is.null(input$varFTSACO_ant) || !is.numeric(input$varFTSACO_ant) || input$varFTSACO_ant < 10){
    return(NaN)
  }else{
    return(as.integer(input$varFTSACO_ant))
  }
})

#mendapatkan nilai seed untuk titik permulaan angka random
FTSACOgetSetSeed <- reactive({
  if(is.null(input$FTSACOSetSeedInput) || is.numeric(as.numeric(input$FTSACOSetSeedInput)) == FALSE || input$FTSACOSetSeed == FALSE){
    valuesFTSACO$FTSACOSetSeedStatus <- 0
    return()
  }else{
    valuesFTSACO$FTSACOSetSeedStatus <- 1
    valuesFTSACO$FTSACOSetSeedValue <- input$FTSACOSetSeedInput
    return(input$FTSACOSetSeedInput)
  }
})

#mendapatkan jumlah data yang akan di prediksi
FTSACOgetNumberForecasting <- reactive({
  if(is.null(input$ftsaco_numberForecastdata) || !is.numeric(input$ftsaco_numberForecastdata) || input$ftsaco_numberForecastdata < 1){
    return(NaN);
  }else{
    return(input$ftsaco_numberForecastdata)
  }
})

#fungsi learning dan forecasting FTS ACO
FTSACOdoLearn <- function(){
  input$FTSACOlearnBtn
  isolate({
    if(is.nan(FTSACOgetMaxIter()) || is.nan(FTSACOgetMaxMSE()) || is.nan(FTSACOgetJmlAnt()) ){
      return(NULL)
    }
    if(valuesFTSACO$FTSACOlearnBtn == 0){
      print(Sys.time())
      set.seed(FTSACOgetSetSeed())
      valuesFTSACO$datatesting <- FTSACOProsesTesting()
      valuesFTSACO$datafitting <- FTSACOProsesFitting(valuesFTSACO$datatesting[[9]], valuesFTSACO$datatesting[[10]], valuesFTSACO$datatesting[[11]])
      print(Sys.time())
    }
    return(list(valuesFTSACO$datafitting, valuesFTSACO$datatesting));
  })
}

# fungsi Forecast Data
FTSACOdoForecast <- function(){
  input$FTSACOForecastData
  isolate({
    if(is.null(valuesFTSACO$datatesting) || is.nan(FTSACOgetNumberForecasting()) ){
      return();
    }else if(valuesFTSACO$FTSACOlearnBtn == 1){
        valuesFTSACO$dataForecast <- FTSACOProsesForecasting(FTSACOgetNumberForecasting())
        valuesFTSACO$FTSACOForecastBtn <- 1 
      }
    return(valuesFTSACO$dataForecast);
  })
}

#mendapatkan hasil fitting
FTSACOgetDataFitting <- function(){
  if(valuesFTSACO$FTSACOlearnBtn == 0){
    return()
  }
  return(FTSACOdoLearn()[[1]])
}

#mendapatkan hasil peramalan
FTSACOgetDataPred <- function(){
  if(valuesFTSACO$FTSACOlearnBtn == 0){
    return()
  }
  return(FTSACOdoLearn()[[2]])
}

# data error
FTSACOdataerror <- function(){
  if(valuesFTSACO$FTSACOlearnBtn == 0){
    return()
  }else{
    mse <- matrix(sapply(FTSACOgetDataPred()[[ 5 ]], function(x) round(x, digits = 5)), ncol = 1)
    rmse <- matrix(sapply(mse, function(x) round(sqrt(x), digits = 5)), ncol = 1)
    mape <- matrix(sapply(FTSACOgetDataPred()[[ 6 ]], function(x) round(x, digits = 5)), ncol = 1)
    mae <- matrix(sapply(FTSACOgetDataPred()[[ 7 ]], function(x) round(x, digits = 5)), ncol = 1)
    bic <- matrix(sapply(FTSACOgetDataPred()[[ 8 ]], function(x) round(x, digits = 5)), ncol=1)
    no <- seq(1, length(mse), 1);
    data <- data.frame(no, mse, rmse, mape, mae, bic)
    colnames(data) <- c("Iteration","MSE","RMSE","MAPE","MAE","BIC")
    return(data)
  }
}

#ketika model button nol maka forecast button nol
observe({
  if(valuesFTSACO$FTSACOlearnBtn == 0){
    valuesFTSACO$FTSACOForecastBtn <- 0
    valuesFTSACO$datatesting <- NULL
    valuesFTSACO$datafitting <- NULL
    valuesFTSACO$dataForecast <- NULL 
  }
})

#set nilai untuk pengecekan kondisi ketika button sudah diklik
observeEvent(input$FTSACOlearnBtn, {
  print("masuk observer")
  FTSACOdoLearn()
  valuesFTSACO$FTSACOlearnBtn <- 1
})

#pengecekan kondisi ketika ada perubahan pada metode
observeEvent(input$algoritmaFTS_FTSACO, {
  valuesFTSACO$FTSACOlearnBtn <- 0
})

#set nilai untuk pengecekan kondisi ketika ada perubahan pada kondisi data
observeEvent(input$FTSACOVarSelected, {
  valuesFTSACO$FTSACOlearnBtn <- 0
  updateTabsetPanel(session,"FTSACOMainTab", selected = "FTSACOfirstTab")
})

#set nilai untuk pengecekan kondisi ketika ada perubahan pada kondisi data
observeEvent(input$ftsaconumber_data, {
  valuesFTSACO$FTSACOlearnBtn <- 0
})

#set nilai untuk pengecekan kondisi ketika ada perubahan pada kondisi data
observeEvent(input$ftsaconumber_datatraining, {
  valuesFTSACO$FTSACOlearnBtn <- 0
})

#set nilai untuk pengecekan kondisi ketika ada perubahan pada kondisi data
observeEvent(input$varFTSACO_maxIter, {
  valuesFTSACO$FTSACOlearnBtn <- 0
})

#set nilai untuk pengecekan kondisi ketika ada perubahan pada kondisi data
observeEvent(input$varFTSACO_maxMSE, {
  valuesFTSACO$FTSACOlearnBtn <- 0
})

#set nilai untuk pengecekan kondisi ketika ada perubahan pada kondisi data
observeEvent(input$varFTSACO_ant, {
  valuesFTSACO$FTSACOlearnBtn <- 0
})

observeEvent(input$varFTSACO_step, {
  valuesFTSACO$FTSACOlearnBtn <- 0
})

observeEvent(input$varFTSACOBasedOn, {
  valuesFTSACO$FTSACOlearnBtn <- 0
})

###################################### Download Report #########################################

DRFACOdataterpilih <- function(){
  dataasli <- FTSACOgetVar()[FTSACOgetFirstNumData():FTSACOgetLastNumData()]
  persen <- c(NA, FTSACOPercentageChange(FTSACOgetTheData()[,1])[1:(length(dataasli)-1)])
  f <- lapply(FTSACOPercentageChange(FTSACOgetTheData()[,1])[1:(length(dataasli)-1)], 
              function(x) paste("A", FTSACOgetindeksdata(x, FTSACOgetDataPred()[[9]], FTSACOgetDataPred()[[10]])))
  fset <- c(NA, f)
  no <- seq(1, length(dataasli), 1)
  datamod <- FTSACOgetDataPred()[[12]][-1]
  data <- cbind(no, dataasli, datamod, persen, fset)
  colnames(data) <- c("Observation", "Data", "datamod", "Percentage Change", "Fuzzy Set")
  return(data)
}

DRFACOdatafitting <- function(){
  no <- seq(4,length(FTSACOgetDataFitting()[[4]])+3, 1)
  data1 <- FTSACOgetDataFitting()[[4]]
  data2 <- FTSACOgetDataFitting()[[5]]
  data <- data.frame(no, data1, data2)
  colnames(data) <- c("Observation","true value","forecast")
  return(data)
}

DRFACOdataforecast <- function(){
  if(is.null(valuesFTSACO$dataForecast[[1]][-(1:2)]) || valuesFTSACO$FTSACOForecastBtn == 0){
    obs <- 1
    nilai_prediksi <- "none"
    persen_prediksi <- "none"
    table <- cbind(obs, nilai_prediksi, persen_prediksi)
    colnames(table) <- c("Observation", "Forcasting Value", "Percentage Value")
  }else{
    nilai_prediksi <- data.frame(valuesFTSACO$dataForecast[[1]][-(1:2)])
    persen_prediksi <- data.frame(valuesFTSACO$dataForecast[[2]][-(1:2)])
    obs <- seq(1, length(valuesFTSACO$dataForecast[[1]][-(1:2)]), 1)
    table <- cbind(obs, nilai_prediksi, persen_prediksi)
    colnames(table) <- c("Observation", "Forcasting Value", "Percentage Value")
  }
  return(table)
}

DRFACOinfodataShareToForum <- function(){
  if(input$algoritmaFTS_FTSACO == "FTS - ACO"){
    NUMBER_OF_ANT <- FTSACOgetJmlAnt()
    SELECTING_MODEL_BASED_ON <- input$varFTSACOBasedOn
    ANT_STEP_WIDTH <- input$varFTSACO_step
    SEED <- FTSACOgetSetSeed()
  }else{
    NUMBER_OF_ANT <- "NONE"
    SELECTING_MODEL_BASED_ON <- "MSE"
    ANT_STEP_WIDTH <- "NONE"
    SEED <- "NONE"
  }
  
  METODE <- input$algoritmaFTS_FTSACO
  TITLE_DATA <- input$datasets
  vAR_DATA <- input$FTSACOVarSelected
  NUMBER_OF_DATA <- FTSACOgetJumlahDataTerpilih()
  NUMBER_OF_TRAINING <- FTSACOTrainingLength()
  NUMBER_OF_TESTING <- FTSACOTestingLength()
  NUMBER_OF_ITERATION <- length(FTSACOdataerror()[[2]])
  
  info <- rbind(
    METODE,
    TITLE_DATA,
    vAR_DATA,
    NUMBER_OF_DATA,
    NUMBER_OF_TRAINING,
    NUMBER_OF_TESTING,
    NUMBER_OF_ITERATION,
    NUMBER_OF_ANT,
    ANT_STEP_WIDTH,
    SELECTING_MODEL_BASED_ON,
    SEED
  )
  aa <- cbind(info)
  colnames(info) <- "VALUE PARAMETER"
  rownames(info) <- c("METODE",
                      "DATA",
                      "VARIABEL DATA",
                      "NUMBER OF DATA",
                      "NUMBER OF TRAINING",
                      "NUMBER OF TESTING",
                      "NUMBER OF ITERATION",
                      "NUMBER OF ANT",
                      "ANT STEP WIDTH",
                      "SELECTING MODEL BASED ON",
                      "SEED")
  return(info)
}

DRFACOinfoErrorShareToForum <- function(){
  if(FTSACOTestingLength()>0){
    pindeksmin <- FTSACOgetindexminmse(FTSACOdataerror()[[2]])
    pmse <- FTSACOdataerror()[[2]][[pindeksmin]]
    prmse <- FTSACOdataerror()[[3]][[pindeksmin]]
    pmape <- FTSACOdataerror()[[4]][[pindeksmin]]
    pmae <- FTSACOdataerror()[[5]][[pindeksmin]]
  }else{
    pmse <- "NONE"
    prmse <- "NONE"
    pmape <- "NONE"
    pmae <- "NONE"
  }
  
  a <- rbind(pmse, prmse, pmape, pmae)
  
  fmse <- FTSACOgetDataFitting()[[1]][[1]]
  frmse <- sqrt(as.numeric(FTSACOgetDataFitting()[[1]][[1]]))
  fmape <- FTSACOgetDataFitting()[[2]][[1]]
  fmae <- FTSACOgetDataFitting()[[3]][[1]]
  b <- rbind(fmse, frmse, fmape, fmae)
  
  ab <- cbind(b, a)
  colnames(ab) <- c("Fitting Error", "Predicting Error")
  rownames(ab) <- c("MSE", "RMSE", "MAPE", "MAE")
  
  
  
  return(ab)
}

DRFACOplotmodel <- function(){
  no <- seq(1,(length(FTSACOgetDataPred()[[12]])-1), 1)
  data1 <- FTSACOgetDataTraining()[-1]
  data2 <- FTSACOgetDataPred()[[12]][-1]
  data <- data.frame(no, data1, data2)
  colnames(data) <- c("no","true value","forecast")
  varplot <- ggplot(data, aes(no)) +
    geom_line(aes(y = data1, colour = "True Value"), lwd = 1) +
    geom_line(aes(y = data2, colour = "Value (Model)"), lwd = 1) +
    theme_bw() +
    scale_color_manual(values=c("#f53e05", "#3A5FCD")) +
    theme(text = element_text(size=12),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
          axis.text.y = element_text(angle = 00, hjust = 1, size = 12),
          legend.position = "bottom") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) + 
    scale_size_area() + 
    xlab("Observation") +
    ylab("Value") 
  
  return(varplot)
}

DRFACOplotfitting <- function(){
  no <- seq(1,length(FTSACOgetDataFitting()[[4]]), 1)
  data1 <- FTSACOgetDataFitting()[[4]]
  data2 <- FTSACOgetDataFitting()[[5]]
  data <- data.frame(no, data1, data2)
  colnames(data) <- c("no","true value","Predicting")
  varplot <- ggplot(data, aes(no)) +
    geom_line(aes(y = data1, colour = "True Value"), lwd = 1) +
    geom_line(aes(y = data2, colour = "Predict"), lwd = 1) +
    theme_bw() +
    scale_color_manual(values=c("#f53e05", "#3A5FCD")) +
    theme(text = element_text(size=12),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
          axis.text.y = element_text(angle = 00, hjust = 1, size = 12),
          legend.position = "bottom") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) + 
    scale_size_area() + 
    xlab("Observation") +
    ylab("Value")
  
  return(varplot)
}

DRFACOplottesting <- function(){
  no <- seq(1,length(FTSACOgetDataPred()[[2]][-(1:2)]), 1)
  data1 <- FTSACOgetDataPred()[[2]][-(1:2)]
  data2 <- FTSACOgetDataPred()[[3]][-(1:2)]
  data <- data.frame(no, data1, data2)
  colnames(data) <- c("no","true value","forecast")
  plot(data1, xlab="time observation", ylab=input$FTSACOVarSelected, 
       main="Testing Data", type="l")
  lines(data2, xlab="time observation", ylab=input$FTSACOVarSelected, type="l", col="red")
}

DRFACOplotForc <- function(){
  if(is.null(valuesFTSACO$dataForecast[[1]][-(1:2)]) || valuesFTSACO$FTSACOForecastBtn == 0){
    return(helpText("You Haven't Any Forecasting Data"))
  }else{
    indexmin <- FTSACOgetindexminmse(FTSACOgetDataPred()[[5]])
    rmse <- sqrt(FTSACOgetDataPred()[[5]][indexmin])
    no <- seq(1,length(valuesFTSACO$dataForecast[[1]][-(1:2)]), 1)
    dataprediksi <- as.matrix(valuesFTSACO$dataForecast[[1]][-(1:2)], ncol=1)
    data0 <- apply(dataprediksi, 1, function(x) (x-rmse))
    data1 <- dataprediksi
    data2 <- apply(dataprediksi, 1, function(x) (x+rmse))
    data <- data.frame(no, data0, data1, data2)
    colnames(data) <- c("no","data0", "Forecasted Data", "data2")
    varplot <- ggplot(data, aes(no)) +
      geom_line(aes(y = data0, colour = "upper-under of CI"), lwd = 1) +
      geom_line(aes(y = data1, colour = "Forecast"), lwd = 1.5) +
      geom_line(aes(y = data2, colour = "upper-under of CI"), lwd = 1) +
      theme_bw() +
      scale_color_manual(values=c("#EEEE00", "#f53e05")) +
      theme(text = element_text(size=12),
            axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
            axis.text.y = element_text(angle = 00, hjust = 1, size = 12),
            legend.position = "bottom") +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) + 
      scale_size_area() + 
      xlab("Observation") +
      ylab("Value")
    
    return(varplot)
  }
}

DRFACOSelectedModel <- function(){
  iterasi <- FTSACOgetindexminmse(FTSACOdataerror()[[2]])
  batasbawah <- FTSACOgetDataPred()[[9]]
  batasatas <- FTSACOgetDataPred()[[10]]
  indeks <- seq(1, length(batasatas), 1) 
  selang <- cbind(indeks, batasbawah, batasatas)
  colnames(selang) <- c("Index Interval","Lower Limit", "Upper Limit")
  return(list(iterasi, selang))
}

DRFACOerror <- function(){
  return(FTSACOdataerror())
}

# Download Report
output$downloadReportFTSACO <- downloadHandler(

  filename = function() {
    paste('Forecasting Report', sep = '.', 
          switch(
            input$formatFTSACO, PDF = 'pdf', HTML = 'html', Word = 'docx'
          )
    )
  },
  
  content = function(file) {
    src <- normalizePath('ftsaco-report.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'ftsaco-report.Rmd')
    
    library(rmarkdown)
    out <- render('ftsaco-report.Rmd', switch(
                      input$formatFTSACO,
                      PDF = pdf_document(), HTML = html_document(), Word = word_document()
                    )
    )
    file.rename(out, file)
  } 
)

############################################## Fungsi Sharing Forum ##################################################

#event observer sharing ke forum button
observeEvent(input$shareFtsAcoForum, {
  valuesFTSACO$info <- ""
  postFtsAcotoForum()
  updateTextInput(session, "subjectFtsAcoForum", value = "")
  updateTextInput(session, "contentFtsAcoForum", value = "")
  valuesFTSACO$info <- "Success Sharing"
})

postFtsAcotoForum <- function(){
  library(rmarkdown)
  time <- format(Sys.time(),"%a%b%d%y%H%M%S")
  if(nrow(getUsernameFromDB()) != 0){
    folder <- getUsernameFromDB()
    dir <- paste0("C:\\xampp\\htdocs\\fast_forum\\media\\",folder,"\\",time)
  }else{
    dir <- paste0("C:\\xampp\\htdocs\\fast_forum\\media\\NoName\\",time)
  }
  out <- render('ftsaco-report1.Rmd',"html_document",output_dir = dir,output_file = "FTSACO.html")
  frame <- paste0("<iframe src=''","http://localhost/fast_forum/media/",folder,"/",time,"/FTSACO.html''"," width= ''100%'' height=''800px''></iframe>")
  content <- paste0("<p>",input$contentFtsAcoForum,"</p>","</br>",frame,"</br>")
  insertTopic(input$subjectFtsAcoForum,content,input$chooseCategoryShareFtsAco)
  print("Success Sharing")
}

#kondisi benar
output$modalShareFtsAco <- renderUI({
  user <- getUsernameFromDB()
  id 
  if(nrow(user)==0){
    list(
      helpText("Please login into Fast Forum for using this feature"),
      bsAlert("modalAlert")
    )
  }else{
    list(
      helpText("This feature allows you to share your analysis to the Forum in the Fuzzy Forecasting Subforum."),
      bsAlert("modalAlert"),
      textInput("subjectFtsAcoForum", strong("Subject :") , value = ""),br(),
      strong("Content :"),br(),
      HTML('<textarea id="contentFtsAcoForum" rows="5" cols="20", class = "form-control"></textarea>'), br(),
      selectInput("chooseCategoryShareFtsAco","Choose Category",getListNode()$name),
      helpText(strong(valuesFTSACO$info)),br(),
      footer = list(
        bsButton("shareFtsAcoForum", "Submit to Forum", style="primary", type = "action"),
        tags$button(type = "button", class = "btn btn-default", 'data-dismiss' = "modal", "Close")
      )
    )
  }
  
})

######################################################################################################################
#fungsi-fungsi analisis Fuzzy Time Series Ant Colony Optimization
######################################################################################################################

#mendapatkan indeks dari selang yang terbentuk yang memiliki mse terkecil
cppFunction('int FTSACOgetindexminmse(NumericVector datamse){
  int hasil = 0;    
  if(datamse.size() == 0){
    hasil = 0;
  }else{
    for(int i=0; i < datamse.size(); i++){
        if(datamse[i] == min(datamse)){
          hasil = i + 1;
          i = datamse.size();
      }
    }
  }
return hasil;
}')

#mendapatkan index selang dari suatu data
cppFunction('int FTSACOgetindeksdata(double persen, NumericVector batasbawah, NumericVector batasatas){
  int hasil;
  for(int i=0; i<batasbawah.size(); i++){
    if(persen >= batasbawah[i] && persen < batasatas[i]){
      hasil = i + 1;
      i = batasbawah.size();
    }else if(i == batasbawah.size()-1 && persen >= batasbawah[i] && persen <= batasatas[i]){
      hasil = i + 1;
      i = batasbawah.size();
    }else if(persen > batasatas[batasatas.size()-1]){
      hasil = batasatas.size();
      i = batasbawah.size();
    }else if(persen < batasbawah[0]){
      hasil = 1;
      i = batasbawah.size();
    }
  }
return hasil;
}')

# mendapatkan vektor fuzzy set
cppFunction('NumericVector FTSACOFuzzyset(NumericVector data, NumericVector BB, NumericVector BA, Function indeksdata){
    int n = data.size();  
    NumericVector Fuzzyset(n);

      for(int i=0; i<n; i++){
          Fuzzyset[i] = as<int>(indeksdata(data[i], BB, BA)); 
      }
  
  return Fuzzyset;
}')

#mendapatkan MSE dari suatu proses predikasi / estimasi
cppFunction('double FTSACOgetMSE(NumericVector datatrue, NumericVector dataforc){
  double MSEFTS = 0;
  if(datatrue.size() > 0){
    for(int i=0; i < datatrue.size(); i++){
      MSEFTS += (datatrue[i] - dataforc[i]) * (datatrue[i] - dataforc[i]);
    }
    MSEFTS = MSEFTS / datatrue.size();
  }
return MSEFTS;
}')

#mendapatkan MAPE dari suatu proses predikasi / estimasi
cppFunction('double FTSACOgetMAPE(NumericVector datatrue, NumericVector dataforc){
  double MAPEFTS = 0;
  if(datatrue.size() > 0){
    for(int i=0; i < datatrue.size(); i++){
      MAPEFTS += fabs(datatrue[i] - dataforc[i]) / fabs(datatrue[i]);
    }
    MAPEFTS = (MAPEFTS / datatrue.size()) * 100;
  }
return MAPEFTS;
}')

#mendapatkan MAE dari suatu proses predikasi / estimasi
cppFunction('double FTSACOgetMAE(NumericVector datatrue, NumericVector dataforc){
  double MAEFTS = 0;
  if(datatrue.size() > 0){
    for(int i=0; i < datatrue.size(); i++){
      MAEFTS += fabs(datatrue[i] - dataforc[i]);
    }
    MAEFTS = MAEFTS / datatrue.size();
  }
return MAEFTS;
}')

#menghitung jumlah keanggotaan interval fuzzy
cppFunction('NumericVector jmlAnggota(NumericVector data, NumericVector batasbawah, NumericVector batasatas){
  NumericVector jmlAnggota(batasatas.size());  
  for(int i=0; i<data.size(); i++){
      for(int j=0; j<batasatas.size(); j++){
          if(data[i] >= batasbawah[j] && data[i] < batasatas[j]){
            jmlAnggota[j] += 1;
          }
      }
  }
return jmlAnggota;
}')

#mendapatkan nilai percentage change
cppFunction('NumericVector FTSACOPercentageChange(NumericVector data){
  NumericVector persenchange(data.size());
    for(int i=0; i<(data.size()-1); i++){
        persenchange[i] = round(((data[i+1] - data[i]) * 100 / data[i]) * 100) / 100; //dikali 100 terus dibagi 100 untuk pembulatan saja
    }
  return persenchange;
}')

#mendapatkan index dari fuzzy relation if-then dengan Fuzzy high Order 2
cppFunction('double FTSACOgetNilaiRamalan(NumericVector flagrelation, NumericVector datarel, int order1, int order2, Function getvalue, NumericVector batasbawah, NumericVector batasatas){
  int lengthmat = datarel.size();
  NumericMatrix hasil(lengthmat, 1);
  double hasilakhir=0;

  int hasilke = 0;
  int nrel = datarel.size();

  NumericVector datarel1((nrel - 2));
  NumericVector datarel2((nrel - 2));
  NumericVector datarel3((nrel - 2));
  
  NumericVector datarel11((nrel - 1));
  NumericVector datarel21((nrel - 1));
  
  //set relation untuk Fuzzy order 2
  for(int i=0; i < (nrel - 2) ; i++){
    datarel1[i] = datarel[i];
    datarel2[i] = datarel[i+1];
    datarel3[i] = datarel[i+2];
  }
  
  //set relation untuk Fuzzy order 1
  for(int j=0; j<(nrel - 1); j++){
    datarel11[j] = datarel[j];
    datarel21[j] = datarel[j+1];
  }
  
  // untuk Fuzzy order 2
  for(int i=0; i<datarel1.size(); i++){
    if(datarel1[i] == order1 && datarel2[i] == order2 && flagrelation[i] == FALSE){
      flagrelation[i] = TRUE;
      hasil[hasilke] = datarel3[i];
      hasilke++;
    }
  }
  
  // untuk Fuzzy order 1            
  if(hasilke == 0){
   for(int i=0; i<datarel11.size(); i++){
    if(datarel11[i] == order2 && flagrelation[i] == FALSE){
        flagrelation[i] = TRUE;
        hasil[hasilke] = datarel21[i];
        hasilke++;
      }
    }
  }
  
  // untuk fuzzy order 0
  if(hasilke == 0){
    hasil[hasilke] = order2;
    hasilke++;
  }
  
  NumericMatrix hasil1(hasilke, 1);
  int j = 0;
    for(int i=0; i<lengthmat; i++){
      if(hasil[i] > 0) {
        hasil1[j] = hasil[i]; 
        j++;
      }
    }

  hasilakhir = as<double>(getvalue(hasil1, batasbawah, batasatas));
  return hasilakhir;
}')

#mendapatkan nilai ramalan dengan model
cppFunction('double FTSACOgetValue(NumericVector hasil, NumericVector batasbawah, NumericVector batasatas){
  double hasilakhir = 0, a1 = 0, a2 = 0, a3 = 0;
    for(int i=0; i<hasil.size(); i++){
        if(hasil[i] == 1){
           a2 = (batasbawah[ (hasil[i]-1) ] + batasatas[ (hasil[i]-1) ] ) / 2;
           a3 = (batasbawah[ (hasil[i]) ]   + batasatas[ (hasil[i]) ]) / 2;
           hasilakhir += round(( 1.5/( 1/a2 + 0.5/a3 )) * 10000)/10000; //dikali 10000 terus dibagi 10000 hanya untuk pembulatan 4 digit
        }else if(hasil[i] == batasatas.size()){
           a1 = (batasbawah[ (hasil[i]-2) ] + batasatas[ (hasil[i]-2) ]) / 2;
           a2 = (batasbawah[ (hasil[i]-1) ] + batasatas[ (hasil[i]-1) ]) / 2;
           hasilakhir += round(( 1.5/( 0.5/a1 + 1/a2 )) * 10000)/10000; //dikali 10000 terus dibagi 10000 hanya untuk pembulatan 4 digit;
        }else{
           a1 = (batasbawah[ (hasil[i]-2) ] + batasatas[ (hasil[i]-2) ]) / 2;
           a2 = (batasbawah[ (hasil[i]-1) ] + batasatas[ (hasil[i]-1) ]) / 2;
           a3 = (batasbawah[ (hasil[i]) ]   + batasatas[ (hasil[i]) ]) / 2;
           hasilakhir += round(( 2/( 0.5/a1 + 1/a2 + 0.5/a3 )) * 10000)/10000; //dikali 10000 terus dibagi 10000 hanya untuk pembulatan 4 digit;
        }
      }
   hasilakhir = hasilakhir / hasil.size();
   return hasilakhir;
}')

#update pheromones
cppFunction('NumericVector FTSACOUpdatePheromones(NumericVector input, NumericVector selangterpilih, double tho0, double bic) {
  int n = input.size();
  NumericVector out(n);
    for(int i = 0; i < n; i++) {
      out[i] = 0.9 * input[i];
    }

    for(int j = 0; j < selangterpilih.size(); j++){
      out[ (selangterpilih[j]-1) ] = out[ (selangterpilih[j]-1) ] + (tho0 / bic) ;
    }

return out;
}'
)

# menghitung peluang titik
cppFunction('NumericVector FTSACOPeluangtitik(NumericVector input){
  int n = input.size(); 
  NumericVector out(n);
  double penyebut;
    for(int i = 0; i < n; i++) {
        penyebut += input[i];
    }
  
    for(int i = 0; i < n; i++) {
        out[i] = input[i] / penyebut;
    }

return out;
}')

# menghitung banyak selang dengan tabel basis
cppFunction('int banyakselang(NumericVector data){
  double difabsolute;
      for(int i=0; i<(data.size()-1); i++){
        difabsolute += abs(data[i+1] - data[i]);
      }
            
  double interval = (difabsolute / data.size()) / 2;
  float nilaibasis;
    if(interval > 0 && interval <=1){
        nilaibasis = 0.1;
    }else if(interval > 1 && interval <= 10){
        nilaibasis = 1;
    }else if(interval > 10 && interval <= 100){
        nilaibasis = 10;
    }else if(interval > 100 && interval <= 1000){
        nilaibasis = 100;
    }else{
        nilaibasis = 1000;
    }
            
  int banyakselang = ceil((max(data) - min(data)) / nilaibasis);
  return banyakselang;
}')

#proses testing data dari model
FTSACOProsesTesting <- function(){
  iter          <- 1
  mse           <- 9999999999999
  iterasi       <- list()
  forecasting   <- list()
  
  if(input$algoritmaFTS_FTSACO == 'FTS - ACO'){
    getmaksiter   <- FTSACOgetMaxIter()
  }else{
    getmaksiter   <- 1
  }
  
  ## untuk progress bar ===============
  withProgress(message = 'Creating Model', value = 0, {
    while(iter <= getmaksiter && mse > FTSACOgetMaxMSE()){
      iterasi[[iter]] <- NaN
      iterasi[[iter]] <- FTSACOProsesTraining()
      # #isi return value FTSACOProsesTraining()
      #1. index min MSE
      #2. MSE
      #3. MAPE
      #4. BIC
      #5. batas bawah
      #6. batas atas
      #7. fuzzyrelation
      #8. nilai forc (hasil memodelkan, dengan menggunakan data training)
      #proses peramalan dengan data testing
      #matriks untuk menyimpan nilai forecasting
      
      # # mengambil nilai data training kedua terakhir
      for(i in 1: 2){
        forecasting$persenforc[[i]] <- FTSACOPercentageChange(FTSACOgetTheData()[,1])[(FTSACOgetFirstNumData()+FTSACOTrainingLength()-4+i)]
        forecasting$nilaiforc[[i]] <- FTSACOgetTheData()[,1][(FTSACOgetFirstNumData()+FTSACOTrainingLength()-3+i)]
        forecasting$fuzzyvalue[[i]] <- FTSACOgetindeksdata(FTSACOPercentageChange(FTSACOgetTheData()[,1])[(FTSACOgetFirstNumData()+FTSACOTrainingLength()-4+i)], iterasi[[iter]][[5]], iterasi[[iter]][[6]])
      }
      forecasting$persentrue <- FTSACOPercentageChange(FTSACOgetTheData()[,1])[(FTSACOgetFirstNumData()+FTSACOTrainingLength()-3):(FTSACOgetLastNumData()-1)]
      forecasting$nilaitrue <- FTSACOgetTheData()[,1][(FTSACOgetFirstNumData()+FTSACOTrainingLength()-2) : FTSACOgetLastNumData()]
      
      if(FTSACOTestingLength() > 0){
        #melakukan peramalan
        for(i in 2 : (FTSACOTestingLength()+1)){
          forecasting$fuzzyrelation[[iter]] <- rep(FALSE, length(iterasi[[iter]][[7]]))
          forecasting$persenforc[[i+1]] <- FTSACOgetNilaiRamalan(
            forecasting$fuzzyrelation[[iter]],
            iterasi[[iter]][[7]],
            forecasting$fuzzyvalue[[i-1]], 
            forecasting$fuzzyvalue[[i]],
            FTSACOgetValue,
            iterasi[[iter]][[5]],
            iterasi[[iter]][[6]])
          forecasting$nilaiforc[[i+1]] <- ((forecasting$persenforc[[ i+1 ]] + 100) / 100) * forecasting$nilaiforc[[i]]
          forecasting$fuzzyvalue[[i+1]] <- FTSACOgetindeksdata(forecasting$persenforc[[i+1]], iterasi[[iter]][[5]], iterasi[[iter]][[6]])
          cat("\rtesting", " >>>> i", i, "of", (FTSACOTestingLength()+1)) 
          flush.console()
        }
        mse <- FTSACOgetMSE(forecasting$nilaitrue[-(1:2)], forecasting$nilaiforc[-(1:2)])
        mape <- FTSACOgetMAPE(forecasting$nilaitrue[-(1:2)], forecasting$nilaiforc[-(1:2)])
        mae <- FTSACOgetMAE(forecasting$nilaitrue[-(1:2)], forecasting$nilaiforc[-(1:2)])
      }else if(input$algoritmaFTS_FTSACO == 'FTS - ACO' && FTSACOTestingLength() == 0){
        relation  <- iterasi[[iter]][[7]]
        fitting       <- FTSACOProsesFitting(iterasi[[iter]][[5]], iterasi[[iter]][[6]], relation)
        mse <- fitting[[1]]
        mape <- fitting[[2]]
        mae <- fitting[[3]]
      }else if(input$algoritmaFTS_FTSACO == 'FTS' && FTSACOTestingLength() == 0){
        mse <- 0
        mape <- 0
        mae <- 0
      }
      forecasting$mse[[iter]] <- mse
      forecasting$mape[[iter]] <- mape
      forecasting$mae[[iter]] <- mae
      forecasting$bic[[iter]] <- iterasi[[iter]][[4]]
      forecasting$hasil[[iter]] <- list(
        forecasting$persentrue,
        forecasting$persenforc,
        forecasting$nilaitrue,
        forecasting$nilaiforc,
        forecasting$fuzzyvalue
      )
      
      #Increment the progress bar, and update the detail text.
      incProgress(1/FTSACOgetMaxIter(), detail = paste("( Iteration ", iter, " of ", FTSACOgetMaxIter(), ")"))
      
      cat("\niterasi", iter, "of", FTSACOgetMaxIter(), " MSE : ", mse, "\n")
      flush.console()
      iter <- iter + 1
      
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
      
    }
  })
  forc <- list(
    indexMSEmin <- FTSACOgetindexminmse(forecasting$mse),
    forecasting$hasil[[ indexMSEmin ]][[ 3 ]],
    forecasting$hasil[[ indexMSEmin ]][[ 4 ]],
    forecasting$hasil[[ indexMSEmin ]][[ 5 ]],
    forecasting$mse,
    forecasting$mape,
    forecasting$mae,
    forecasting$bic,
    iterasi[[ indexMSEmin ]][[ 5 ]],
    iterasi[[ indexMSEmin ]][[ 6 ]],
    iterasi[[ indexMSEmin ]][[ 7 ]],
    iterasi[[ indexMSEmin ]][[ 8 ]]
  )
  return( forc );
}

#proses Fitting model dengan data training
FTSACOProsesFitting <- function(batasbawah, batasatas, relation){
  forecasting   <- list()
  
  # # mengambil nilai data training kedua pertama
  for(i in 1: 2){
    forecasting$persenforc[[i]] <- FTSACOPercentageChange(FTSACOgetTheData()[,1])[(FTSACOgetFirstNumData()-1+i)]
    forecasting$nilaiforc[[i]] <- FTSACOgetTheData()[,1][(FTSACOgetFirstNumData()+i)]
  }
  
  forecasting$persentrue <- FTSACOPercentageChange(FTSACOgetTheData()[,1])[FTSACOgetFirstNumData():(FTSACOgetFirstNumData()+FTSACOTrainingLength()-2)]
  forecasting$nilaitrue <- FTSACOgetTheData()[,1][(FTSACOgetFirstNumData()+1):(FTSACOgetFirstNumData()+FTSACOTrainingLength()-1)]
  forecasting$fuzzyvalue <- relation[FTSACOgetFirstNumData():( FTSACOgetFirstNumData() + FTSACOTrainingLength()-2 )]
  
  if(FTSACOTrainingLength() > 0){
    #melakukan peramalan
    withProgress(message = 'Fitting Model', value = 0, {
      for(i in 2 : (FTSACOTrainingLength()-2)){
        forecasting$fuzzyrelation <- rep(FALSE, length(relation))
        forecasting$persenforc[[i+1]] <- FTSACOgetNilaiRamalan(
          forecasting$fuzzyrelation,
          relation,
          forecasting$fuzzyvalue[[i-1]], 
          forecasting$fuzzyvalue[[i]],
          FTSACOgetValue,
          batasbawah,
          batasatas)
        forecasting$nilaiforc[[i+1]] <- ((forecasting$persenforc[[ i+1 ]] + 100) / 100) * forecasting$nilaitrue[[i]]
        cat("\rfitting >>>> i", i, "of", (FTSACOTrainingLength()-2)) 
        flush.console()
        
        incProgress(1/(FTSACOTrainingLength()-2), detail = paste("(Observation ", (i-1), " of ", (FTSACOTrainingLength()-3), ")"))
        # Pause for 0.01 seconds to simulate a long computation.
        Sys.sleep(0.01)
      }
    })
  }
  
  mse <- FTSACOgetMSE(forecasting$nilaitrue[-(1:2)], forecasting$nilaiforc[-(1:2)])
  mape <- FTSACOgetMAPE(forecasting$nilaitrue[-(1:2)], forecasting$nilaiforc[-(1:2)])
  mae <- FTSACOgetMAE(forecasting$nilaitrue[-(1:2)], forecasting$nilaiforc[-(1:2)])
  return(list(mse, mape, mae, forecasting$nilaitrue[-(1:2)], forecasting$nilaiforc[-(1:2)]))
}

#proses memprediksi data
FTSACOProsesForecasting <- function(n){
  data.true         <- FTSACOgetDataPred()[[ 2 ]]
  batas.bawah       <- FTSACOgetDataPred()[[ 9 ]]
  batas.atas        <- FTSACOgetDataPred()[[ 10 ]]
  prediksi.nilaiforc <- NaN
  prediksi.fuzzyvalue <- NaN
  prediksi.persenforc <- NaN
  prediksi.nilaiforc[[ 1 ]] <- data.true[[ length(data.true)-1 ]]
  prediksi.nilaiforc[[ 2 ]] <- data.true[[ length(data.true)]]
  
  if(FTSACOTestingLength() > 0){
    FvalueTest        <- matrix(ncol = 1, nrow = FTSACOTestingLength())
    for(i in 1:FTSACOTestingLength()){
      FvalueTest[i] <- FTSACOgetindeksdata(FTSACOPercentageChange(FTSACOgetTheData()[,1])[(FTSACOgetFirstNumData()+FTSACOTrainingLength()-2+i)], batas.bawah, batas.atas)
    }
  }else{
    FvalueTest <- NULL
  }
  
  data.relation     <- c(FTSACOgetDataPred()[[11]], FvalueTest)
  prediksi.fuzzyvalue[[ 1 ]] <- data.relation[[ (length(data.relation) - 1) ]]
  prediksi.fuzzyvalue[[ 2 ]] <- data.relation[[ length(data.relation) ]]
  prediksi.fuzzyrelation <- rep(FALSE, length(data.relation))
  
  for(i in 2:(n+1)){
    prediksi.persenforc[[i+1]] <- FTSACOgetNilaiRamalan(
      prediksi.fuzzyrelation,
      data.relation,
      prediksi.fuzzyvalue[[ i-1 ]], 
      prediksi.fuzzyvalue[[ i ]], 
      FTSACOgetValue,
      batas.bawah,
      batas.atas
    )
    prediksi.nilaiforc[[i+1]] <- ((prediksi.persenforc[[ i+1 ]] + 100) / 100) * prediksi.nilaiforc[[ i ]]
    prediksi.fuzzyvalue[[i+1]] <- FTSACOgetindeksdata(prediksi.persenforc[[ i+1 ]], batas.bawah, batas.atas)
  }
  return(list(prediksi.nilaiforc, prediksi.persenforc))
}

#proses trainning model dengan menggunakan Fuzzy - ant colony optimization
FTSACOProsesTraining <- function(){
  ########################_____identifikasi data_______###############################
  
  FTSACO                          <- list()
  FTSACOframe                     <- NaN
  FTSACOframe.data                <- FTSACOgetTheData()[,1]
  FTSACOframe.percentage_change   <- FTSACOPercentageChange(FTSACOgetTheData()[,1])
  FTSACOframe.datatrain           <- FTSACOgetDataTraining()
  
  FTSACOframe.fenrollment_1.batasbawah  <- NaN
  FTSACOframe.fenrollment_1.batasatas   <- NaN
  FTSACOframe.fenrollment_1.jmlanggota  <- NaN
  FTSACOframe.fuzzyvalue                <- NaN
  
  if(input$algoritmaFTS_FTSACO == 'FTS - ACO'){
    jumlahsemut     <- FTSACOgetJmlAnt()
    banyakselang    <- banyakselang(FTSACOframe.datatrain)
    banyakselang    <- floor(banyakselang/2)
  }else{
    jumlahsemut     <- 1
    banyakselang    <- 7
  }
  
  #mencari nilai min dan max untuk menentukan interval
  max_change <- ceiling(max(FTSACOframe.percentage_change[ (FTSACOgetFirstNumData()) : (FTSACOgetFirstNumData()+FTSACOTrainingLength()-2)]) )
  min_change <- floor(min(FTSACOframe.percentage_change[ (FTSACOgetFirstNumData()) : (FTSACOgetFirstNumData()+FTSACOTrainingLength()-2)]) )
  
  
  #membentuk selang yang pertama / awal
  size_interval <- (max_change - min_change)/banyakselang
  for(i in 1:banyakselang){
    FTSACOframe.fenrollment_1.batasbawah[[i]] <- round(min_change + size_interval * (i-1) , digits = 2)
    FTSACOframe.fenrollment_1.batasatas[[i]] <- round(min_change + size_interval * (i), digits = 2)
    FTSACOframe.fenrollment_1.jmlanggota[[i]] <- 0
  }
  
  ########################_____identifikasi parameter ACO_______##################################
  
  if(input$algoritmaFTS_FTSACO == 'FTS - ACO'){
    
    ## tetapan pengupan pheromones
    tho0 <- 0.1
    FTSACO$selangbagian  <- FTSACOframe.fenrollment_1.batasatas  #nilainya tidak tergantung iterasi
    FTSACO$nilaitho[[1]]      <- rep(tho0, length(FTSACOframe.fenrollment_1.batasatas))   #nilainya tergantung iterasi
    FTSACO$nilaipeluang[[1]]  <- rep((tho0/length(FTSACOframe.fenrollment_1.batasatas)), length(FTSACOframe.fenrollment_1.batasatas))   #nilainya tergantung iterasi
  }
  
  
  #####_________membuat selang dengan optimasi Ant Colony dengan cara random_____######################
  
  FTSACOSelangSemut <- list()
  for(ant in 1:jumlahsemut){
    ############################################################################################################### 
    ################################# dijalankan hanya ketika menggunakan algoritma koloni semut ##################
    ############################################## proses pencarian selang ########################################
    if(input$algoritmaFTS_FTSACO == 'FTS - ACO'){
      FTSACOSelangSemut[[ant]] <- NaN
      FtsAcoselangtemp <- list(); 
      selangterpilih <- list();
      FtsAcoselangtemp[1] <- NaN
      selangterpilih[1] <- NaN
      j <- 1
      k <- 1
      while(j < (length(FTSACO$selangbagian) - input$varFTSACO_step)){
        step11 <- runif(1, 0, 1)
        if(step11 <= 0.9){# proporsi peluang 0.9
          m <- j
          while(FTSACO$nilaipeluang[[ant]][j] < max(FTSACO$nilaipeluang[[ant]][m:(m+input$varFTSACO_step)]) && j < ( m + input$varFTSACO_step) ){
            j <- j + 1 
          }
          
        }else if(step11 > 0.9){ # proporsi peluang 0.1
          #j <- round(runif(1, j, (j + input$varFTSACO_step) ), digits = 0)
          j <- sample(j:(j+input$varFTSACO_step), 1)
        }
        
        FtsAcoselangtemp[[1]][k] <- FTSACO$selangbagian[j]
        selangterpilih[[1]][k] <- j
        j <- j + 1;
        k <- k + 1;
      }
      
      ## menghitung jumlah selang yang terbentuk dengan ACO
      banyakselangsemut <- 2 + length(selangterpilih[[1]])
      
      #memasukkan titik - titik yang dilewati semut
      FTSACOSelangSemut[[ant]] <- c(min_change, FtsAcoselangtemp[[1]], max_change)
      
      #memperbaharui selang keangotaan dari hasil ACO
      FTSACOframe.fenrollment_1.batasbawah <- FTSACOSelangSemut[[ant]][1:(banyakselangsemut-1)]
      FTSACOframe.fenrollment_1.batasatas  <- FTSACOSelangSemut[[ant]][2:banyakselangsemut]
      FTSACOframe.fenrollment_1.jmlanggota <- rep(0, (banyakselangsemut-1))
    }
    
    ####################### ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  #################################
    ##############################       batas akhir pencarian selang       #######################################
    ###############################################################################################################
    
    FTSACOframe.fenrollment_1.jmlanggota <- jmlAnggota(FTSACOframe.percentage_change[FTSACOgetFirstNumData() : (FTSACOgetFirstNumData()+FTSACOTrainingLength()-2)], FTSACOframe.fenrollment_1.batasbawah, FTSACOframe.fenrollment_1.batasatas)
    
    ###############################################################################################################
    ################## mencari jumlah anggota terbesar ke 1, 2, dan 3 untuk pembentukan interval baru #############
    ###############################################################################################################
    
    #terbesar pertama
    FTSACO$first <- NULL
    x1 <- max(FTSACOframe.fenrollment_1.jmlanggota)
    if(x1 > 0){
      fts_firstcount <- 1
      for(i in 1:(length(FTSACOframe.fenrollment_1.jmlanggota))){
        if(FTSACOframe.fenrollment_1.jmlanggota[[i]] == x1){
          FTSACO$first[[fts_firstcount]] <- i
          fts_firstcount <- fts_firstcount + 1
          FTSACOframe.fenrollment_1.jmlanggota[[i]] <- -1
        }
      }
    }
    
    #terbesar kedua
    FTSACO$second <- NULL
    x2 <- max(FTSACOframe.fenrollment_1.jmlanggota)
    if(x2>0){
      fts_secondcount <- 1
      for(i in 1:(length(FTSACOframe.fenrollment_1.jmlanggota))){
        if(FTSACOframe.fenrollment_1.jmlanggota[[i]] == x2){
          FTSACO$second[[fts_secondcount]] <- i
          fts_secondcount <- fts_secondcount + 1
          FTSACOframe.fenrollment_1.jmlanggota[[i]] <- -2
        }
      }
    }
    
    #terbesar ketiga
    FTSACO$third <- NULL
    x3 <- max(FTSACOframe.fenrollment_1.jmlanggota)
    if(x3>0){
      fts_thirdcount <- 1
      for(i in 1:(length(FTSACOframe.fenrollment_1.jmlanggota))){
        if(FTSACOframe.fenrollment_1.jmlanggota[[i]] == x3){
          FTSACO$third[[fts_thirdcount]] <- i
          fts_thirdcount <- fts_thirdcount + 1
          FTSACOframe.fenrollment_1.jmlanggota[[i]] <- -3
        }
      }
    }
    
    #pertambahan subinterval pada interval baru
    fts_countsuninter <- length(FTSACO$first) * 3 + length(FTSACO$second) * 2 + length(FTSACO$third)
    
    FTSACOframe.fenrollment_2.batasbawah <- NaN
    FTSACOframe.fenrollment_2.batasatas <- NaN
    FTSACOframe.fenrollment_2.jmlanggota <- NaN
    
    #interval baru untuk anggota terbanyak pertama
    if(length(FTSACO$first) > 0){
      for(i in 1:length(FTSACO$first)){
        min_change_first <- FTSACOframe.fenrollment_1.batasbawah[[FTSACO$first[[i]]]]
        size_interval_first <- (FTSACOframe.fenrollment_1.batasatas[[FTSACO$first[[i]]]] - FTSACOframe.fenrollment_1.batasbawah[[FTSACO$first[[i]]]])/4
        for(j in 1:4){
          FTSACOframe.fenrollment_2.batasbawah[[4 * (i-1) + j]] <- round(min_change_first + size_interval_first * (j-1), digits = 2)
          FTSACOframe.fenrollment_2.batasatas[[4 * (i-1) + j]] <- round(min_change_first + size_interval_first * (j), digits = 2)
        }
      }
    }
    
    #interval baru untuk anggota terbanyak kedua
    if(length(FTSACO$second) > 0){
      for(i in 1:length(FTSACO$second)){
        min_change_second <- FTSACOframe.fenrollment_1.batasbawah[[FTSACO$second[[i]]]]
        size_interval_second <- (FTSACOframe.fenrollment_1.batasatas[[FTSACO$second[[i]]]] - FTSACOframe.fenrollment_1.batasbawah[[FTSACO$second[[i]]]])/3
        for(j in 1:3){
          FTSACOframe.fenrollment_2.batasbawah[[(length(FTSACO$first) * 4) + 3 * (i-1) + j]] <- round(min_change_second + size_interval_second * (j-1), digits = 2)
          FTSACOframe.fenrollment_2.batasatas[[(length(FTSACO$first) * 4) + 3 * (i-1) + j]] <- round(min_change_second + size_interval_second * (j), digits = 2)
        }
      }
    }
    
    #interval baru untuk anggota terbanyak ketiga
    if(length(FTSACO$third) > 0){
      for(i in 1:length(FTSACO$third)){
        min_change_third <- FTSACOframe.fenrollment_1.batasbawah[[FTSACO$third[[i]]]] 
        size_interval_third <- (FTSACOframe.fenrollment_1.batasatas[[FTSACO$third[[i]]]] - FTSACOframe.fenrollment_1.batasbawah[[FTSACO$third[[i]]]])/2
        fts_mulaithird <- (length(FTSACO$first) * 4) + (length(FTSACO$second) * 3) + 2 * (i-1)
        for(j in 1:2){
          FTSACOframe.fenrollment_2.batasbawah[[fts_mulaithird + j]] <- round(min_change_third + size_interval_third * (j-1), digits = 2)
          FTSACOframe.fenrollment_2.batasatas[[fts_mulaithird + j]] <- round(min_change_third + size_interval_third * (j), digits = 2)
        }
      }
    }
    
    #memasukkan nilai yang tersisa di interval pertama (yang bukan terbanyak pertama, kedua atau ketiga)
    fts_selang_selanjutnya <- (length(FTSACO$first) * 4) + (length(FTSACO$second) * 3) + (length(FTSACO$third) * 2) + 1
    for(i in 1:(length(FTSACOframe.fenrollment_1.jmlanggota))){
      if(FTSACOframe.fenrollment_1.jmlanggota[[i]] > -1){
        FTSACOframe.fenrollment_2.batasbawah[[fts_selang_selanjutnya]] <- FTSACOframe.fenrollment_1.batasbawah[[i]]
        FTSACOframe.fenrollment_2.batasatas[[fts_selang_selanjutnya]] <- FTSACOframe.fenrollment_1.batasatas[[i]]
        fts_selang_selanjutnya <- fts_selang_selanjutnya + 1
      }
    }
    
    #mengurutkan nilai-nilai pada interval secara ascending
    FTSACOframe.fenrollment_2.batasbawah <- sort(FTSACOframe.fenrollment_2.batasbawah)
    FTSACOframe.fenrollment_2.batasatas <- sort(FTSACOframe.fenrollment_2.batasatas)
    FTSACOframe.fenrollment_2.jmlanggota <- rep(0, length(FTSACOframe.fenrollment_2.batasatas))
    
    FTSACOframe.fenrollment_2.jmlanggota <- jmlAnggota(FTSACOframe.percentage_change[FTSACOgetFirstNumData() : (FTSACOgetFirstNumData()+FTSACOTrainingLength()-2)], FTSACOframe.fenrollment_2.batasbawah, FTSACOframe.fenrollment_2.batasatas)
    
    #memasukkan interval terbentuk final ke db selang ACO
    FTSACO$selangACO$batasbawah[[ant]] <- FTSACOframe.fenrollment_2.batasbawah
    FTSACO$selangACO$batasatas[[ant]]  <- FTSACOframe.fenrollment_2.batasatas
    FTSACO$selangACO$jmlanggota[[ant]] <- FTSACOframe.fenrollment_2.jmlanggota
    
    ####################### ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  #################################
    ##########################    batas akhir pembentukan selang  yang baru    ####################################
    ###############################################################################################################
    
    FTSACO$fuzzyrelation[[ant]] <- FTSACOFuzzyset(FTSACOframe.percentage_change[ FTSACOgetFirstNumData() : (FTSACOgetFirstNumData()+FTSACOTrainingLength()-2)], FTSACO$selangACO$batasbawah[[ant]], FTSACO$selangACO$batasatas[[ant]], FTSACOgetindeksdata)
    
    ###############################################################################################################
    ################################# uji model yang sudah terbentuk ##############################################
    ###############################################################################################################
    
    #matriks untuk menyimpan nilai pengujian selang yang diperoleh dengan menggunakan ACO
    #dan untuk mendapatkan dan menyimpan nilai mse
    FTSACO$testingACO$persentrue[[1]] <- FTSACOframe.percentage_change[FTSACOgetFirstNumData()]
    FTSACO$testingACO$nilaiforc[[1]] <- FTSACOframe.datatrain[1]
    
    FTSACO$testingACO$persenforc <- FTSACOframe.percentage_change[FTSACOgetFirstNumData():(FTSACOgetFirstNumData() + FTSACOTrainingLength() - 1)]
    FTSACO$testingACO$nilaitrue <- FTSACOframe.datatrain[1:FTSACOTrainingLength()]
    
    #melakukan peramalan dengan selang yang terbentuk dengan menggunakan algoritma ACO
    for(i in 1:(FTSACOTrainingLength() - 1)){
      FTSACO$testingACO$persenforc[[i+1]] <- FTSACOgetValue(FTSACO$fuzzyrelation[[ant]][[i]], FTSACO$selangACO$batasbawah[[ant]], FTSACO$selangACO$batasatas[[ant]])
      FTSACO$testingACO$nilaiforc[[i+1]] <- ((FTSACO$testingACO$persenforc[[ i+1 ]] + 100) / 100) * FTSACO$testingACO$nilaitrue[[i]]
    }
    FTSACO$selangACO$MSE[[ant]] <- FTSACOgetMSE(FTSACO$testingACO$nilaitrue[-1], FTSACO$testingACO$nilaiforc[-1])
    FTSACO$selangACO$MAPE[[ant]] <- FTSACOgetMAPE(FTSACO$testingACO$nilaitrue[-1], FTSACO$testingACO$nilaiforc[-1])
    FTSACO$selangACO$BIC[[ant]] <- log(FTSACO$selangACO$MSE[[ant]]) + length(FTSACO$selangACO$batasbawah[[ant]]) * log(FTSACOTrainingLength()) / FTSACOTrainingLength()
    FTSACO$selangACO$nilaiforc[[ant]] <- FTSACO$testingACO$nilaiforc
    
    ####################### ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  #################################
    ##############################         akhir pengujian model      #############################################
    ###############################################################################################################
    
    ###############################################################################################################
    ####################### updating matriks feromones khusus algoritma ACO #######################################
    ###############################################################################################################
    
    if(input$algoritmaFTS_FTSACO == 'FTS - ACO'){
      if((ant + 1) <= jumlahsemut){
        FTSACO$nilaitho[[ ant+1 ]]     <- NaN 
        FTSACO$nilaipeluang[[ ant+1 ]] <- NaN
        FTSACO$nilaitho[[ ant+1 ]]         <- FTSACO$nilaitho[[ ant ]]
        FTSACO$nilaitho[[ ant+1 ]]         <- FTSACOUpdatePheromones(FTSACO$nilaitho[[ ant+1 ]], selangterpilih[[1]], tho0, FTSACO$selangACO$BIC[[ant]])
        FTSACO$nilaipeluang[[ ant+1 ]]     <- FTSACO$nilaitho[[ ant+1 ]]
        FTSACO$nilaipeluang[[ ant+1 ]]     <- FTSACOPeluangtitik(FTSACO$nilaitho[[ ant+1 ]])
      }
    }
    
    ####################### ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  ^  #################################
    ###############      akhir updating matriks feromones khusus algoritma ACO    #################################
    ###############################################################################################################
    
    cat("ant", ant, "of", jumlahsemut, " MSE : ", FTSACO$selangACO$MSE[[ant]], " MAPE : ", FTSACO$selangACO$MAPE[[ant]], "\n")
    flush.console()
  }
  
  
  if(input$varFTSACOBasedOn == "MSE" || input$algoritmaFTS_FTSACO == 'FTS'){
    FTSACOIndexTrainMin <- FTSACOgetindexminmse(FTSACO$selangACO$MSE)
  }else{
    FTSACOIndexTrainMin <- FTSACOgetindexminmse(FTSACO$selangACO$BIC)
  }
  
  thedataReturn <- list(
    FTSACOIndexTrainMin,
    FTSACO$selangACO$MSE[[ FTSACOIndexTrainMin ]],
    FTSACO$selangACO$MAPE[[ FTSACOIndexTrainMin ]],
    FTSACO$selangACO$BIC[[ FTSACOIndexTrainMin ]],
    FTSACO$selangACO$batasbawah[[ FTSACOIndexTrainMin ]],
    FTSACO$selangACO$batasatas[[ FTSACOIndexTrainMin ]],
    FTSACO$fuzzyrelation[[ FTSACOIndexTrainMin ]],
    FTSACO$selangACO$nilaiforc[[ FTSACOIndexTrainMin ]]
  )
  return(thedataReturn);
}

