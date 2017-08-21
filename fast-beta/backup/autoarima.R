output$uiArima_var1 <- renderUI({
  isTimeSeries <- "ts" == getdata_class_ts() | "mts" == getdata_class_ts()
  vars <- varnames_ts()[isTimeSeries]
  if(length(vars) ==  0) return()
  selectInput(inputId = "arima_var1", label = "Dependent variable:", choices = vars,
              selected = vars[1], multiple = FALSE)
})

# untuk memindahkan panel kembali ke data setiap berpindah menu
observe({
  if(!identical(input$nav_fast, "ARIMA")){
    updateTabsetPanel(session, "arimatab", selected = "datatab")
    nilaiTabel[["tmp"]] <- data.frame()
    values$modellist <- NULL
  }
})

output$uimenu_arima <- renderUI({
  wellPanel(
    HTML(paste("<label><strong>Menu:", "Forecasting","</strong></label>")),
    HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
    HTML(paste("<label><strong>Data:",input$datasets,"</strong></label>"))
    #textOutput(outputId = "namadata")
    # actionButton('commit', 'Commit Change')
  )
})

output$autoarima <- renderUI ({
  sidebarLayout(
    sidebarPanel(
      div(class = "busy",
          p("Calculation in progress ..."),
          img(src="ajaxloaderq.gif")
      ),
      uiOutput("uimenu_arima"),
      wellPanel(
        uiOutput("uiArima_var1")
      ),
      conditionalPanel(
        condition = "input.arimatab == 'datatab'",
        wellPanel(
          strong("Data attributes"),
          selectInput(
            inputId = "frequency",
            label = "Time period",
            choices = list(
              "Monthly" = 12,
              "Quartly" = 4,
              "Annual" = 1
            ),
            selected = 12),
          numericInput(inputId = "date",label = "Start Years", value = 2000, min = 1900)
        )
      ),
      conditionalPanel(
        condition = "input.arimatab == 2",
        wellPanel(
          strong("Specification"),br(),br(),
          wellPanel(
            checkboxInput(
              inputId = "dfdat",
              label = "Apply Differencing?",
              value = FALSE),
            conditionalPanel(
              condition = "input.dfdat == true",
              selectInput(
                inputId = "dfd",
                label = "",
                choices = list(
                  "First-Order" = 1,
                  "Second-Order" = 2),
                selected = 1))),
          conditionalPanel(
            condition = "input.subtab != 'historical' ",
            wellPanel(
              sliderInput(inputId = "confval", label = "Confidence Interval (%)", min = 0, max = 100, value = 95))),
          conditionalPanel(
            condition = "input.subtab == 1 || input.subtab == 2 ",
            wellPanel(
              numericInput(inputId = "lagdat", label = "Lags to include", value = 20, min = 1))),
          conditionalPanel(
            condition = "input.uniroottest == 'adf'  &&  (input.subtab == 4 )",
            wellPanel(
              numericInput(inputId = "lagdf", label = "Lagged differences", value = 10, min = 1)))
        )  
      ),
      conditionalPanel(
        condition = "input.arimatab == 3",
        
        wellPanel(
          selectInput(
            inputId = "methods",
            label = "Modelling Method:",
            choices = list(
              "Automatic Modelling" = 1,
              "Manual Modelling" =2 ),
            selected = 1)
        ),
        conditionalPanel(
          condition = "input.methods == 1",
          wellPanel(
            selectInput(
              inputId = "ic",
              label = "Information Criteria",
              choices = list(
                "aicc",
                "aic",
                "bic"),
              selected = "aicc"
            ),
            selectInput(
              inputId = "selRoot",
              label = "Unit root test",
              choices = list(
                "KPSS" = "kpss",
                "ADF" = "adf",
                "PP" = "pp"),
              selected = "kpss"
            ),
            selectInput(
              inputId = "seasonalTest",
              label = "Seasonal Test",
              choices = list(
                "ocsb",
                "ch"),
              selected = "ocsb"
            ),
            checkboxInput(
              inputId = "seasonal",
              label = "Seasonal Model",
              value = TRUE),
            checkboxInput(
              inputId = "stepwise",
              label = "Use Stepwise Selection",
              value = TRUE),
            checkboxInput(
              inputId = "allowDrift",
              label = "Allow drift in models",
              value = TRUE)
          )
        )
        ,
        conditionalPanel(
          condition = "input.methods == 2",
          wellPanel(
            numericInput(
              inputId = "ar",
              label = "Autoregressive",
              min = 0,
              value = 0),
            numericInput(
              inputId = "df",
              label = "Differencing",
              min = 0,
              value = 0),
            numericInput(
              inputId = "ma",
              label = "Moving Average",
              min = 0,
              value = 0),
            checkboxInput(
              inputId = "allowDriftman",
              label = "Allow drift in models",
              value = TRUE)
          )
          ,
          wellPanel(
            selectInput(
              inputId = "seasonalman",
              label = "Seasonal Model:",
              choices = list(
                "Non-Seasonal Model" = 1,
                "Seasonal Model" =2 ),
              selected = 1)
          ),
          conditionalPanel(
            condition = "input.seasonalman == 2",
            wellPanel(
              numericInput(
                inputId = "arseason",
                label = "P",
                min = 0,
                value = 0),
              numericInput(
                inputId = "dfseason",
                label = "D",
                min = 0,
                value = 0),
              numericInput(
                inputId = "maseason",
                label = "Q",
                min = 0,
                value = 0),
              numericInput(
                inputId = "periodman",
                label = "Period",
                min = 0,
                value = 12)
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.arimatab == 5 && input.forecasttab == 1",
        wellPanel(
          numericInput(
            inputId = "period",
            label = "Number Of Period",
            min = 1,
            max = input$frequency,
            value = 6))
      ),
      conditionalPanel(
        condition = "input.arimatab == 5 && input.forecasttab == 3",
        wellPanel(
          numericInput(inputId = "trainobservation", label = "Training-set Observation Size", value = 168, max = 175)
        )
      ),
      conditionalPanel(
        condition = "input.arimatab == 5",
        wellPanel(
          helpText(strong("Generate your report")),
          radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                       inline = TRUE),
          downloadButton("downloadReport")
        )
      ),
      helpAndReport('ARIMA','arima', inclMD("tools/help/arima.md"))
    ),
    mainPanel(
      tabsetPanel(
        id = "arimatab",
        tabPanel(
          title = "Data",
          #           tags$style(type='text/css', '#tampil_ts {background-color: rgB(240,248,255); color: green;}'), 
          tableOutput("tampil_ts"),
          value = "datatab"
        ),
        tabPanel(
          title = "Identification",
          #           helpText("Identify the model of the simulated data using the correlograms,
          #                  the", strong("Autocorrelation Function"), "(ACF) and the", 
          #                    strong("Partial Autocorrelation Function"), "(PACF).",
          #                    br(),br()),
          tabsetPanel(
            id = "subtab",
            tabPanel(
              title = "Historical Plot",
              withTags(
                bsCollapse(multiple = TRUE, open = c("plot1", "plot2"), id = "collapse1",
                           bsCollapsePanel("Series Plot", plotOutput(outputId = "arimaplot", height = "290px"),
                                           id = "plot1", value = "test1"),
                           bsCollapsePanel("Differenced Series Plot", helpText("Apply differencing to see the differenced series plot."),
                                           plotOutput(
                                             outputId = "h.new", height = "290px")
                                           , id = "plot2", value = "test2")
                )
              ),
              value = "historical" ),
            tabPanel(
              title = "Unit Root Test",
              withTags(
                bsCollapse(multiple = TRUE, open = c("uroot"), id = "collapse2",
                           bsCollapsePanel("", id = "uroot",
                                           selectInput(
                                             inputId = "uniroottest",
                                             label = strong("Please Select Unit Root Test :"),
                                             choices = c(
                                               "ADF" = "adf",
                                               "KPSS" = "kpss",
                                               "PP" = "pp"
                                             ),
                                             selected = "kpss"),
                                           verbatimTextOutput("textUnitRoot"),
                                           br(),br(),
                                           helpText(
                                             strong("H0:"),
                                             textOutput(outputId = "rooth0"),br(),
                                             strong("H1:"),
                                             textOutput(outputId = "rooth1"),br(),br(),
                                             strong("COMPUTATION:"),br(),
                                             "The computed p-value is:",
                                             textOutput(outputId = "rootpv"),br(),br(),
                                             strong("DECISION"), textOutput(outputId = "rootdc"),br(),br(),
                                             strong("CONCLUSION"), textOutput(outputId = "rootcn")
                                           )
                           )
                )
              ), value = 4
            ),
            tabPanel(
              title = " ACF",
              uiOutput("korelogram"),
              bsCollapse(
                multiple = TRUE, open = c("acfbs", "acfvalbs"), id = "acfcollapse",
                bsCollapsePanel("ACF Plot", id = "acfbs", plotOutput(outputId = "acf")),
                bsCollapsePanel("ACF Value", id = "acfvalbs", dataTableOutput(outputId = "acfval"))
              ),
              value = 1),
            tabPanel(
              title = "PACF",
              uiOutput("korelogram2"),
              bsCollapse(multiple = TRUE, open = c("pacfbs", "pacfvalbs"), id = "pacfcollapse",
                         bsCollapsePanel("PACF Plot", id = "pacfbs", plotOutput(outputId = "pacf")),
                         bsCollapsePanel("PACF Value", id = "pacfvalbs", dataTableOutput(outputId = "pacfval"))
              ),
              value = 2)),
          div(class = "busy",
              p("Calculation in progress ..."),
              img(src="ajaxloaderq.gif")
          ),
          value = 2),
        tabPanel(
          title = "Estimation",
          bsCollapse(multiple = TRUE, open = c("estimasisummary"), id = "collapseinterpretasi",
                     bsCollapsePanel("Summmary", id = "estimasisummary", 
                                     helpText("Estimate the identified model of the data.", br(), br()),
                                     htmlOutput("estmodelname"),
                                     tableOutput(
                                       outputId = "est"),
                                     #                                      verbatimTextOutput("est1"),
                                     htmlOutput(outputId = "sigma"),
                                     htmlOutput(outputId = "likelihood"),
                                     actionButton("addModel", "Add this model for comparison"),br(),br(),
                                     helpText("Choose criteria for selected the best model from table below :", br()),
                                     #verbatimTextOutput("modeldebug"),
                                     selectInput(
                                       inputId = "modelterbaik",
                                       label = "Best model criteria",
                                       choices = list(
                                         "Choose By System" = "bysystem",
                                         "aicc" = "aicc",
                                         "aic" = "aic",
                                         "bic" = "bic",
                                         "RMSE" = 2,
                                         "MAE" = 3,
                                         "MAPE" =5,
                                         "MASE" = 6
                                       ),
                                       selected = "bysystem"
                                     ),
                                     htmlOutput(outputId = "modeltable"),
                                     withTags(
                                       br())
                     ),
                     bsCollapsePanel("Interpretation",
                                     HTML("Akaike's Information Criterion (AIC), which was useful in selecting predictors for regression, is also useful for determining the order of an ARIMA model. It can be written as"),
                                     withMathJax("$$\\text{AIC} = -2 \\log(L) + 2(p+q+k+1) $$"),
                                     HTML("where L is the likelihood of the data, k=1 if c &#8800; 0 and k=0 if c=0. Note that the last term in parentheses is the number of parameters in the model (including &sigma;&sup2; , the variance of the residuals)."),
                                     HTML(paste0(br(),br(), "For ARIMA models, the corrected AIC can be written as")),
                                     withMathJax("$$\\text{AIC}_{\\text{c}} = \\text{AIC} + \\frac{2(p+q+k+1)(p+q+k+2)}{T-p-q-k-2}.$$"),
                                     HTML("and the Bayesian Information Criterion can be written as"),
                                     withMathJax("$$\\text{BIC} = \\text{AIC} + (\\log(T)-2)(p+q+k+1). $$"),
                                     HTML("Good models are obtained by minimizing either the AIC, AICc or BIC. Our preference is to use the AICc."),
                                     
                                     helpText(strong("Source:"),"from ",
                                              a("otext.org", 
                                                href="https://www.otexts.org/fpp/8/7", 
                                                target = "_blank")), 
                                     
                                     id = "interpretasi")
          ),
          div(class = "busy",
              p("Calculation in progress ..."),
              img(src="ajaxloaderq.gif")
          ),
          value = 3),
        tabPanel(
          title = "Diagnostic",
          helpText("Diagnose the model by testing the residuals for randomness."),
          br(),br(),
          withTags(
            div(
              class = "fluid-row",
              div(
                selectInput(
                  inputId = "test",
                  label = strong("PORTMANTEAU TEST"),
                  choices = c(
                    "Box-Pierce" = "Box",
                    "Ljung-Box" = "Ljung"),
                  selected = "Ljung"),
                br(),br(),
                helpText(
                  strong("H0:"),
                  "The data are independently distributed (i.e. the correlations in 
                       the population from which the sample is taken are 0, so that any 
                       observed correlations in the data result from randomness of the 
                       sampling process.",br(),
                  strong("H1:"),
                  "The data are not independently distributed.",br(),br(),br(),
                  strong("COMPUTATION:"),br(),
                  "The computed statistics is:",
                  textOutput(outputId = "diagx2"),
                  "The computed p-value is:",
                  textOutput(outputId = "diagpv"),br(),br(),
                  strong("DECISION:"),textOutput(outputId = "diagdc"),br(),br(),
                  strong("CONCLUSION:"),textOutput(outputId = "diagcn")
                ), class = "span5"),
              div(
                class = "span7",
                plotOutput(outputId = "resplot", height = "580px")))),br(),br(),
          value = 4),
#         tabPanel(
#           title = "Cross Validation",
#           plotOutput(outputId = "cvplot"),
#           value = 6
#           ),
        tabPanel(
          title = "Forecast",
          tabsetPanel(
            id = "forecasttab",
            tabPanel(
              title = "Forecast plot and value",
              bsCollapse(multiple = TRUE, open = c("collfcast", "collfval"), id = "collapsefor",
                         bsCollapsePanel("Forecast plot",plotOutput(outputId = "forecastplot"),
                                         id = "collfcast"),
                         bsCollapsePanel("Forecast Value", 
                                         verbatimTextOutput(outputId = "forecastvalue"),
                                         id = "collfval")
              ),
              value = 1),
            tabPanel(
              title = "Fitted plot",
              helpText("Obtain the predicted values of the model, and plot this with the
                 original data."),
              plotOutput(outputId = "fitplot"),
              helpText("Red line is showing the original data, and the blue line is the predicted values"),              
              value = 2),
            tabPanel(
              title = "Out-of sample forecast",
              bsCollapse(multiple = TRUE, open = c("oosplot", "oosacc", "oosfval"), id = "collapseout",
                         bsCollapsePanel("Out-of sample forecast plot", plotOutput(outputId = "outsampleplot"),
                                         id = "oosplot"),
                         bsCollapsePanel("Out-of sample accuracy",verbatimTextOutput("outsamplevalue"),
                                         id = "oosacc"),
                         bsCollapsePanel("Out-of sample forecast value",
                                         verbatimTextOutput("outsamplevalue1"),
                                         id = "oosfval"),
                         bsCollapsePanel("Out-sample data value", 
                                         verbatimTextOutput("testsetvalue"),
                                         id = "oosval")
              ),
              value = 3
            )
          ),
          value = 5
        )
        #         ,
        #         tabPanel(
        #           title = "Data Table",
        #           dataTableOutput(outputId = "data.table1"), 
        #           value = 6)
      )
    )
  )
})

# conversi data ke ts object dan mengambil variable yang dibutuhkan
getdata_ts <- reactive({
  if(is.null(input$frequency) || is.null(input$date) || is.null(input$arima_var1)) return()
  isolate({
    if(is.null(input$frequency) || is.null(input$date) || is.null(input$arima_var1)){
      dat <- ts(getdata(), frequency = nilaiTabel[["frequency"]], start=c(nilaiTabel[["start"]],1))
      #dat <- window(dat, end= c(2013,12))# tambahan 7 Agustus, untuk mengecek in sample
      
      return(dat[,as.character(input$arima_var1)])
    }else{
      dat <- ts(getdata(), frequency = as.numeric(input$frequency), start=c(input$date,1))
      
      return(dat[,as.character(input$arima_var1)])
    }
  })
})

get_data <- reactive({
  if(is.null(input$frequency) || is.null(input$date) || is.null(input$arima_var1) || is.null(input$datasets)) return()
  isolate({
    return (getdata()[,as.character(input$arima_var1)])
  })
})

# mengambil jumlah observasi
jumlahdata <- function(){
  isolate({
    return(length(get_data()))
  })
}

######################################################
## Bagian perhitungan
######################################################


# memberi tanggal pada dataset
dataset <- reactive({
  val <- as.numeric(get_data())
  date <- seq(as.Date("2009/01/1"), 
              as.Date(paste("2014/",06,"/1", sep = "")), 
              by = "1 months")
  data <- data.frame(Value = val, 
                     Date = date)
  return(data)
})

# membuat dataset menjadi data.frame agar bisa ditampilkan di dataTable format
dataframe <- reactive({
  #a <- as.matrix(sim.ts)
  Months <- c("Jan", "Feb", "Mar", "Apr", "May", 
              "Jun", "Jul", "Aug", "Sep", "Oct", 
              "Nov", "Dec")
  b <- rep(Months, floor(length(dataset()$Value)/12))
  c <- as.character(c(b, Months[1:(length(dataset()$Value) %% 12)]))
  aa <- data.frame(dataset()$Value,c)
  
  Jan <- Feb <- Mar <- Apr <- May <- Jun <- Jul <- Aug <- Sep <- Oct <- Nov <- Dec <- numeric()
  for(i in 1:nrow(aa)){
    if(aa[i,2] == "Jan"){
      Jan[i] <- aa[i,1]
    }
    if(aa[i,2] == "Feb"){
      Feb[i] <- aa[i,1]
    }
    if(aa[i,2] == "Mar"){
      Mar[i] <- aa[i,1]
    }
    if(aa[i,2] == "Apr"){
      Apr[i] <- aa[i,1]
    }
    if(aa[i,2] == "May"){
      May[i] <- aa[i,1]
    }
    if(aa[i,2] == "Jun"){
      Jun[i] <- aa[i,1]
    }
    if(aa[i,2] == "Jul"){
      Jul[i] <- aa[i,1]
    }
    if(aa[i,2] == "Aug"){
      Aug[i] <- aa[i,1]
    }
    if(aa[i,2] == "Sep"){
      Sep[i] <- aa[i,1]
    }
    if(aa[i,2] == "Oct"){
      Oct[i] <- aa[i,1]
    }
    if(aa[i,2] == "Nov"){
      Nov[i] <- aa[i,1]
    }
    if(aa[i,2] == "Dec"){
      Dec[i] <- aa[i,1]
    }
  }
  bb <- list("Jan" = Jan[complete.cases(Jan)],
             "Feb" = Feb[complete.cases(Feb)],
             "Mar" = Mar[complete.cases(Mar)],
             "Apr" = Apr[complete.cases(Apr)],
             "May" = May[complete.cases(May)],
             "Jun" = Jun[complete.cases(Jun)],
             "Jul" = Jul[complete.cases(Jul)],
             "Aug" = Aug[complete.cases(Aug)],
             "Sep" = Sep[complete.cases(Sep)],
             "Oct" = Oct[complete.cases(Oct)],
             "Nov" = Nov[complete.cases(Nov)],
             "Dec" = Dec[complete.cases(Dec)])
  for(i in 1:length(bb)){
    if(max(sapply(bb, length)) > length(bb[[i]])){
      bb[[i]] <- c(bb[[i]],NA)
    }
  }
  
  kk <- as.data.frame(bb)
  cc <- numeric()
  for(i in 1:6){
    cc[i] <- 2008 + i
  }
  kk <- data.frame(Year = cc, kk)
  return(kk)
})

output$data.table1 <- renderDataTable({
  dataframe()
})

# menampilkan data time series awal
output$tampil_ts <- renderTable({
  if(is.null(input$arima_var1) || is.null(input$frequency) || is.null(input$date))  return ()
  isolate({
    mulai <- as.numeric(input$date)
    nilaiTabel[["start"]] <- mulai
    nilaiTabel[["frequency"]] <- as.numeric(input$frequency)
    message <- ts(data = get_data(), frequency = as.numeric(input$frequency), start = c(input$date,1))
    message
  })
}, digits = 4)


# menampilkan training set
output$trainingSet <- renderPrint({
  window(getdata_ts(), end = c(input$endYear, input$endMonth))
})

# menampilkan test set
output$testSet <- renderPrint({
  window(getdata_ts(), start = c(input$endYear, input$endMonth+1))
})

# plot data awal
output$arimaplot <- renderPlot({
  par(mfcol = c(1,1), mar = c(5,4,1,2))
  data <- getdata_ts()
  plot(data)
})


# hipotesis 0 dari unit root
output$rooth0 <- renderText({
  if(input$uniroottest =="kpss"){
    print("The data is stationary and doesn't need to be differenced.")
  }
  else {
    print("The data needs to be differenced to make it stationary.")
  }
})

# hipotesis 1 dari unit root
output$rooth1 <- renderText({
  if(input$uniroottest =="kpss"){
    "The data needs to be differenced to make it stationary."
  }
  else {
    "The data is stationary and doesn't need to be differenced."
  }
})

# menghitung nilai trunc/lag adf
observe({
  if(is.null(input$uniroottest) || is.null(input$arima_var1)) return()
  isolate({
    k <- trunc((length(getdata_ts())-1)^(1/3))
    updateNumericInput(session, inputId = "lagdf", value = k) #(length(getdata_ts())-1)^(1/3)
  })
})

# menampilkan pilihan jumlah traning set sesuai dengan
observe({
  if(identical(input$nav_fast, "ARIMA")){
    k <- length(getdata_ts())
    frequency <- as.numeric(input$frequency)
    lag.max <- floor(10 * (log10(k) - log10(1)))
    updateNumericInput(session, inputId = "trainobservation", value = k-frequency, max = k-1)
    updateNumericInput(session, inputId = "lagdat", value = lag.max, max = k)
  }
})

# menampilkan hasil uji unit root
output$textUnitRoot <- renderPrint({
  series <- getdata_ts()
  if(input$uniroottest =="kpss"){
    dodiff <- tseries::kpss.test(series)
    if(input$dfdat){
      differencedseries <- diff(series, differences = as.numeric(input$dfd))
      dodiff <- tseries::kpss.test(differencedseries)
    }}
  
  else if(input$uniroottest =="adf"){
    dodiff <- tseries::adf.test(series, k = input$lagdf)
    if(input$dfdat){
      differencedseries <- diff(series, differences = as.numeric(input$dfd))
      dodiff <- tseries::adf.test(differencedseries, k = input$lagdf)
    }}
  
  else if(input$uniroottest =="pp"){
    dodiff <- tseries::pp.test(series)
    if(input$dfdat){
      differencedseries <- diff(series, differences = as.numeric(input$dfd))
      dodiff <- tseries::pp.test(differencedseries)
    }}
  print(dodiff)
})

# nilai p-value dari unit root test
# menampilkan hasil uji unit root
output$rootpv <- renderPrint({
  if(input$uniroottest =="kpss"){
    dodiff <- tseries::kpss.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::kpss.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }}
  
  else if(input$uniroottest =="adf"){
    dodiff <- tseries::adf.test(getdata_ts(), k = input$lagdf)
    if(input$dfdat){
      dodiff <- tseries::adf.test(diff(getdata_ts(), differences = as.numeric(input$dfd)), k = input$lagdf)
    }}
  
  else if(input$uniroottest =="pp"){
    dodiff <- tseries::pp.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::pp.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }}
  cat(dodiff$p.value)
})

# keputusan dari uji unit root (decision)
output$rootdc <- renderPrint({
  if(input$uniroottest =="kpss"){
    dodiff <- tseries::kpss.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::kpss.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }}
  
  else if(input$uniroottest =="adf"){
    dodiff <- tseries::adf.test(getdata_ts(), k = input$lagdf)
    if(input$dfdat){
      dodiff <- tseries::adf.test(diff(getdata_ts(), differences = as.numeric(input$dfd)), k = input$lagdf)
    }}
  
  else if(input$uniroottest =="pp"){
    dodiff <- tseries::pp.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::pp.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }}
  
  if(dodiff$p.value >= (1-input$confval/100)){
    cat(paste0("Do not reject the null hypothesis, since the p-value is greater than ", (1-input$confval/100))) 
  }
  if(dodiff$p.value < (1-input$confval/100)){
    cat(paste0("Reject the null hypothesis, since the p-value is less than ", (1-input$confval/100))) 
  }
})

# kesimpulan dari uji unit root (conclusion)
output$rootcn <- renderPrint({
  
  if(input$uniroottest =="kpss"){
    dodiff <- tseries::kpss.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::kpss.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }
    
    if(dodiff$p.value >= (1-input$confval/100)){
      cat("Therefore, we do not have enough evidence to reject the null hypothesis. And the data is Stationary")
    }
    if(dodiff$p.value < (1-input$confval/100)){
      cat("Therefore, we reject the null hypothesis. And the data is not Stationary") 
    }}
  
  else if(input$uniroottest =="adf"){
    dodiff <- tseries::adf.test(getdata_ts(), k = input$lagdf)
    if(input$dfdat){
      dodiff <- tseries::adf.test(diff(getdata_ts(), differences = as.numeric(input$dfd)), k = input$lagdf)
    }
    
    if(dodiff$p.value >= (1-input$confval/100)){
      cat("Therefore, we do not have enough evidence to reject the null hypothesis. And the data is not Stationary")
    }
    if(dodiff$p.value < (1-input$confval/100)){
      cat("Therefore, we reject the null hypothesis. And the data is Stationary") 
    }}
  
  else if(input$uniroottest =="pp"){
    dodiff <- tseries::pp.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::pp.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }
    
    if(dodiff$p.value >= (1-input$confval/100)){
      cat("Therefore, we do not have enough evidence to reject the null hypothesis. And the data is not Stationary")
    }
    if(dodiff$p.value < (1-input$confval/100)){
      cat("Therefore, we reject the null hypothesis. And the data is Stationary") 
    }}
  
})

# menampilkan jumlah sample dan included observation
output$korelogram <- renderUI({
  n <- length(get_data())
  obs <- n
  if(input$dfdat){
    d <- diff(get_data(), lag = 1, conf.level = (input$confval/100), differences = as.numeric(input$dfd))
    obs <- length(d)
  }
  withTags(
    div(
      helpText(
        strong("Sample : "), n , br(),		  
        strong("Included observations : "), obs ))  	  
  )
})

output$korelogram2 <- renderUI({
  n <- length(get_data())
  obs <- n
  if(input$dfdat){
    d <- diff(get_data(), lag = 1, conf.level = (input$confval/100), differences = as.numeric(input$dfd))
    obs <- length(d)
  }
  withTags(
    div(
      helpText(
        strong("Sample : "), n , br(),  	  
        strong("Included observations : "), obs ))  	  
  )
})

# plot acf
output$acf <- renderPlot({
  p <- qacf(get_data(), conf.level = (input$confval/100), max.lag = input$lagdat)
  if(input$dfdat){
    d <- diff(get_data(), lag = 1, conf.level = (input$confval/100), differences = as.numeric(input$dfd))
    p <- qacf(d, max.lag = input$lagdat)
  }
  print(p) 
})


# nilai acf
output$acfval <- renderDataTable({
  d <- get_data()
  p <- acf(get_data(), lag.max = input$lagdat, conf.level = (input$confval/100),plot = FALSE )
  if(input$dfdat){
    d <- diff(get_data(), lag = 1, differences = as.numeric(input$dfd))
    p <- acf(d, lag.max  = input$lagdat, conf.level = (input$confval/100), plot = FALSE)
  }
  
  p <- data.frame(p$lag,p$acf)
  colnames(p) <- c("Lag", "ACF Value")
  
  # membuang nilai observasi 1
  p <- p[-1,]
  
  # mencoba membuat perhitungan ljung box untuk setiap lag
  
  # array untuk menyimpan nilai ljung-box
  lj <- array(NA, c(input$lagdat,2))
  colnames(lj) <- c("Box-Ljung", "P-value")
  
  # fungsi mengisi nilai ljung-box
  for(i in 1:input$lagdat){
    lj[i,1] <- Box.test(d, lag = i, type = "Ljung-Box")$statistic
    lj[i,2] <- Box.test(d, lag = i, type = "Ljung-Box")$p.value
  }
  
  # menggabungkan nilai lag,acf,ljung-box, dan pvalue
  p <- cbind(p,lj)
  
})

# plot pacf
output$pacf <- renderPlot({
  p <- qpacf(get_data(), conf.level = (input$confval/100), max.lag = input$lagdat)
  if(input$dfdat){
    d <- diff(get_data(), lag = 1, conf.level = (input$confval/100), differences = as.numeric(input$dfd))
    p <- qpacf(d, max.lag = input$lagdat)
  }
  print(p)
})

# nilai pacf
output$pacfval <- renderDataTable({
  d <- get_data()
  p <- pacf(get_data(), lag.max = input$lagdat, conf.level = (input$confval/100),plot = FALSE )
  if(input$dfdat){
    d <- diff(get_data(), lag = 1, differences = as.numeric(input$dfd))
    p <- pacf(d, lag.max = input$lagdat, conf.level = (input$confval/100), plot = FALSE)
  }
  p <- data.frame(p$lag,p$acf)
  colnames(p) <- c("Lag", "PACF Value")
  
  # mencoba membuat perhitungan ljung box untuk setiap lag
  
  # array untuk menyimpan nilai ljung-box
  lj <- array(NA, c(input$lagdat,2))
  colnames(lj) <- c("Box-Ljung", "P-value")
  
  # fungsi mengisi nilai ljung-box
  for(i in 1:input$lagdat){
    lj[i,1] <- Box.test(d, lag = i, type = "Ljung-Box")$statistic
    lj[i,2] <- Box.test(d, lag = i, type = "Ljung-Box")$p.value
  }
  
  # menggabungkan nilai lag,acf,ljung-box, dan pvalue
  p <- cbind(p,lj)
})

# plot setelah dilakukan differencing
output$h.new <- renderPlot({
  if(input$dfdat){
    data <- diff(getdata_ts(), differences = as.numeric(input$dfd))
    par(mfcol = c(1,1), mar = c(5,4,1,2))
    p <- plot(data)
    print(p)
  }
})

#estimasi menggunakan auto.arima
estimate.auto <- reactive({
  #if(is.null(input$methods) || input$methods == 0) return()
  input$methods
  #isolate({
  if(as.numeric(input$methods) == 1){
    k <- auto.arima(getdata_ts(), ic = input$ic, seasonal = input$seasonal,stepwise = input$stepwise, 
                    test = input$selRoot, seasonal.test = input$seasonalTest, allowdrift = input$allowDrift)
  }
  if(as.numeric(input$methods) == 2){
    if(as.numeric(input$seasonalman) == 2){
      k <- Arima(getdata_ts(), order = c(input$ar, input$df, input$ma), seasonal = list(order = c(input$arseason, input$dfseason, input$maseason), period = input$periodman), include.drift = input$allowDriftman)
    }else{
      k <- Arima(getdata_ts(), order = c(input$ar, input$df, input$ma), include.drift = input$allowDriftman)}
  }
  
  return(k)
  #})
})

# menampilkan hasil estimasi
output$est <- renderTable({
  xtablest(estimate.auto())
}, digits=6)

#untuk debugging
output$est1 <- renderPrint({
  estimate.auto()
})

# menampilkan hasil estimasi dalam bentuk yang lebih rapi
xtablest <- function(fit){
  # mengekstrak estimasi parameter secara otomatis
  l <- length(fit$coef)
  
  order <- fit$arma[c(1,6,2,3,7,4,5)]
  npara <- order[1] + order[2] + order[3] + order[4] + order[5] + order[6]
  nobs <- length(fit$x)
  ndf <- nobs-npara
  
  # matriks nilai estimasi parameter
  matestimasi <- matrix(NA,l,1 )
  
  for(i in 1:l){
    matestimasi[i, 1] <- fit$coef[i]
    if(i == 1){
      rownames <- names(fit$coef[i])
    }else{
      rownames <- c(rownames, names(fit$coef[i]))
    }
  }
  #memberi nama untuk row
  rownames(matestimasi) <- rownames
  matrownames <- matrix(rownames)
  
  #standar error dari parameter
  standarderror <- lapply(diag(fit$var.coef), sqrt)
  
  #matriks standard error
  matstd <- matrix(standarderror)
  
  matT <- matrix(NA,l,1)
  matF <- matrix(NA,l,1)
  
  for(i in 1:l){
    matT[i,1] <- (as.numeric(matestimasi[i,1])/as.numeric(matstd[i,1]))
    matF[i,1] <- 1-pf((matT[i,1])^2,1, ndf)
  }
  
  # data frame untuk ditampilkan xtable
  result <- data.frame(matrownames, matestimasi, matstd , matT, matF)
  
  # memberi nama kolom dari dataframe
  colnames(result) <- c("Parameter", "Estimate", "Std. Error", "t-Statistic", "Prob.")
  rownames(result) <- NULL
  return(result)
}

# memberi nama model
output$estmodelname <- renderText({
  HTML( paste0(strong(getarimanames(estimate.auto())), "<br></br>") )
})

# menampilan nilai sigma2
output$sigma <- renderText({
  fit <- estimate.auto()
  paste0(HTML("<p align=left> sigma^2 estimated as : <strong>"), fit$sigma2, HTML("</strong></p>"))
})

# menampilkan likelihood
output$likelihood <- renderText({
  fit <- estimate.auto()
  print(paste0(HTML("<p align=left> log likelihood = <strong>"), fit$loglik, HTML("</strong></p>")))
})

# fungsi untuk mengambil nama model arima
getarimanames <- function(fit){  
  order <- fit$arma[c(1,6,2,3,7,4,5)]
  result <- paste("ARIMA(",order[1],",",order[2],",",order[3],")",sep="")
  if(order[7]>1 & sum(order[4:6]) > 0)
    result <- paste(result,"(",order[4],",",order[5],",",order[6],")[",order[7],"]",sep="")
  if(is.element("constant",names(fit$coef)) | is.element("intercept",names(fit$coef)))
    result <- paste(result,"with non-zero mean")
  else if(is.element("drift",names(fit$coef)))
    result <- paste(result,"with drift        ")
  else if(order[2]==0 & order[5]==0)
    result <- paste(result,"with zero mean    ")
  else
    result <- paste(result,"                  ")
  return(result)
}


# fungsi untuk mengolah data untuk ditampilkan di tabel
olahdata <- reactive({
  arrayterbaik()
  for(i in 1:length(values$modellist)){
    fit <- values$modellist[[i]]
    sum <- summary(fit)
    order <- fit$arma[c(1,6,2,3,7,4,5)]
    result <- paste("ARIMA(",order[1],",",order[2],",",order[3],")",sep="")
    if(order[7]>1 & sum(order[4:6]) > 0)
      result <- paste(result,"(",order[4],",",order[5],",",order[6],")[",order[7],"]",sep="")
    if(is.element("constant",names(fit$coef)) | is.element("intercept",names(fit$coef)))
      result <- paste(result,"with non-zero mean")
    else if(is.element("drift",names(fit$coef)))
      result <- paste(result,"with drift        ")
    else if(order[2]==0 & order[5]==0)
      result <- paste(result,"with zero mean    ")
    else
      result <- paste(result,"                  ")
    
    # mengambil nilai aic, aicc, bic, ME, RMSE, MAE , MPE, MAPE, MASE
    data <- data.frame(result, fit$aic, fit$aicc, fit$bic, sum[2], sum[3], sum[5], sum[6])
    colnames(data) <- c("ARIMA MODEL", "AIC", "AICc", "BIC", "RMSE", "MAE", "MAPE", "MASE")
    
    nilaiTabel[["tmp"]] <- unique(rbind(nilaiTabel[["tmp"]], data))
    
    colnames(nilaiTabel[["tmp"]]) <- c("ARIMA MODEL", "AIC", "AICc", "BIC", "RMSE", "MAE", "MAPE", "MASE")
  } 
})

# mengisi data ke array penampung model terbaik
arrayterbaik <- reactive({
  
  if(length(values$modellist ) == 0){
    values$modellist[[1]] <- estimate.auto()
    values$modellist[[2]] <- auto.arima(getdata_ts(), seasonal = F, stepwise = F)
  }else{
    values$modellist[[length(values$modellist)+1]] <- estimate.auto()
  } 
  
})

# membersihkan semua array jika diload data baru
observe ({
  if(is.null(input$datasets)) return()
  input$datasets
  isolate({
    nilaiTabel[["tmp"]] <- data.frame()
    values$modellist <- NULL
  })
})


# menampilkan tabel dari beberapa model 
output$modeltable <- renderGvis({
  if(length(nilaiTabel[["tmp"]]) == 0){
    olahdata()
    gvisTable(nilaiTabel[["tmp"]], options = list(width = 1000, height = '250px', showRowNumber = T))
  }
  if(is.null(input$addModel)) return()
  isolate({
    olahdata()
    gvisTable(nilaiTabel[["tmp"]], options = list(width = 1000, height = '250px', showRowNumber = T))
  })
})

#menampilkan model hanya untuk debugging
output$modeldebug <- renderPrint({
  if(length(values[["modellist"]]) == 0 || is.null(values[["modellist"]])) return()
  terbaik()
})

# memilih model terbaik berdasarkan kriteria tertentu
terbaik <- reactive({  
  if(length(values[["modellist"]]) == 0 || is.null(values[["modellist"]])) return(estimate.auto())
  kriteria = as.character(input$modelterbaik)
  if(kriteria == "aic" || kriteria == "aicc" || kriteria == "bic"){
    model = values$modellist[[1]]
    min <- as.numeric(values$modellist[[1]][kriteria]) 
    for(i in 1:length(values$modellist)){
      now = as.numeric(values$modellist[[i]][kriteria])
      if(now < min){
        model = values$modellist[[i]] 
        min = now
      }
    }
  }
  # menghitung semua kemungkinan model terbaik dari fungsi auto.arima
  else if(kriteria == "bysystem"){
    model = values$modellist[[1]]
    min = bantuterbaik(fit = model)
    for(i in 1:length(values$modellist)){
      now = bantuterbaik(fit = values$modellist[[i]])
      if(now < min){
        model = values$modellist[[i]] 
        min = now
      }
    }
  }
  else{
    model = values$modellist[[1]]
    summodel = summary(values$modellist[[1]])
    min <- summodel[as.numeric(input$modelterbaik)]
    for(i in 1:length(values$modellist)){
      bantu = summary(values$modellist[[i]])
      now = bantu[as.numeric(input$modelterbaik)]
      if (now < min){
        model = values$modellist[[i]]
        min = now
      }
    }
  }
  return(model)
})

# sortting daftar model terbaik
sort.terbaik <- reactive({  
  kriteria = as.character(input$modelterbaik)
  n <- length(values$modellist)
  
  if(kriteria == "aic" || kriteria == "aicc" || kriteria == "bic"){
    no_passes = 0
    while(1) {
      no_swaps = 0
      for (j in 1 : (n - 1 - no_passes)) {
        if(as.numeric(values$modellist[[j]][kriteria]) > as.numeric(values$modellist[[j+1]][kriteria])){
          swap = values$modellist[[j]] 
          values$modellist[[j]] = values$modellist[[j+1]]
          values$modellist[[j+1]] = swap
          no_swaps = no_swaps + 1
        }
      }
      no_passes = no_passes + 1
      if(no_swaps == 0) break
    }
  }
  # menghitung semua kemungkinan model terbaik dari fungsi auto.arima
  else if(kriteria == "bysystem"){
    no_passes = 0
    while(1) {
      no_swaps = 0
      for (j in 1 : (n - 1 - no_passes)) {
        if(bantuterbaik(fit = values$modellist[[j]]) > bantuterbaik(fit = values$modellist[[j+1]])){
          swap = values$modellist[[j]] 
          values$modellist[[j]] = values$modellist[[j+1]]
          values$modellist[[j+1]] = swap
          no_swaps = no_swaps + 1
        }
      }
      no_passes = no_passes + 1
      if(no_swaps == 0) break
    }
  }
  else{
    no_passes = 0
    while(1) {
      no_swaps = 0
      for (j in 1 : (n - 1 - no_passes)) {
        if(summary(values$modellist[[j]])[as.numeric(input$modelterbaik)] > summary(values$modellist[[j+1]])[as.numeric(input$modelterbaik)]){
          swap = values$modellist[[j]] 
          values$modellist[[j]] = values$modellist[[j+1]]
          values$modellist[[j+1]] = swap
          no_swaps = no_swaps + 1
        }
      }
      no_passes = no_passes + 1
      if(no_swaps == 0) break
    }
  }
  
})


# membantu menghitung out of sample sebagai kriteria model terbaik dari sistem
bantuterbaik <- function(fit = NULL)({
  
  train <- gettrainingset()
  
  hasil <- create.arima(fit, train)
  
  testset <- gettestset()
  
  fcastout <- forecast(hasil, length(testset))
  
  acc <- accuracy(fcastout, testset)[2,8]
  return(acc)
  
})

# buat arima model dari model sebelumnya
create.arima <- function(fit = NULL, train = null){
  
  hasil = 0
  seasonal = FALSE
  nonzeromean = TRUE
  withdrift = FALSE
  
  order <- fit$arma[c(1,6,2,3,7,4,5)]
  if(order[7]>1 & sum(order[4:6]) > 0){    
    seasonal = TRUE 
  }
  if(is.element("constant",names(fit$coef)) | is.element("intercept",names(fit$coef))){
    nonzeromean = TRUE
  }
  else if(is.element("drift",names(fit$coef))){
    withdrift = TRUE
  }
  else if(order[2]==0 & order[5]==0){
    nonzeromean = FALSE
  }
  else{
    
  }
  
  if(seasonal == TRUE){
    hasil <- Arima(train, order = c(order[1],order[2], order[3]),seasonal = c(order[4],order[5],order[6]), include.drift = withdrift, include.mean = nonzeromean)
  }else{
    hasil <- Arima(train, order = c(order[1],order[2], order[3]), include.drift = withdrift, include.mean = nonzeromean)
  }
  
  return(hasil)
}

# hasil uji dalam bentuk statistik
output$diagx2 <- renderPrint({
  bestmodel_Residual <- terbaik()$residuals
  names(bestmodel_Residual) <- "ini contoh"
  m <- Box.test(bestmodel_Residual, type = input$test)
  cat(as.numeric(as.matrix(m$statistic)))
})

# hasil uji dalam bentuk p-value
output$diagpv <- renderPrint({
  m <- Box.test(terbaik()$residuals, type = input$test)
  cat(as.numeric(as.matrix(m$p.value)))
})

# keputusan dari uji diagnostic (decision)
output$diagdc <- renderPrint({
  m <- Box.test(terbaik()$residuals, type = input$test)
  j <- as.numeric(as.matrix(m$p.value))
  if(j >= 0.05){
    cat("Do not reject the null hypothesis, since the p-value is greater than 0.05")
  }
  if(j < 0.05){
    cat("Reject the null hypothesis, since the p-value is less than 0.05")
  }
})

# kesimpulan dari uji diagnostic (conclusion)
output$diagcn <- renderPrint({
  m <- Box.test(terbaik()$residuals, type = input$test)
  j <- as.numeric(as.matrix(m$p.value))
  if(j >= 0.05){
    cat("Therefore, we do not have enough evidence to reject the null hypothesis. And thus, the residuals of the model exhibits randomness")
  }
  if(j < 0.05){
    cat("Therefore, the residuals of the model are not independently distributed.")
  }
})

# plot residual
output$resplot <- renderPlot({
  p <- tsdisplay(terbaik()$residuals)
  print(p)
})

# output nilai peramalan terhadap fitted value
output$fitplot <- renderPlot({
  fit <- terbaik()
  value <- fit$x
  plot(value,col="red")
  lines(fitted.Arima(fit),col="blue")
})

# perhitungan out-of sample
fcastout <- reactive({  
  
  train <- gettrainingset()
  
  hasil = 0
  seasonal = FALSE
  nonzeromean = TRUE
  withdrift = FALSE
  
  fit <- terbaik()
  order <- fit$arma[c(1,6,2,3,7,4,5)]
  result <- paste("ARIMA(",order[1],",",order[2],",",order[3],")",sep="")
  if(order[7]>1 & sum(order[4:6]) > 0){    
    result <- paste(result,"(",order[4],",",order[5],",",order[6],")[",order[7],"]",sep="")
    seasonal = TRUE 
  }
  if(is.element("constant",names(fit$coef)) | is.element("intercept",names(fit$coef))){
    result <- paste(result,"with non-zero mean")
    nonzeromean = TRUE
  }
  else if(is.element("drift",names(fit$coef))){
    result <- paste(result,"with drift        ")
    withdrift = TRUE
  }
  else if(order[2]==0 & order[5]==0){
    result <- paste(result,"with zero mean    ")
    nonzeromean = FALSE
  }
  else{
    result <- paste(result,"                  ")
  }
  
  if(seasonal == TRUE){
    hasil <- Arima(train, order = c(order[1],order[2], order[3]),seasonal = c(order[4],order[5],order[6]), include.drift = withdrift, include.mean = nonzeromean)
  }else{
    hasil <- Arima(train, order = c(order[1],order[2], order[3]), include.drift = withdrift, include.mean = nonzeromean)
  }
  
  testset <- gettestset()
  
  return(train_forecast <- forecast(hasil, h  = length(testset)))
  
})

#mengambil nilai test set
gettestset <- reactive({
  if(is.null(input$trainobservation)){
    testset <- as.zoo(getdata_ts())
    n <- length(testset)
    frequency <- as.numeric(input$frequency)
    testset <- tail(testset,  n = frequency)
    testset <- as.ts(testset)
  }else{
    testset <- as.zoo(getdata_ts())
    n <- length(testset)
    testset <- tail(testset,  n = n - input$trainobservation)
    testset <- as.ts(testset)
  }
  #   testset <- window(getdata_ts(), start = c(2014, 1))
  return(testset)
})

#mengambil nilai traning set
gettrainingset <- reactive({
  if(is.null(input$trainobservation)){
    trainingset <- as.zoo(getdata_ts())
    n <- length(trainingset)
    frequency <- as.numeric(input$frequency) 
    trainingset <- head(trainingset, n = n-frequency)
    trainingset <- as.ts(trainingset)
  }else{
    trainingset <- as.zoo(getdata_ts())
    n <- length(trainingset)
    trainingset <- head(trainingset, n = input$trainobservation)
    trainingset <- as.ts(trainingset)
  }
  #   traningset <- window(getdata_ts(), end = c(2013, 12))
  return(trainingset)
})

# output out of sample forecast
output$outsampleplot <- renderPlot({
  
  p <- plot(fcastout(), include = 0)
  lines(gettestset(), col = "red")
  
})

# output nilai akurasi outsample forecast
output$outsamplevalue <- renderPrint({
  testset <- gettestset()
  accuracy(fcastout(), testset)[2,]
})

# output nilai forecast outofsample
output$outsamplevalue1 <- renderPrint({
  p <- fcastout()
  print(p)
})

# menampilkan nilai test set
output$testsetvalue <- renderPrint({
  gettestset()
})

# output plot forecast
output$forecastplot <- renderPlot({
  if(is.null(terbaik())){
    forecast1 <- forecast.Arima(estimate.auto(), h=input$period)  
  }
  else{
    forecast1 <- forecast.Arima(terbaik(), h=input$period)
  }
  p <- plot.forecast(forecast1)
})

# output nilai forecast
output$forecastvalue <- renderPrint({
  if(is.null(terbaik())){
    forecast1 <- forecast.Arima(estimate.auto(), h=input$period)  
  }
  else{
    forecast1 <- forecast.Arima(terbaik(), h=input$period)
  }
  forecast1
})

#######################################################
# fungsi untuk plot acf dan pacf menggunakan ggplot2

qacf <- function(x, conf.level = 0.95, max.lag = NULL,
                 min.lag = 1) {
  ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
  bacf <- Acf(x, plot = FALSE, lag.max = max.lag)
  bacfdf <- with(bacf, data.frame(lag, acf))
  if (min.lag > 0) {
    bacfdf <- bacfdf[-seq(1, min.lag), ]
  }
  significant <- (abs(bacfdf[, 2]) > abs(ciline))^2
  bacfdf <- cbind(bacfdf, significant)
  q <- qplot(
    lag, acf, data = bacfdf, geom = "bar",
    stat = "identity", position = "identity",
    ylab = "Autocorrelation",
    fill = factor(significant))
  q <- q + geom_hline(
    yintercept = -ciline,
    color = "blue", size = 0.2, linetype="dashed")
  q <- q + geom_hline(
    yintercept = ciline,
    color = "blue", size = 0.2, linetype="dashed")
  q <- q + geom_hline(
    yintercept = 0, color = "red",
    size = 0.3)
  q <- q + scale_fill_hue(
    name = paste("Significant at the", conf.level, "level"),
    breaks = 0:1,
    labels = c("Not Significant", "Significant")) +
    theme(panel.background = element_rect(
      size = 3, 
      colour = "black",
      fill = "white"),
      axis.ticks = element_line(
        size = 2),
      axis.title.x = element_text(
        size = rel(1.2), 
        face = "bold"),
      axis.title.y = element_text(
        size = rel(1.2), 
        face = "bold"),
      plot.title = element_text(
        size = 20,
        face = "bold", 
        vjust = 1.5),
      legend.position = "bottom",
      legend.title = element_text(
        size=rel(1.2), 
        face="bold"),
      legend.text = element_text(
        colour="blue", 
        size = 13))
  return(q)
}

qpacf <- function(x, conf.level = 0.95, max.lag = NULL,
                  min.lag = 0) {
  ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
  bacf <- pacf(x, plot = FALSE, lag.max = max.lag)
  bacfdf <- with(bacf, data.frame(lag, acf))
  if (min.lag > 0) {
    bacfdf <- bacfdf[-seq(1, min.lag), ]
  }
  significant <- (abs(bacfdf[, 2]) > abs(ciline))^2
  bacfdf <- cbind(bacfdf, significant)
  q <- qplot(
    lag, acf, data = bacfdf, geom = "bar",
    stat = "identity", position = "identity",
    ylab = "Partial Correlation",
    fill = factor(significant))
  q <- q + geom_hline(
    yintercept = -ciline,
    color = "blue", size = 0.2, linetype="dashed")
  q <- q + geom_hline(
    yintercept = ciline,
    color = "blue", size = 0.2, linetype="dashed")
  q <- q + geom_hline(
    yintercept = 0, color = "red",
    size = 0.3)
  q <- q + scale_fill_hue(
    name = paste("Significant at the", conf.level, "level"), 
    breaks = 0:1, labels = c("Not Significant", "Significant")) +
    theme(panel.background = element_rect(
      size = 3, 
      colour = "black",
      fill = "white"),
      axis.ticks = element_line(
        size = 2),
      axis.title.x = element_text(
        size = rel(1.2), 
        face = "bold"),
      axis.title.y = element_text(
        size = rel(1.2), 
        face = "bold"),
      plot.title = element_text(
        size = 20,
        face = "bold", 
        vjust = 1.5),
      legend.position = "bottom",
      legend.title = element_text(
        size=rel(1.2), 
        face="bold"),
      legend.text = element_text(
        colour="blue", 
        size = 13))
  return(q)
}

# menampilkan nama datasets
output$namadata <- renderText({
  nama <- paste0("Data: ", input$datasets)
  nama
})

# mengisi kemungkinan model terbaik dari auto.arima
isiarrayterbaik <- reactive({
  values
})

####################################################
# Fungsi tambahan eg. cross validation
###################################################

output$cvplot <- renderPlot({
  
  if(is.null(input$buttoncv) || input$buttoncv == 0) return()
  isolate({
    # sorting nilai
    sort.terbaik()
    
    k <- 60 # minimum data length for fitting a model
    n <- length(get_data())
    mae1 <- mae2 <- matrix(NA,n-k,12)
    st <- tsp(data_series)[1]+(k-2)/12
    
    
    # cross validation
    
    
    for(i in 1:(n-k))
    {
      xshort <- window(data_series, end=st + i/12)
      xnext <- window(data_series, start=st + (i+1)/12, end=st + (i+12)/12)
            
      fit1 <- create.arima(values$modellist[[1]], xshort)  
      fcast1 <- forecast(fit1, h=12)
      fit2 <- create.arima(values$modellist[[2]], xshort)
      fcast2 <- forecast(fit2, h=12)
      
      mae1[i,1:length(xnext)] <- abs(fcast1[['mean']]-xnext)
      mae2[i,1:length(xnext)] <- abs(fcast2[['mean']]-xnext)
    }
    
    plot(1:12, colMeans(mae1,na.rm=TRUE), type="l", col="blue", xlab="horizon", ylab="MAE")
    lines(1:12, colMeans(mae2,na.rm=TRUE), type="l",col=3)
    
  })
})


########################################################
# bagian report, save html, pdf word dll.
########################################################

output$downloadReport <- downloadHandler(
  filename = function() {
    paste('Forecast-Report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  
  content = function(file) {
    src <- normalizePath('arima-report.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'arima-report.Rmd')
    
    library(rmarkdown)
    out <- render('arima-report.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)



# output plot forecast
plotperamalan <- function(){
  if(is.null(terbaik())){
    forecast1 <- forecast.Arima(estimate.auto(), h=input$period)  
  }
  else{
    forecast1 <- forecast.Arima(terbaik(), h=input$period)
  }
  p <- plot.forecast(forecast1)
}

# output nilai forecast
nilaiperamalan <- function(){
  if(is.null(terbaik())){
    forecast1 <- forecast.Arima(estimate.auto(), h=input$period)  
  }
  else{
    forecast1 <- forecast.Arima(terbaik(), h=input$period)
  }
  forecast1
}