shinyUI(
  navbarPage("Fast", id = "nav_fast", collapsable = TRUE,inverse = F,header = list(
    br(),
    includeCSS("www/style.css"),
    tags$head(
      tags$style(HTML("#explanationBox {
        overflow-y: auto;
        }
        #tableDataPred {
        overflow: scroll;
        height : 500px;
        }
        table.data {
          width : 100%
        }
        "
      ) # close HTML
      )            # close tags$style
    ),
    tags$head(
      tags$script(src = "js/jquery-ui.custom.min.js"),
      tags$script(src = "js/busy.js"),
      tags$script(src = 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML',
                  type = 'text/javascript')
      #tags$style(type='text/css', '.well {background-color: rgB( 0, 132, 180);}')
    )
  ),
  
  tabPanel("Data", uiOutput('data_ui_and_tabs')),
  
  navbarMenu("Classify",
             tabPanel("K Nearest Neighbour", uiOutput("knn")),
             tabPanel("Linear Disciminant Analysis", uiOutput("lda")),
             tabPanel("Random Forest", uiOutput("rf"))),
  
  navbarMenu("Cluster",
             tabPanel("Hierarchical", uiOutput("clusteringH")),
             tabPanel("Partitional", uiOutput("clusteringP")),
             tabPanel("Spatial Clustering (FGWC-GSA)", uiOutput("fgwcGsa"))
             ),

  navbarMenu("Forecast",
             tabPanel("FPA-SVR",uiOutput("FPASVR")),
             tabPanel("ARIMA", uiOutput("autoarima")),
             tabPanel("RELM time series", uiOutput("relm_forecast")),
             tabPanel("FFNN-Quasi Newton", uiOutput("ffnn_quasi")),
             tabPanel("FTS-ACO", uiOutput("ftsaco"))
             ),

  navbarMenu("Multivariate",
             tabPanel("Principal Component Analysis", uiOutput("pca")),
             tabPanel("Factor Analysis", uiOutput("factor")),
             tabPanel("Seemingly Unrelated Regression", uiOutput("sureg"))
             ),
  
  navbarMenu("Regression",
             tabPanel("Linear (OLS)", uiOutput("regression")),
             tabPanel("Tobit Regression", uiOutput("tobit_reg")),
             tabPanel("Ridge Regression", uiOutput("ridge")),
             tabPanel(" Geographically Weighted Ridge Regression", uiOutput("GWRR")),
             tabPanel("Spatial Regression",uiOutput("spatialreg")),
             tabPanel("Ridge Robust Regression", uiOutput("robust_ridge")),
             tabPanel("Robust Hurdle Poisson", uiOutput('robusthp'))
             ),
        
  navbarMenu("Survival",
             tabPanel("Life Table", uiOutput("survivalLT")),
             tabPanel("Kaplan Meier", uiOutput("survivalKM")),
             tabPanel("Cox PH", uiOutput("survivalCox"))),
  
  navbarMenu("Soft Computing",
             tabPanel("ANFIS", uiOutput("anfis")),
             tabPanel("Neufo Fuzzy-GMDH",uiOutput("NFGMDH")),
             tabPanel("Fuzzy C-Means", uiOutput("fuzzyCMeans")),
             tabPanel("FCM-ABC", uiOutput("fcmabc")),
             tabPanel("FGWC-ABC", uiOutput("abc")),
	           tabPanel("FGWC-PSO", uiOutput("fgwcpso"))
             ),
    navbarMenu("Association Rule",
             tabPanel("Apriori", uiOutput("apriori")),
             tabPanel("Eclat", uiOutput("eclat"))
            #tabPanel("FP-Growth")
            ),
    navbarMenu("BiClustering",
            tabPanel("Cheng Church's BiClustering",uiOutput("CCBiclustering"))
    )

 )
)