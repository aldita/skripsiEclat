shinyUI(
  
  navbarPage("Fast", id = "nav_fast", collapsable = TRUE,inverse = F,header = list(
    br(),
    includeCSS("www/flatlybootstrap.min.css"),
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
  
  tabPanel("Data",uiOutput('data_ui_and_tabs')),
  
  navbarMenu("Classification",
             tabPanel("Linear Discriminant Analysis", uiOutput("lda")),
             tabPanel("k-Nearest Neighbor", uiOutput("knn")),
             tabPanel("Random Forest", uiOutput("rf"))
  ),
  
  navbarMenu("Cluster",
             tabPanel("Hierarchical", uiOutput("clusteringH")),
             tabPanel("Partitional", uiOutput("clusteringP"))
  ),
  
  navbarMenu("Forecast",
             tabPanel("ARIMA", uiOutput("autoarima")),
             tabPanel("FPA-SVR", uiOutput("FPASVR"))
  ),
  
  navbarMenu("Multivariate",
             tabPanel("Manova", uiOutput("multivariate")),
             tabPanel("Compare Two Means", uiOutput("mean")),
             tabPanel("Factor Analysis", uiOutput("factor")),
             tabPanel("Principal Component Analysis", uiOutput("pca"))
  ),
  
  navbarMenu("Regression",
             tabPanel("Linear (OLS)", uiOutput("regression")),
             #tabPanel("Tobit Regression", uiOutput("tobit_reg")),
             tabPanel("Ridge Regression", uiOutput("ridge"))
  ),
  
  navbarMenu("Survival",
             tabPanel("Life Table", uiOutput("survivalLT")),
             tabPanel("Kaplan Meier", uiOutput("survivalKM")),
             tabPanel("Cox PH", uiOutput("survivalCox"))
  ),
  
  navbarMenu("Soft Computing",
             tabPanel("ANFIS", uiOutput("anfis")),
             tabPanel("Fuzzy C-Means", uiOutput("fuzzyCMeans")),
             tabPanel("FGWC-PSO", uiOutput("fgwcpso"))
  )
             
)
)