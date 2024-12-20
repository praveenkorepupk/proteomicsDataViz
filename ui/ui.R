#### UI Scripts ####

ui = dashboardPage(
  dashboardHeader(title = "Olink Scout"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Home",
      tabName = "scouthome",
      icon = icon("home"),
      selected = T
    ),
    menuItem(
      "Data Import",
      tabName = "fileimport",
      icon = icon("fas fa-file")
    ),
    menuItem(
      "Data Check",
      tabName = "datacheck",
      icon = icon("fas fa-chart-bar")
    ), 
    menuItem(
      "Data Processing",
      tabName = "dataprocessing",
      icon = icon("database"),
      menuSubItem(tabName = "subsetting", text = "Data Subsetting"),
      menuSubItem(tabName = "missingness", text = "Missing Values"),
      menuSubItem(tabName = "outlier", text = "Outlier Detection"),
      menuSubItem(tabName = "dimension", text = "Dimension Reduction")
    ),
    menuItem(
      "Exploratory Data Analysis",
      tabName = "visualization",
      icon = icon("fas fa-chart-line"),
      menuSubItem("clust", text="Clustering"),
      menuSubItem("pca", text = "PCA"),
      menuSubItem("hmap",
                  text = "Heatmap "),
      menuSubItem("boxplt", text = "Boxplot"),
      
      menuSubItem("corr",
                  text = "Correlation")
    ),
    menuItem(
      "Univariate Analysis",
      tabName = "univariateanalysis",
      icon = icon("fas fa-table")
    ),
    menuItem("Time course analysis",tabName = "tca",icon = icon("hourglass")),
    menuItem("Machine learning", icon=icon("laptop-code"),
             menuSubItem("pls", text="PLS"),
             menuSubItem("spls", text="SPLS"),
             menuSubItem("glmm", text="GLMM"),
             menuSubItem("knn", text = "KNN"),
             menuSubItem("svm", text = "SVM"),
             menuSubItem("rf", text = "Random Forest"),
             menuSubItem("xgb", text = "XgBoost"),
             menuSubItem("roc", text = "ROC Analysis")),
    menuItem("Functional Analysis",tabName = "gsea",icon = icon("fas fa-chart-line")),
    menuItem(
      "Report Generation",
      tabName = "reportGen",
      icon = icon("fas fa-file")
    ),
    menuItem(
      "Help Manual",
      tabName = "helpManual",
      icon = icon("fas fa-file")
    )
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "scouthome",
              div(
                class = "home-page-text",
                HTML("<h2>Olink® Statistical Analysis app</h2>
             <p>Olink has extensive coverage of the plasma proteome and delivers high quality data for over 3k unique proteins. However, when it comes time to perform data analysis, this amount of data can be overwhelming. Therefore, Olink provides a web-based application for basic data visualizations and statistical analyses based around the same tools and methods used by the Olink Data Science team.</p>
             <p>This app has the following modules:</p>
             <ul>
               <li><b>Data Import:</b> User can import the data from different sources.It has two sub features <i>File Import</i> and <i>File select/Test data</i>.</li>
               <li><b>Data Check:</b> User can check the data quality.It has two sub features <i>Data QC</i> and <i>Data Filtration</i>.</li>
               <li><b>Data Processing:</b> User can do the data preprocessing which is crucial for down stream analysis.It has the following sub features: <i>Data Subsetting</i>, <i>Missing Values</i>, <i>Outlier Detection</i> and <i>Dimension Reduction</i>.</li>
               <li><b>Exploratory Data Analysis:</b> User can perform EDA and can get useful insights.It has the following sub features: <i>Clustering</i>, <i>PCA</i>, <i>Heat Map</i>, <i>Boxplot</i> and <i>Correlation</i>.</li>
               <li><b>Univariate Analysis:</b> User and perform univariate analysis using <i>T-Test(2 groups)</i>, <i>Analysis of Variance(ANOVA, 3+ groups)</i>, <i>Mann-Whitney U Test</i>.</li>
               <li><b>Time Course Analysis:</b> User can use Time course analysis which is performed using the lmer function from OlinkAnalyze package.</li> 
               <li><b>Machine Learning:</b> User can perform multi variate analysis using <i>PLS</i>, <i>SPLS</i>, <i>GLMM</i>, <i>KNN</i>, <i>SVM</i>, <i>Random Forest</i> and <i>XgBoost</i> machine learning methods.Also user can summarise the results of different models using <i>ROC Analysis</i>.
               <li><b>GSEA:</b> User can perform gene set enrichment analysis using the results of univariate and multivariate analysis.</li>
               <li><b>Report Generation:</b> User can download results/figures for more focused data analysis.</li>
             </ul>")
              )
      ),
      tabItem(tabName = "fileimport", uiOutput("fileimportUI")),
      tabItem(tabName = "datacheck",
              fluidPage(
                shinyjs::useShinyjs(),
                titlePanel("Data Check"),
                fluidRow(uiOutput("descriptiondatacheck")),
                br(),
                fluidRow(
                  column(width = 6,
                         box(
                           width = 12,
                           title = "Data QC",
                           status = "primary",
                           solidHeader = T,
                           checkboxGroupInput(
                             inputId = "Panel_1",
                             label = "Select panel",
                             choices = ""
                           ),
                           fluidRow(
                             style = 'padding-left:15px; padding-right:5px; padding-top:5px; padding-bottom:5px',
                             actionButton(inputId = "runqcplots", label = "Show Plots")
                           )
                         )
                  ),
                  column(width = 6,
                         box(
                           width = 12,
                           title = "Data Filtration",
                           status = "primary",
                           solidHeader = T,
                           checkboxGroupInput(
                             inputId = "filterdata",
                             label = "Select filter",
                             choices = c(" ")
                           ),
                           actionButton("filter", "Filter"),
                           downloadButton("download4", "Download Filtered Data"),
                         )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.runqcplots > 0",
                tabsetPanel(
                  tabPanel("Scatter Plot",
                           fluidRow(plotlyOutput("scatterplots") %>% withSpinner())
                           ),
                  tabPanel("Frequency Distribution",
                           fluidRow(plotlyOutput("freqdistplot") %>% withSpinner())
                           ),
                  tabPanel("Outliers",
                           fluidRow(plotOutput("pcaplot") %>% withSpinner()),
                           br(),
                           downloadButton("download3", "Download")
                           )
                  )
                )
              ),
      tabItem(tabName = "subsetting",
              shinyjs::useShinyjs(),
              style = 'padding-left:15px; padding-right:5px; padding-top:5px; padding-bottom:5px',
              titlePanel("Data Subsetting"),
              fluidRow(uiOutput("descriptiondatass")),
              br(),
              uiOutput("datassUI")),
      tabItem(tabName = "missingness",
              fluidPage(
                shinyjs::useShinyjs(),
                style = 'padding-left:15px; padding-right:5px; padding-top:5px; padding-bottom:5px',
                titlePanel("Missing Values"),
                fluidRow(uiOutput("descriptionmissval")),
                br(), 
                fluidRow(
                  column(width = 3,
                         box(
                           width = 12,
                           title = "1. Check missingness",
                           status = "primary",
                           solidHeader = T,
                           actionButton("showmiss", "Show Plots")
                         )
                  ),
                  column(width = 8,
                         offset = 1,
                         tabsetPanel(
                           tabPanel(span("Protein missingness",title = "Protein missingness across samples"),
                                    plotlyOutput("proteinmissignessplot")
                                    ),
                           tabPanel(span("Sample missingness",title = "Sample missigness across protein"),
                                    plotlyOutput("samplemissignessplot")
                                    )
                           )
                  ),
                  column(width = 6,
                         box(
                           id = "removemissui",
                           width = 12,
                           title = "2. Remove features(protein) with high missingness",
                           status = "primary",
                           solidHeader = T,
                           checkboxGroupInput(
                             inputId = "miss",
                             label = "Remove proteins",
                             choices = c("Exclude proteins having missingness greater than")
                           ),
                           # Enter Number box within the filter box
                           div(class = "compact-number-input",
                               id = "miss_box",
                               conditionalPanel(
                                 condition = "input.miss.indexOf('Exclude proteins having missingness greater than') !== -1",
                                 textInput("miss_num", "Missigness value percentage")
                               )
                           ),
                           actionButton("remove", "Remove")
                           )
                         ),
                  column(width = 6,
                         box(
                           id = "imputemissui",
                           width = 12,
                           title = "3. Impute the remaining missing values",
                           status = "primary",
                           solidHeader = T,
                           selectInput(
                             inputId = "impute_method",
                             label = "Select Imputation method",
                             choices = c("Replace by LOD","Replace by Mean","Replace by Median","Replace by Min")
                           ),
                           actionButton("impute", "Impute")
                         )
                  )
                )
              )
      ),
      tabItem(tabName = "outlier",
              fluidPage(
                shinyjs::useShinyjs(),
                style = 'padding-left:15px; padding-right:5px; padding-top:5px; padding-bottom:5px',
                titlePanel("Outlier Detection"),
                fluidRow(uiOutput("descriptionoutlier")),
                br(),
                fluidRow(
                  column(width = 4,
                         box(
                           width = 12,
                           title = "Outlier Detection Techniques",
                           status = "primary",
                           solidHeader = T,
                           selectInput(
                             inputId = "outlierdetection",
                             label = "Select outlier detection method",
                             choices = c("IQR","PCA","Cook's Distance")
                           ),
                           actionButton("showoutlier", "Show Outliers"),
                           actionButton("removeoutliers", "Remove Outliers")
                         )
                  ))
              ),
              conditionalPanel(
                condition = "input.showoutlier > 0",
                tabsetPanel(
                  tabPanel("Outliers",
                           fluidRow(plotlyOutput("outlierplot") %>% withSpinner())
                  )
                )
              )
      ),
      tabItem(tabName = "dimension",
              fluidPage(
                shinyjs::useShinyjs(),
                style = 'padding-left:15px; padding-right:5px; padding-top:5px; padding-bottom:5px',
                titlePanel("Dimension Reduction"),
                fluidRow(uiOutput("descriptiondimreduction")),
                br(),
                fluidRow(
                  column(width = 4,
                         box(
                           width = 12,
                           title = "Dimension Reduction Techniques",
                           status = "primary",
                           solidHeader = T,
                           selectInput("drmethod", "Dimension Reduction Method:", 
                                       choices = c("PCA", "t-SNE", "UMAP")),
                           selectInput("color_var", "Color Variable:", 
                                       choices = c("")),
                           selectInput("shape_var", "Shape Variable:", 
                                       choices = c("")),
                           div(class = "compact-number-input",
                               id = "perplexityinput",
                               conditionalPanel(
                                 condition = "input.drmethod == 't-SNE'",
                                 numericInput(inputId = "prexid", "Perplexity:", value = 3),
                                 bsTooltip(id = "prexid",title ="Defaut recommended value is 30.Use a lower value less than 5 for smaller dataset.")
                               )
                              ),
                           div(class = "compact-number-input",
                               id = "umapn",
                               conditionalPanel(
                                 condition = "input.drmethod == 'UMAP'",
                                 numericInput(inputId = "umapnid", "Neighbors:", value = 15),
                                 bsTooltip(id = "umapnid",title ="Defaut recommended value is 15.Neighbors value should always be less than number of samples. ")
                               )
                           ),
                           
                           actionButton("showdimr", "Plot")
                           )
                         )
                  )),
              conditionalPanel(
                condition = "input.showdimr > 0",
                tabsetPanel(
                  tabPanel("Dimension Reduction Plots",
                           fluidRow(plotlyOutput("dimplot") %>% withSpinner())
                  )
                )
              )),
      tabItem(tabName = "univariateanalysis", uiOutput("diagnosticmarkerUI")),
      tabItem(tabName = "prognosticm1", uiOutput("prognosticmarkerUI")),
      tabItem(tabName = "predictivem1", uiOutput("predictivemarkerUI")),
      tabItem(tabName = "clust",
              tabsetPanel(
                tabPanel(title = "Heirarchical clustering",
                         column(width=12,
                                box(
                                  HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
  <h2>Heirarchical clustering</h2>
  <p>Heirarchical clustering is performed using the <i>hclust</i> function in R. It uses 2 parameters - distance method and agglomeration method.(Reference: <a href='https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/hclust'> hclust vignette</a>)</p>
  </div>"),width = 12,collapsible = T)),
                         fluidRow(
                           column(width = 3, selectInput(inputId = "dist",label = "Distance method",choices = c("euclidean", "maximum","canberra","manhattan","minkowski","binary"),selected = "euclidean")),
                           bsTooltip(id = "dist",title = "method used for computing distance matrix",placement = "top",trigger = "hover"),
                           column(width = 3, selectInput(inputId = "agg", label = "Agglomeration method", choices = c("ward.D", "ward.D2","single","average","complete","median","centroid","mcquitty"),selected = "average")),
                           bsTooltip(id = "agg",title = "method used for agglomeration or grouping similar entities",placement = "top",trigger = "hover")),
                         fluidRow(
                           column(width=3, actionButton(inputId = "clustbutton",label = "Plot"))
                         ),
                         br(),
                         fluidRow(plotlyOutput("hclustOut"))),
                tabPanel(title = "PAM",
                         column(width=12,
                                box(
                                  HTML(  "<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
  <h2>Partition around Medioids</h2>
  <p>PAM clustering is performed by first finding the optimal clusters using the <i>NbClust</i> package in R and then running the <i>pam</i> function. It uses 1 parameter - distance method. Only 2 distance methods - euclidean and manhattan are available in this method.
  (Reference: <a href='https://www.rdocumentation.org/packages/cluster/versions/2.1.6/topics/pam'> PAM vignette</a>)</p>
  </div>"),collapsible = T, width = 12)),
                         fluidRow(
                           column(width = 3, selectInput(inputId = "dist2",label = "Distance method", choices = c("euclidean","manhattan"))),
                           bsTooltip(id = "dist2",title = "method used for computing distance matrix",placement = "top",trigger = "hover")
                           
                           
                         ),
                         fluidRow(column(width = 4, actionButton(inputId = "pam_submit",label = "Plot"))),
                         br(),
                         fluidRow(plotlyOutput("pamOut")))
              )),
      tabItem(tabName = "pca",
              column(width=12,
                     box(
                       HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
  <h2>Principal Component analysis</h2>
             <p>PCA is performed using the <i>prcomp</i> function. It uses 3 parameters: color vector, shape vector and the principal components.This module generates the biplot and screeplot.(Reference: <a href='https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/prcomp'> PCA vignette</a>)</p>
             </div>"),collapsible = T, width = 12)),
              uiOutput("pcaUI")),
      tabItem(tabName="boxplt",
              column(width=12,
                     box(
                       HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
  <h2>Boxplot</h2>
  <p>Boxplots provide an easy way of viewing the NPX values for each protein across various groups. It uses 2 inputs - Group and Protein (Reference: <a href='http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization'>Boxplot vignette</a>)</p>
  </div>"),collapsible = T,width = 12
                     )),
              uiOutput("boxpltUI")),
      tabItem(tabName = "hmap",
              column(width = 12,
                     box(
                       HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
  <h2>Heatmap</h2>
  <p>This module provides a method to visualize proteins across multiple groups and cluster them. The user chooses a group or multiple groups and clustering method to plot a heatmap. (Reference: <a href='https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html'>Heatmaply vignette</a>)</p>
  </div>"), collapsible = T,width = 12
                     )),
              uiOutput("hmUI")),
      tabItem(tabName = "corr",uiOutput("correlationedaUI")),
      tabItem(tabName = "tca", uiOutput("timecourseanalysisUI")),
      tabItem(tabName = "pls", uiOutput("plsUI")),
      tabItem(tabName = "spls", uiOutput("splsUI")),
      tabItem(tabName = "glmm", uiOutput("glmmUI")),
      tabItem(tabName = "knn", uiOutput("knnUI")),
      tabItem(tabName = "svm",uiOutput("svmUI")),
      tabItem(tabName = "xgb", uiOutput("xgbUI")),
      tabItem(tabName = "rf", uiOutput("rfUI")),
      tabItem(tabName = "roc", uiOutput("rocUI")),
      tabItem(tabName = "gsea", uiOutput("genesetenrichmentanalysisUI")),
      tabItem(tabName = "reportGen", 
              column(width=12,
                     box(
                       HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
  <h2>Generate report</h2>
  <p>Report Contains the plots which are generated by the user, if user doesn't performed or skipped any section, that will be showcased as NULL or Not selected</p>
  <h2>Download Plots</h2>
  <p>It will be downloaded as zipped file, which contains the plots generated by the user. if user doesn't performed or skipped any section during the analysis, those plots will be generated as Empty Plots. </p>
  </div>"),collapsible = T,width = 12
                     )),
              downloadButton("report", "Generate report"),
              downloadButton("plotsDownloadButton", "Download Plots")),
      tabItem(tabName = "helpManual", 
              column(width = 12,
                     tags$iframe(style="height:655px; width:100%", src = "Olink_Scout_Manual.pdf")
              )
      )
    )
  )
)
