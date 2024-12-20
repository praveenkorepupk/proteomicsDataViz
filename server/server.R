#### Shiny Configuration ####
server = shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 1024 * 1024 ^ 2)
  options(show.error.messages = TRUE)
  options = list(lengthMenu = c(5, 10, 15, 20, 25, 50, 100),
                 pageLength = 10)
  
  # Reactive values to store all the generated plots to use them while generating report & download zip file
  reportProd <- reactiveValues(column_barplot1 = NULL,
                               column_piechart1 = NULL,
                               failed_samples = NULL,
                               qcPlot1 = NULL, 
                               qcPlot2 = NULL,
                               qcPlot3 = NULL,
                               missingValuePlots1 = NULL,
                               missingValuePlots2 = NULL,
                               outlierDetection1 = NULL,
                               dimRedPlots1 = NULL,
                               hclust_plot1 = NULL,
                               pam_plot1 = NULL,
                               pca_plot1 = NULL,
                               scree_plot1 = NULL,
                               heatmap1_eda1 = NULL,
                               box_plot1 = NULL,
                               corel_plot1 = NULL,
                               heatmap_ttest1 = NULL,
                               volcanoplt_ttest1 = NULL,
                               bxplt1_ttest1 = NULL,
                               heatmap_Anova1 = NULL,
                               anovabxplt1_an = NULL,
                               heatmap_mwutest1 = NULL,
                               mwutbxplt1_mwut = NULL,
                               pls_imp1 = NULL,
                               pls_roc1 = NULL,
                               pls_text1 = NULL,
                               pls_cm_plot1 = NULL,
                               spls_roc1 = NULL,
                               spls_text1 = NULL,
                               spls_cm_plot1 = NULL,
                               spls_imp1 = NULL,
                               svm_text1 = NULL,
                               svm_cm_plot1 = NULL,
                               svm_roc1 = NULL,
                               svm_imp1 = NULL,
                               rf_text1 = NULL,
                               rf_cm_plot1 = NULL,
                               rf_roc1 = NULL,
                               rf_imp1 = NULL,
                               xgb_text1 = NULL,
                               xgb_cm_plot1 = NULL,
                               xgb_roc1 = NULL,
                               xgb_imp1 = NULL,
                               lmer_plot1 = NULL,
                               tca_heatmap1 = NULL,
                               dot_plot1 = NULL,
                               bw_plot1 = NULL,
                               dotplot_GSEA1 = NULL,
                               ridgeplot_GSEA1 = NULL,
                               dimRed_color_var = NULL,
                               dimRed_shape_var = NULL,
                               hclust_distMethod = NULL, 
                               hclust_aggMethod = NULL,
                               PAM_distMethod = NULL,
                               pca_shape1 = NULL,
                               pca_color1 = NULL,
                               PC_11 = NULL,
                               PC_21 = NULL,
                               heatmapgroups1 = NULL,
                               heatmap_clust1 = NULL,
                               box_genenames1 = NULL,
                               box_group1 = NULL,
                               ttest_comparison_variable1 = NULL,
                               ttest_pair_id1 = NULL,
                               ttest_condition11 = NULL,
                               ttest_condition21 = NULL,
                               anova_comparison_variable1 = NULL,
                               anova_comparison_variable2 = NULL,
                               anova_covariate = NULL,
                               anova_pair_id = NULL,
                               type_of_mwu_test1 = NULL,
                               mwut_comparison_variable = NULL,
                               mwut_pair_id = NULL,
                               mwut_condition1 = NULL,
                               mwut_condition2 = NULL,
                               tca_var1 = NULL,
                               tca_rand1 = NULL,
                               tca_cov1 = NULL,
                               pls_column1 = NULL,
                               pls_perc1 = NULL, 
                               pls_fsel1 = NULL,
                               spls_column1 = NULL,
                               spls_perc1 = NULL, 
                               spls_fsel1 = NULL,
                               glmm_column1 = NULL,
                               glmm_perc1 = NULL,
                               glmm_fsel1 = NULL,
                               knn_column1 = NULL,
                               knn_perc1 = NULL,
                               knn_fsel1 = NULL,
                               svm_column1 = NULL,
                               svm_perc1 = NULL,
                               svm_fsel1 = NULL,
                               xgb_column1 = NULL,
                               xgb_perc1 = NULL,
                               xgb_fsel1 = NULL,
                               glmm_text1 = NULL,
                               glmm_cm_plot1 = NULL,
                               glmm_roc1 = NULL,
                               glmm_imp1 = NULL,
                               knn_text1 = NULL,
                               knn_cm_plot1 = NULL,
                               knn_roc1 = NULL,
                               knn_imp1 = NULL,
                               df_msg21 = NULL, 
                               df_msg_FS1 = NULL,
                               df_msg_FI1 = NULL,
                               total_rows1 = NULL,
                               total_columns1 = NULL,
                               npx_format1 = NULL,
                               protein_counts1 = NULL,
                               sample_counts1 = NULL,
                               unMatchedSamplesOlink1 = NULL,
                               meta_rows1 = NULL,
                               meta_cols1 = NULL,
                               meta_format1 = NULL,
                               meta_cols_name1 = NULL,
                               unMatchedSamplesMeta1 = NULL
                               )
  
  notMatchedSampleIds <- reactiveValues(
    no_matchd_olink_SampleIDs = NULL,
    no_matchd_meta_SampleIDs = NULL
  )
  
  resetPlotsAndVariables <- function() {
    reportProd$column_barplot1 <- NULL
    reportProd$column_piechart1 <- NULL
    reportProd$qcPlot1 <- NULL
    reportProd$qcPlot2 <- NULL
    reportProd$qcPlot3 <- NULL
    reportProd$missingValuePlots1 <- NULL
    reportProd$missingValuePlots2 <- NULL
    reportProd$outlierDetection1 <- NULL
    reportProd$dimRedPlots1 <- NULL
    reportProd$hclust_plot1 <- NULL
    reportProd$pam_plot1 <- NULL
    reportProd$pca_plot1 <- NULL
    reportProd$scree_plot1 <- NULL
    reportProd$heatmap1_eda1 <- NULL
    reportProd$box_plot1 <- NULL
    reportProd$corel_plot1 <- NULL
    reportProd$heatmap_ttest1 <- NULL
    reportProd$volcanoplt_ttest1 <- NULL
    reportProd$bxplt1_ttest1 <- NULL
    reportProd$heatmap_Anova1 <- NULL
    reportProd$anovabxplt1_an <- NULL
    reportProd$heatmap_mwutest1 <- NULL
    reportProd$mwutbxplt1_mwut <- NULL
    reportProd$pls_imp1 <- NULL
    reportProd$pls_roc1 <- NULL
    reportProd$pls_text1 <- NULL
    reportProd$pls_cm_plot1 <- NULL
    reportProd$spls_roc1 <- NULL
    reportProd$spls_text1 <- NULL
    reportProd$spls_cm_plot1 <- NULL
    reportProd$spls_imp1 <- NULL
    reportProd$svm_text1 <- NULL
    reportProd$svm_cm_plot1 <- NULL
    reportProd$svm_roc1 <- NULL
    reportProd$svm_imp1 <- NULL
    reportProd$rf_text1 <- NULL
    reportProd$rf_cm_plot1 <- NULL
    reportProd$rf_roc1 <- NULL
    reportProd$rf_imp1 <- NULL
    reportProd$xgb_text1 <- NULL
    reportProd$xgb_cm_plot1 <- NULL
    reportProd$xgb_roc1 <- NULL
    reportProd$xgb_imp1 <- NULL
    reportProd$lmer_plot1 <- NULL
    reportProd$tca_heatmap1 <- NULL
    reportProd$dot_plot1 <- NULL
    reportProd$bw_plot1 <- NULL
    reportProd$dotplot_GSEA1 <- NULL
    reportProd$ridgeplot_GSEA1 <- NULL
  }
  
  ####File import UI ####
  output$fileimportUI = renderUI({
    
    fluidPage(
      
      tabsetPanel(
        id = "loadDataTabs",  # Add an ID to the tabsetPanel
        type = "tabs",
        tabPanel(
          title = "Data Import",
          value = "description",
          id= "data-description",
          icon = icon("database"),
          br(),
          fluidRow(uiOutput("descriptionDataImport")),
          br(),
          fluidRow(radioButtons(inputId = "datatype1",
                                label = "Select a import method",
                                choices = c("File import","File select/Test data"),
                                selected = "File import",inline = T)),
          conditionalPanel(
            condition = "input.datatype1 == 'File import'",
            fluidRow(column(
              width = 7,
              box(
                title = "File Import",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                fileInput(
                  inputId = "file1",
                  multiple = TRUE,
                  label = strong('Please select a NPX file'),
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    "npx"
                  )
                ),
                fileInput(
                  inputId = "file2",
                  multiple = TRUE,
                  label = strong('Please Select a Metadata File'),
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",
                    ".xls",
                    ".xlsx",
                    ".json",
                    "Excel-Worksheet",
                    ".Rdata"
                  )
                ),
                fluidRow(
                  column(width = 3, checkboxInput("olinkHTCheckbox", 
                                                  HTML("<strong>Olink HT</strong>"), 
                                                  value = FALSE),
                         tags$script("$(document).ready(function(){
                         $('#olinkHTCheckbox').attr('title', 'Check this box if the data is from Olink HT');});")
                         ),
                  column(width = 6, actionButton("loadBtn", "Load data"))
                )
              )
            ))),
          conditionalPanel(
            condition = "input.datatype1 == 'File select/Test data'",
            fluidRow(column(
              width = 7,
              box(
                title = "File Import",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput(
                  "Dfile1",
                  label = "Please select a NPX file",
                  ## Comment this for DB Integration and uncomment choices = NULL
                  # choices = list.files("./data/", pattern = "NPXdata"),
                  choices = list.files("./data/proteinData/", pattern = "_NPXdata"),
                  # choices = NULL,
                  selectize = F
                ),
                selectInput(
                  "Dfile2",
                  label = "Please select a Metadata file",
                  ## Comment this for DB Integration and uncomment choices = NULL
                  # choices = list.files("./data/", pattern = "_metadata"),
                  choices = list.files("./data/proteinData/", pattern = "_metadata"),
                  # choices = NULL,
                  selectize = F
                ),
                actionButton("loadBtns", "Load data"))),
              
            )), 
          
          fluidRow(style = 'padding-left:28px; padding-right:5px; padding-top:5px; padding-bottom:10px',
                   htmlOutput("message3")%>% withSpinner()), 
          
          
        ),
        tabPanel(
          title = "Data Summary",
          value = "Data Summary",
          id= "data-tab-summary",
          icon = icon("trello"),
          tags$style(HTML(".tab-content .shiny-tab-content.Metadata { margin-left: 20px; }")),
          fluidRow(
            column(width = 5,uiOutput("qc_info")%>% withSpinner()),
            column(width = 6,
                   tabsetPanel(
                     type = "pills",
                     tabPanel("Sample Distribution", 
                              selectInput("column_barplot", "Select Column", choices = NULL),
                              plotlyOutput("barplot")
                     ),
                     tabPanel("Protein Statistics",
                              selectInput("column_piechart", "Select Column", choices = NULL),
                              tags$div(style = "margin-bottom: 30px;"),
                              plotlyOutput("piechart")
                     )
                   )
            )
          )
        ),
        tabPanel(
          title = "NPX Data",
          value = "NPX_Data",
          icon = icon("uber"),
          # fluidRow(DTOutput("data_table_npx")%>% withSpinner()),
          fluidRow(dataTableOutput("df")%>% withSpinner()),
          id = "npx-tab-content"  # Add an ID to the tabPanel for NPX Data
        ),
        tabPanel(
          title = "Metadata",
          value = "Metadata",
          icon = icon("uber"),
          fluidRow(DTOutput("data_table_metadata")%>% withSpinner()),
          id = "metadata-tab-content"  # Add an ID to the tab Panel for Metadata
        )
      )
    )
    
  })
  
  # Data Check Description
  output$descriptiondatacheck <- renderUI({
    HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #3498db;'>
       <p>Data check is a mandatory step after importing the dataset. It includes the following features:</p>
       <ul>
         <li><strong>Data QC</strong> - User can select multiple panels and can perform QC analysis using plots. Note selecting panel is mandatory for downstream analysis. (Reference: <a href='https://cran.r-project.org/web/packages/OlinkAnalyze/vignettes/Vignett.html#scatterplot-for-qc-olink_qc_plot'>Scatter QC Plot Vignette</a>, <a href='https://cran.r-project.org/web/packages/OlinkAnalyze/vignettes/Vignett.html#boxplots-for-qc-olink_dist_plot'>Frequency Distribution QC Plot Vignette</a>, <a href='https://cran.r-project.org/web/packages/OlinkAnalyze/vignettes/Vignett.html#principal-components-analysis-pca-plot-olink_pca_plot'>Outlier QC Plot Vignette</a>)</li>         <li><strong>Data Filtration</strong> - User can filter the data based on the QC analysis. Note its not a mandatory step.</li>
       </ul>      
     </div>")
  })
  
  # Data Processing- Subsetting Description
  output$descriptiondatass <- renderUI({
    HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #3498db;'>
       <p>Data subsetting is used to select a subset of a data from the entire dataset. For data subsetting metadata has used. User can use the following features:</p>
       <ul>
         <li>User can filter the data in the view using search box given for different features(of the metadata). Note its not a mandatory step.</li>
         <li>User can adjust the view using <strong> show entries </strong> tab.</li>
       </ul>      
     </div>")
  })
  
  # Data Processing- Missing Values Description
  output$descriptionmissval <- renderUI({
    HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #3498db;'>
       <p>Too many missing values will cause difficulties for downstream analysis. Missingness can be caused by removing QC and Assay warning. There are several different methods for this purpose. User can use the following features:</p>
       <ul>
         <li> <strong>Step1.</strong> Check missigness in your dataset. (Reference: <a href='https://search.r-project.org/CRAN/refmans/naniar/html/gg_miss_var.html'>Missingness Vignette)</a></li>
         <li> <strong>Step2.</strong> User can remove missing proteins across samples by giving a threshold of missigness percentage. Use Sample missigness plot to see protein missingness distribution. Note its not a mandatory step.</li>
         <li> <strong>Step3.</strong> User can impute the remaining missing values by using <strong>LOD</strong>(Limit of detection), <strong>mean</strong>, <strong>median</strong>, <strong>min</strong>(minimum)</strong> methods. Note its not a mandatory step.</li>
       </ul>      
     </div>")
  })
  
  # Data Processing- Outlier Detection
  output$descriptionoutlier <- renderUI({
    HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #3498db;'>
       <p>Outlier detection is a crucial step in data analysis to identify extreme or unusual data points that can significantly affect the results. There are several different methods for this purpose. User can use the following features:</p>
       <ul>
         <li>User can detect oultier by using <strong>IQR, PCA, Cook's Distance</strong> methods. Note its not a mandatory step.</li>
       </ul>      
     </div>")
  })
  
  # Data Processing- Dimension Reduction
  output$descriptiondimreduction <- renderUI({
    HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #3498db;'>
       <p>The datasets can contain a large number of dimensions which can cause various issues like making computation expensive, adding noise to data, and inducing multicollinearity. The dimension reduction methods allow us to visualize data in lower dimensions so we understand the data. There are several different methods for this purpose. User can use the following features:</p>
       <ul>
         <li>User can detect oultier by using <strong>PCA, t-SNE, UMAP</strong> methods. Note its not a mandatory step.</li>
         <li>User can pick metadata fields as legends <strong>(Color Variable)</strong> in the plot.</li>
         <li>User can pick metadata fields as legends <strong>(Shape Variable)</strong> in the plot.</li>
       </ul>      
     </div>")
  })
  
  # Render Data Summary for data upload
  output$qc_info <- renderUI({
    #npx_data
    if (input$datatype1 == "File import") {
      total_rows <- nrow(df3()[[1]])
      total_columns <- ncol(df3()[[1]])
      npx_col_names <- colnames(df3()[[1]])
      npx_extension <- tools::file_ext(df1()[[2]])
      if (npx_extension == "csv") {
        npx_format = ".csv"
      } else if (npx_extension == "xlsx") {
        npx_format = ".xlsx"
      } else if (npx_extension == "parquet"){
        npx_format = ".parquet"
      } 
      protein_counts <- length(unique(df3()[[1]][["OlinkID"]]))
      sample_counts <- length(unique(df3()[[1]][["SampleID"]]))
      #meta_data
      meta_rows <- nrow(df3()[[2]])
      meta_cols <- ncol(df3()[[2]])
      meta_cols_name <- colnames(df3()[[2]])
      meta_extension <- tools::file_ext(df2()[[2]])
      if (meta_extension == "csv") {
        meta_format = ".csv"
      } else if (meta_extension == "xlsx") {
        meta_format = ".xlsx"
      } else if (meta_extension == "parquet"){
        meta_format = ".parquet"
      }
      
      reportProd$total_rows1 <- total_rows
      reportProd$total_columns1 <- total_columns
      reportProd$npx_format1 <- npx_format
      reportProd$protein_counts1 <- protein_counts
      reportProd$sample_counts1 <- sample_counts
      reportProd$unMatchedSamplesOlink1 <- notMatchedSampleIds$no_matchd_olink_SampleIDs
      reportProd$meta_rows1 <- meta_rows
      reportProd$meta_cols1 <- meta_cols
      reportProd$meta_format1 <- meta_format
      reportProd$meta_cols_name1 <- meta_cols_name
      reportProd$unMatchedSamplesMeta1 <- notMatchedSampleIds$no_matchd_meta_SampleIDs
      
      #Data summary
      summary_text <- paste(
        "<br/>",
        "&nbsp;",
        "&nbsp;",
        "&nbsp;",
        "&nbsp;",
        "&nbsp;",
        "&nbsp;",
        "<strong>Data Processing Information</strong>",
        "<br/>",
        "<br/>",
        "&nbsp;",
        "<strong>NPX Data:</strong",
        "<br/>",
        "<br/>",
        "&nbsp;",
        "1. Total number of rows: ", total_rows,
        "<br/>",
        "&nbsp;",
        "2. Total number of columns: ", total_columns,
        "<br/>",
        "&nbsp;",
        "3. Uploaded file format: ", npx_format,
        "<br/>",
        "&nbsp;",
        "4. Total proteins: ", protein_counts,
        "<br/>",
        "&nbsp;",
        "5. Total samples: ", sample_counts,
        "<br/>",
        "&nbsp;",
        "6. Unmatched samples: ", toString(notMatchedSampleIds$no_matchd_olink_SampleIDs),
        "<br/>",
        "<br/>",
        "&nbsp;",
        "<strong>Meta Data:</strong",
        "<br/>",
        "<br/>",
        "&nbsp;",
        "1. Total number of rows: ", meta_rows,
        "<br/>",
        "&nbsp;",
        "2. Total number of features: ", meta_cols, 
        "<br/>",
        "&nbsp;",
        "3. Uploaded file format: ", meta_format,
        "<br/>",
        "&nbsp;",
        "4. Features Names: ", toString(meta_cols_name),
        "<br/>",
        "&nbsp;",
        "5. Unmatched samples: ", toString(notMatchedSampleIds$no_matchd_meta_SampleIDs)
      )
      HTML(summary_text) 
    } else{
      total_rows <- nrow(df3()[[1]])
      total_columns <- ncol(df3()[[1]])
      npx_col_names <- colnames(df3()[[1]])
      protein_counts <- length(unique(df3()[[1]][["OlinkID"]]))
      sample_counts <- length(unique(df3()[[1]][["SampleID"]]))
      #meta_data
      meta_rows <- nrow(df3()[[2]])
      meta_cols <- ncol(df3()[[2]])
      meta_cols_name <- colnames(df3()[[2]])
      reportProd$total_rows1 <- total_rows
      reportProd$total_columns1 <- total_columns
      reportProd$npx_format1 <- npx_format
      reportProd$protein_counts1 <- protein_counts
      reportProd$sample_counts1 <- sample_counts
      reportProd$unMatchedSamplesOlink1 <- notMatchedSampleIds$no_matchd_olink_SampleIDs
      reportProd$meta_rows1 <- meta_rows
      reportProd$meta_cols1 <- meta_cols
      reportProd$meta_format1 <- meta_format
      reportProd$meta_cols_name1 <- meta_cols_name
      reportProd$unMatchedSamplesMeta1 <- notMatchedSampleIds$no_matchd_meta_SampleIDs
      #Data summary
      summary_text <- paste(
        "<br/>",
        "&nbsp;",
        "&nbsp;",
        "&nbsp;",
        "&nbsp;",
        "&nbsp;",
        "&nbsp;",
        "<strong>Data Processing Information</strong>",
        "<br/>",
        "<br/>",
        "&nbsp;",
        "<strong>NPX Data:</strong",
        "<br/>",
        "<br/>",
        "&nbsp;",
        "1. Total number of rows: ", total_rows,
        "<br/>",
        "&nbsp;",
        "2. Total number of columns: ", total_columns,
        "<br/>",
        "&nbsp;",
        "4. Total proteins: ", protein_counts,
        "<br/>",
        "&nbsp;",
        "5. Total samples: ", sample_counts,
        "<br/>",
        "&nbsp;",
        "6. Unmatched samples: ", toString(notMatchedSampleIds$no_matchd_olink_SampleIDs),
        "<br/>",
        "<br/>",
        "&nbsp;",
        "<strong>Meta Data:</strong",
        "<br/>",
        "<br/>",
        "&nbsp;",
        "1. Total number of rows: ", meta_rows,
        "<br/>",
        "&nbsp;",
        "2. Total number of features: ", meta_cols, 
        "<br/>",
        "&nbsp;",
        "4. Features Names: ", toString(meta_cols_name),
        "<br/>",
        "&nbsp;",
        "5. Unmatched samples: ", toString(notMatchedSampleIds$no_matchd_meta_SampleIDs)
      )
      HTML(summary_text)
    }
  })
  
  # Observe changes in the load button(s) and datatype selection
  observeEvent(req(input$loadBtn | input$loadBtns),{
    # Check if the selected datatype is "File select/Test data"
    if (input$datatype1 == "File select/Test data") {
      data <- as.data.frame(df3()[[2]])
      char_cols <- names(data)[sapply(data, is.character)] # Identify character columns in the data frame
      valid_cols <- char_cols[sapply(char_cols, function(col) length(unique(data[[col]])) < nrow(data))] # Filter out columns with unique values less than the number of rows in the data frame
      updateSelectInput(session, "column_barplot", choices = valid_cols) # Update the choices in the column_barplot select input with the valid columns
    }else{
      # If datatype is not "File select/Test data", perform the same operations as above
      data <- as.data.frame(df3()[[2]])
      char_cols <- names(data)[sapply(data, is.character)]
      valid_cols <- char_cols[sapply(char_cols, function(col) length(unique(data[[col]])) < nrow(data))]
      updateSelectInput(session, "column_barplot", choices = valid_cols)  
    }
  })
  

  # Render the bar plot based on selected column
  output$barplot <- renderPlotly({
    req(input$column_barplot) # Ensure that a column is selected before rendering
    data <- as.data.frame(df3()[[2]]) 
    
    # Create a ggplot bar plot with the selected column as x-axis and fill
    ggplot(data, aes_string(x = input$column_barplot, fill = input$column_barplot)) +
      geom_bar(position = "dodge") +
      labs(title = " ")
  })
  
  # Update the interactive plotly bar plot when the selected column changes
  observeEvent(input$column_barplot, {
    req(input$column_barplot)   # Ensure that a column is selected
    data <- as.data.frame(df3()[[2]])
    
    # Create a ggplot bar plot with the selected column as x-axis and fill
    p <- ggplot(data, aes_string(x = input$column_barplot, fill = input$column_barplot)) +
      geom_bar(position = "dodge") +
      labs(title = " ")
    
    # Convert the ggplot plot to a plotly plot and store it in reportProd$column_barplot1
    reportProd$column_barplot1<-ggplotly(p)
  })
  
  
  # Pie Charts
  # Update dropdown choices for barplots dynamically
  observeEvent(req(input$loadBtn | input$loadBtns),{
    # Check if the selected datatype is "File select/Test data"
    if (input$datatype1 == "File select/Test data") {
      data <- as.data.frame(df3()[[1]])
      required_columns <- c("QC_Warning", "Assay_Warning", "Panel") # Define required columns for pie charts
      missing_columns <- setdiff(required_columns, colnames(data))  # Identify columns missing in the uploaded dataset
      matched_columns <- intersect(required_columns, colnames(data)) # Identify columns that match the required columns
      if (length(missing_columns) > 0) {
        # Show notification message
        updateSelectInput(session, "column_piechart", choices = matched_columns)  # Update dropdown choices with matched columns and show a warning notification
        showNotification(
          paste("Columns", paste(missing_columns, collapse = ", "), "are missing in the NPX dataset uploaded."),
          type = "warning"
        )
      } else {
        # Update dropdown choices
        updateSelectInput(session, "column_piechart", choices = required_columns) # Update dropdown choices with required columns
      }
    }else{
      # If datatype is not "File select/Test data", perform the same operations as above
      data <- as.data.frame(df3()[[1]])
      required_columns <- c("QC_Warning", "Assay_Warning", "Panel")
      missing_columns <- setdiff(required_columns, colnames(data))
      matched_columns <- intersect(required_columns, colnames(data))
      print("matched_columns")
      print(matched_columns)
      if (length(missing_columns) > 0) {
        updateSelectInput(session, "column_piechart", choices = matched_columns)  # Update dropdown choices with matched columns and show a warning notification
        showNotification(
          paste("Columns", paste(missing_columns, collapse = ", "), "are missing in the NPX dataset uploaded."),
          type = "warning"
        )
      } else {
        updateSelectInput(session, "column_piechart", choices = matched_columns) # Update dropdown choices with required columns
      }
    }
  })
  
  # Render Piechart
  output$piechart <- renderPlotly({
    req(input$loadBtn | input$loadBtns,input$column_piechart)  # Ensure that the load button and column selection are pressed before rendering
    # Check if selectInput options are empty
    if (is.null(input$column_piechart) || length(input$column_piechart) == 0) {
      return("No data available for selected column.")
    }
    
    data <- as.data.frame(df3()[[1]])
    selected_column <- input$column_piechart # Get the selected column for the pie chart
    # Check if the selected column exists in the dataset
    if (selected_column %in% colnames(data)) {
      data <- data %>% select(selected_column) # Subset the data to include only the selected column
      count_table <- table(data[[selected_column]]) # Create a table of counts for the selected column
      # Create a plotly pie chart
      pie_chart <- plot_ly(labels = names(count_table),values = count_table,type = "pie") %>%
        layout(title = " ",height = 300,width = 400)
      reportProd$column_piechart1<-pie_chart    # Store the pie chart in reportProd$column_piechart1
      pie_chart # Return the pie chart
    } else {
      reportProd$column_piechart1<-NULL     # If the selected column is not present in the dataset, show an error notification
      showNotification("Selected column is not present in the NPX dataset you uploaded", type = "error")
    }
  })
  
  ##########################################################
  # Establish a connection to Snowflake
  # con <- DBI::dbConnect(
  #   odbc::odbc(),
  #   Driver = "SnowflakeDSIIDriver",
  #   Server = "el57643.ap-south-1.aws.snowflakecomputing.com",
  #   UID = "AMANJOLLY",
  #   PWD = "Hitman@047",
  #   Warehouse = "COMPUTE_WH",
  #   Database = "SANOFI_OLINK",
  #   Schema = "RSHINY"
  # )
  # 
  # Function to fetch Snowflake table names
  # get_snowflake_tables <- function(pattern) {
  #   tables <- dbGetQuery(con, "SHOW TABLES;")
  #   tables <- tables$name
  #   # tables <- tables[grepl(pattern, tables)]
  #   return(tables)
  # }
  # 
  # Function to fetch Snowflake NPX data based on table name
  # get_snowflake_npx_data <- function(table_name) {
  #   data <- DBI::dbGetQuery(con, paste0('SELECT * FROM "', table_name, '"'))
  #   return(data)
  # }
  # 
  # # Function to fetch Snowflake Metadata based on table name
  # get_snowflake_metadata_data <- function(table_name) {
  #   data <- DBI::dbGetQuery(con, paste0('SELECT * FROM "', table_name, '"'))
  #   return(data)
  # }
  # 
  # selected_data <- reactiveValues(
  #   npx = NULL, metadata = NULL
  # )
  # 
  # # Update Snowflake dropdown choices when the Data Select tab is selected
  # observeEvent(req(input$datatype1 == "File select/Test data"),
  #              {
  #                if (input$datatype1 == "File select/Test data") {
  #                  # Fetch and update NPX dropdown choices
  #                  tables_npx <- get_snowflake_tables()
  #                  updateSelectInput(session, "Dfile1", 
  #                                    choices = tables_npx, selected = " ")
  #                }
  #              })
  # 
  # observeEvent(input$Dfile1, {
  #   if (input$datatype1 == "File select/Test data") {
  #     updateSelectInput(session, "Dfile2",
  #                       choices = paste0(input$Dfile1, "_metadata"),
  #                       selected = paste0(input$Dfile1, "_metadata"))
  #     
  #   }
  # })
  # 
  
  
  #### Load Data Dimension-Check ####
  observeEvent(req(input$loadBtn | input$loadBtns),{
    print("Load Dataset Dimesion")
    print(dim(df3()[[1]]))
    print(dim(df3()[[2]]))
  })
  
  #### Data check plots ####
  # Check panel 
  observeEvent(input$runqcplots,{
    # Check if any panel is selected
    if(length(input$Panel_1) > 0 ){
      # Print selected panel
      print(paste('Panel selected: ',input$Panel_1))
    }
    else{
      # Show warning message if no panel is selected
      shinyalert(
        title = "Panels not found",
        text = "Please select a panel for Data check.",
        type = "warning")
    }
  })
  
  # 1. Generate scatter qc plots 
  # Define event reactive function to generate quality control plot
  qc_plot1 <- eventReactive(input$runqcplots, {
    # Require the selection of a panel
    req(input$Panel_1)
    
    # Get the selected panel name
    panel_name <- input$Panel_1
    
    # Extract the Olink data
    olink_data <- df3()[[1]]
    
    # Check if the "QC_Warning" column exists in the data
    if ("QC_Warning" %in% colnames(df3()[[1]])) {
      # Generate the scatter plot for the specified panel
      fig1 <- scatter_plot(olink_data, panel_name)
      
      # Return the plot
      return(fig1)
    }
  })
  
  # Render scatter qc plots 
  observeEvent(input$runqcplots, {
    output$scatterplots <- renderPlotly({
      # Check if the "QC_Warning" column exists in the data
      if (any("QC_Warning" %in% colnames(df3()[[1]]))) {
        
        # Store the scatter plot in the report
        reportProd$qcPlot1 = qc_plot1()[[1]]
        reportProd$failed_samples <- qc_plot1()[[2]]
        
        # Display the scatter plot
        qc_plot1()[[1]]
      }
      else {
        # Set the scatter plot in the report to NULL
        reportProd$qcPlot1 = NULL
        
        # Display a success message if no "QC_Warning" column is found
        shinyalert(
          title = "No 'QC_Warning' found in your dataset",
          text = "There are no QC plots. Please proceed to the next step.",
          type = "success"
        )
        # Return NULL if no "QC_Warning" column is found
        NULL
      }
    })
  })
  
  # 2. Generate frequency distribution qc plots 
  # Define reactive expression for generating box plots
  boxp1 <- eventReactive(input$runqcplots, {
    # Check if Panel_1 input is provided
    req(input$Panel_1)
    
    # Extract panel name from input
    panel_name <- input$Panel_1
    
    # Extract data from dataframe
    olink_data <- df3()[[1]]
    
    # Calculate unique sample count
    unique_sample_count <- length(unique(olink_data$SampleID))
    
    # Check if "QC_Warning" column exists in the data
    if ("QC_Warning" %in% colnames(df3()[[1]])) {
      # Check if unique sample count is greater than or equal to 200
      
      if (unique_sample_count >= 200) {
        # Randomly select few samples from dataset for QC
        random_samples_for_QC <- select_random_samples_for_QC(olink_data)
        fig2 <- frq_plot(random_samples_for_QC, panel_name)
      } 
      else {
        # Generate frequency plot using all samples
        fig2 <- frq_plot(olink_data, panel_name)
      }
      
      # Convert ggplot to plotly object
      fig2 <- ggplotly(fig2)
      
      # Return the plotly object
      return(fig2)
    }
  })
  
  # Render frequency distribution plot
  output$freqdistplot <- renderPlotly({
    # Check if "QC_Warning" column exists in the data
    if ("QC_Warning" %in% colnames(df3()[[1]])) {
      
      # Store the plot in the reportProd object
      reportProd$qcPlot2 = boxp1()
      
      # Return the plotly object
      boxp1()
    } 
    else {
      # If "QC_Warning" column is not found, set reportProd$qcPlot2 to NULL
      reportProd$qcPlot2 <- NULL
      
      # Return NULL
      NULL
    }
  })
  
  # 3. Generate Outlier qc plots
  # Observer for generating PCA plots
  observeEvent(input$runqcplots, {
    # Check if Panel is selected
    req(input$Panel_1)
    
    # Initialize list to store plots
    plots <- list()
    
    # Extract panel name
    panel_name <- input$Panel_1
    
    # Filter data for PCA
    pca_data <- df3()[[1]] %>% 
      filter(Panel %in% panel_name) %>% 
      filter(!str_detect(SampleID, 'CONTROL_SAMPLE'))
    
    # Check if QC_Warning column exists
    if ("QC_Warning" %in% colnames(df3()[[1]])) {
      # Generate PCA plots
      pcalist <- generate_plots(pca_data)
      
      # Keep only plots that are of class "gg"
      pcaplots <- keep(pcalist, ~ inherits(., "gg"))
      
      # Iterate over plots and add them to the list
      for (i in seq_along(pcaplots)) {
        plots[[i]] <- pcaplots[[i]]
      }
      
      # Combine plots
      tryCatch(
        {
          combined_plot <- grid.arrange(
            grobs = plots,
            ncol = 1 
          )
          ggsave("OutliersPCAplots.png", combined_plot, width = 10, height = 6, units = "in", dpi = 300)
        },
        error = function(e) {
          print(paste("An error occurred:", e$message))
        }
      )
    }
    
    # Render PCA plot
    output$pcaplot <- renderPlot({
      if ("QC_Warning" %in% colnames(df3()[[1]])) {
        grid.arrange(grobs = pcaplots)
        reportProd$qcPlot3 = pcaplots
      } else {
        reportProd$qcPlot3 = NULL
        NULL
      }
    })
    
    # Download the plot
    output$download3 <- downloadHandler(
      filename = function() {
        "OutliersPCAplots.png"  # Change the filename and extension as needed
      },
      content = function(file) {
        filename <- file
        file.copy("OutliersPCAplots.png", file)
      }
    )
  })
  
  #### Data Filtration ####
  # Reactive Data
  filtered_data <- reactiveValues()
  
  # Update filter check box choices
  observeEvent(req(input$loadBtn | input$loadBtns),{
    
    # Default value assignment to reactive
    filtered_data$npx <- df3()[[1]]
    filtered_data$metadata <- df3()[[2]]
    
    # Filter choices
    choices = character(0)
    columnames <- colnames(df3()[[1]])

    # Conditions for filter choices
    if ("QC_Warning" %in% columnames) 
    {
      choices <- c(choices, "Exclude QC Warning")
    } 
    if ("Assay_Warning" %in% columnames)
    {
      choices <- c(choices, "Exclude Assay Warning")
    }
    
    # Update filter choices
    updateCheckboxGroupInput(session, "filterdata", choices = choices)
  })
  
  # Data filtration
  observeEvent(input$filter,{
    
    # Condition when filters are selected
    if (!is.null(input$filterdata)){
      # Data
      filtered_data$npx <- df3()[[1]]
      filtered_data$metadata <- df3()[[2]]
      
      # Filters selected
      print(paste('Filter selected: ',input$filterdata))
      
      # Filter QC Warning
      if ("Exclude QC Warning" %in% input$filterdata) {
        filtered_data$npx <- filtered_data$npx %>% filter(QC_Warning != "WARN")
      }
      
      # Filter Assay Warning
      if ("Exclude Assay Warning" %in% input$filterdata) {
        filtered_data$npx <- filtered_data$npx %>% filter(Assay_Warning != "WARN")
      }
      
      # Update the reactiveVals
      filtered_data$npx <- filtered_data$npx
      filtered_data$metadata <- filtered_data$metadata
      
      # Show a notification
      shinyalert::shinyalert(
        title = "Data Filtered",
        text = "The data has been filtered successfully.",
        type = "success"
      )
    }
    
    # Condition when filters are not selected
    else{
      # Data
      filtered_data$npx <- df3()[[1]]
      filtered_data$metadata <- df3()[[2]]
      
      # Show a notification
      shinyalert(
        title = "Filters not found",
        text = "Please select a filter for Data filtration.",
        type = "warning")
      
    }
    
    # Download data
    output$download4 <- downloadHandler(
      filename = function() {
        'Filtered_Data.zip'
      },
      
      content = function(file) {
        filename <- file
        file1 <- "filtered_NPXdata.csv"
        file2 <- "filtered_metadata.csv"
        
        print("Writing filtered data to CSV files...")
        print(paste("File 1:", file1))
        print(paste("File 2:", file2))
        
        write.csv(filtered_data$npx, file1, row.names = FALSE)
        write.csv(filtered_data$metadata, file2, row.names = FALSE)
        
        # Check if the files are written successfully
        print("Filtered data written to CSV files successfully.")
        print("Creating zip file...")
        
        #create the zip file
        zip::zip(filename, files = c(file1, file2))
        
        # Check if the zip file is created successfully
        if (file.exists(filename)) {
          print("Zip file created successfully.")
        } else {
          print("Failed to create zip file.")
        }
      }
    )
    
  })
  
  #### Filter Data Dimension-Check ####
  observe({
    print("Data after filtration")
    print(dim(filtered_data$npx))
    print(dim(filtered_data$metadata))
  })
  
  #### Data Processing ####
  
  #1. Data Subsetting 
  
  # Subsetting UI
  output$datassUI = renderUI({ 
    fluidPage(
      shinyjs::useShinyjs(),
      fluidRow(
        column(width = 6,
               box(
                 width = 12,
                 title = "Subsetting Options",
                 status = "primary",
                 solidHeader = T,
                 actionButton("select_all", "Select All Rows in View"),
                 actionButton("clear_selection", "Clear All selected Rows"),
               )
        )
      ),
      br(),
      fluidRow(dataTableOutput("subset")),
      br(),
      fluidRow(actionButton("subset_data", "Subset data"))
    )
  })
  
  # Subsetting Logic
  output$subset<- renderDataTable({
    datatable(
      filtered_data$metadata,
      class = "display",
      filter = "top",
      selection = "multiple",
      extensions = 'Buttons',
      options = list(
        pageLength = 0,
        lengthMenu = c(10, 15, 20,50,100,200),
        autoWidth = FALSE,
        scrollX = TRUE,
        buttons = c('csv', 'excel')
      )
    )
  }, server = FALSE)
  
  # Define table proxy
  tableProxy <- dataTableProxy('subset')
  
  # Event handler for selecting all rows
  observeEvent(input$select_all, {
    selectRows(proxy = tableProxy,
               selected = input$subset_rows_current)
  })
  
  # Event handler for clearing selection
  observeEvent(input$clear_selection, {
    selectRows(proxy = tableProxy,
               selected = " ")
  })
  
  # Reactive expression for subsetting data
  subsetdata <- reactive({
    npx = filtered_data$npx
    metadata = filtered_data$metadata
    tryCatch(
      expr = {
        if (input$subset_data > 0) {
          metadata = metadata[input$subset_rows_selected, ]
          if (nrow(metadata) == 0) {
            metadata = filtered_data$metadata
          }
        }
        else {
          metadata = filtered_data$metadata
        }
      },
      error = function(e) {
        npx = filtered_data$npx
        metadata = filtered_data$metadata
      }
    )
    subsetsamples <- metadata$SampleID
    common_sample_ids <- intersect(metadata$SampleID, npx$SampleID)
    
    # Filter metadata and data to include only common SampleIDs
    npx <- npx[npx$SampleID %in% common_sample_ids,]
    metadata <- metadata[metadata$SampleID %in% common_sample_ids,]
    return(list(npx = npx, metadata = metadata))
  })
  
  # Subset Notifications
  observeEvent(input$subset_data, {
    if(input$subset_data>0){
      shinyalert::shinyalert(
        title = "Data Subsetted",
        text = paste("The data has been subsetted successfully. Total",length(unique(subsetdata()$metadata$SampleID))," samples are selected. Note: Samples that does not have metadata information will be removed. "),
        type = "success"
      )
    }
  })
  
  #### Subset Data Dimension-Check ####
  observe({
    print("Check dataset after subseting")
    print(dim(subsetdata()$npx))
    print(dim(subsetdata()$metadata))
  })

  #2. Missing Values
  
  # Missingness calculation and removal logic
  observeEvent(input$showmiss,{
    
    # Data
    npx_data <- subsetdata()$npx
    
    # Convert long to wide format
    test <- npx_data %>% pivot_wider(id_cols = OlinkID, #Instead of assay use olink ID
                                   names_from = SampleID,
                                   values_from = NPX,
                                   values_fill = NA
                                   )
    npx_data_short <- as.data.frame(subset(test, select = -OlinkID))
    rownames(npx_data_short) <- test$OlinkID
    
    # Protein Missingness(in samples) plot
    output$proteinmissignessplot <- renderPlotly({
      if (any(is.na(npx_data_short))) {
        missing_percentage_plot(npx_data_short)[[1]]
        reportProd$missingValuePlots1 <- missing_percentage_plot(npx_data_short)[[1]]
        } 
      else {
        reportProd$missingValuePlots1 <- NULL
        shinyalert(
        title = "No protein missingness found in your dataset",
        text = "Please proceed to Outlier detection step.",
        type = "success"
        )
        NULL
        }
      })
    
    # Sample Missingness(in protein) plot
    output$samplemissignessplot <- renderPlotly({
      if (any(is.na(npx_data_short))) {
        missing_percentage_plot(npx_data_short)[[2]]
        reportProd$missingValuePlots2 <- missing_percentage_plot(npx_data_short)[[2]] 
        } 
      else {
        reportProd$missingValuePlots2 <- NULL
        shinyalert(
          title = "No sample missingness found in your dataset",
          text = "Please proceed to Outlier detection step.",
          type = "success"
          )
        NULL
        }
      })
    })
  
  # Removal of protein with specific missingness
  # Defining new reactive for missingness
  removemiss <- reactive({ 
    if (input$remove > 0) {
      
      # Data 
      metadata <- subsetdata()$metadata
      npx <- subsetdata()$npx
      
      # Missingnes threshold given by UI
      miss_num <- input$miss_num
      
      # Condition for input missingness removal
      if (grepl("^\\d+$", input$miss_num)) {
        
        # Remove protein with specific missingness
        miss_output <- removeprotein(npx,miss_num)
        remove_proteins <<- miss_output[[1]]
        print(paste("Proteins to remove: ",remove_proteins))
        npx <- miss_output[[2]]
      } 
      else {
        shinyalert::shinyalert(
          title = "Warning",
          text = "Please provide a numeric value for missingness removal. Example: 5",
          type = "warning"
        )
      }
    } 
    else {
      # Data when no missingness is removed
      metadata <- subsetdata()$metadata
      npx <- subsetdata()$npx
    }
    
    return(list(npx = npx, metadata = metadata))
  })
  
  # Notifications for unmet missingness conditions
  observeEvent(c(input$miss_num,input$remove),{
    removemiss()
    if (input$remove > 0 ){
      if (grepl("^\\d+$", input$miss_num) & !is.null(reportProd$missingValuePlots2) ){
        shinyalert::shinyalert(
          title = "Missigness Removed",
          text = paste("Proteins having missingness greater than", input$miss_num, "percent has been removed successfully. Total",length(remove_proteins), "proteins has been removed."),
          type = "success")    
      }
      else if(is.null(reportProd$missingValuePlots2)){
        shinyalert::shinyalert(
          title = "No sample missingness found across protein in your dataset.",
          text = "Please proceed to Outlier detection step.",
          type = "success") 
      }
    }
    
  })
  
  #### Missingness Removal Data Dimension-Check ####
  observe({
    req(removemiss())
    print("Check dataset after missingness removal")
    print(dim(removemiss()$npx))
    print(dim(removemiss()$metadata))
  })
  
  # Imputation
  # Defining new reactive for imputation
  imputemiss <- reactive({
    if (input$impute > 0) {
      
      # Process spinner for UI
      show_modal_spinner(spin = "atom",
                         color = "firebrick",
                         text = "Please wait while imputation is in progress.")
      
      # Data
      metadata <- removemiss()$metadata
      npx <- removemiss()$npx
      
      # Convert long to wide format
      test <- npx %>% pivot_wider(id_cols = OlinkID, #Instead of assay use olink ID
                                       names_from = SampleID,
                                       values_from = NPX,
                                       values_fill = NA
                                       )
      npx_data_short <- as.data.frame(subset(test, select = -OlinkID))
      rownames(npx_data_short) <- test$OlinkID
      npx_data_short_t <<- t(npx_data_short)
      
      # Check missingess before imputation
      print(paste("Checking missingness present ",any(is.na(npx_data_short_t))))
      
      # Imputation logic
      # Imputation method selected
      imputation_method <- input$impute_method
      
      # Conditions
      if(imputation_method == "Replace by Mean"){
        for (col in colnames(npx_data_short_t)) {
          npx_data_short_t[,col] <- na.aggregate(npx_data_short_t[,col],FUN = mean)
        }
      }
      if(imputation_method == "Replace by Median"){
        for (col in colnames(npx_data_short_t)) {
          npx_data_short_t[,col] <- na.aggregate(npx_data_short_t[,col],FUN = median)      
        }
      }
      if(imputation_method == "Replace by Min"){
        for (col in colnames(npx_data_short_t)) {
          npx_data_short_t[,col] <- na.aggregate(npx_data_short_t[,col],FUN = min)      
        }
      }
      if(imputation_method == "Replace by LOD"){
      
        # Create dictionary for saving OLinkID and their LOD values
        old_npx <- subsetdata()$npx %>% select(OlinkID, LOD) %>% distinct(OlinkID, .keep_all = TRUE)
        olink_lod <- old_npx %>% group_by(OlinkID) %>% summarise(LOD = unique(LOD)) %>% deframe()
        
        # Replacing nan with LOD
        for (col in colnames(npx_data_short_t)) {
          npx_data_short_t[,col] <- sapply(npx_data_short_t[,col], function(x) ifelse(is.na(x), olink_lod[col],x))
        }
      }
      
      # Check missingess after imputation
      print(paste("Checking missingness present after imputation ",any(is.na(npx_data_short_t))))
      
      # Convert the list to a dataframe
      npx_data_short_t <- as.data.frame(npx_data_short_t)
      
      # Convert row names to a column
      npx_data_short_t$SampleID <- rownames(npx_data_short_t)
      
      # Convert from wide to long format
      npx <- pivot_longer(npx_data_short_t, 
                                cols = -SampleID, 
                                names_to = "OlinkID", 
                                values_to = "NPX")
      
      # Adding other important columns to npx_data long format
      data <- df3()[[1]]
      
      # Merge data and npx based on SampleID and OlinkID columns
      merged_df <- merge(npx, data, by = c("SampleID", "OlinkID"), all.x = TRUE)
      
      # Reorder the columns to match the desired order
      merged_df <- merged_df[c("SampleID", "OlinkID", "NPX.x", "UniProt", "Assay", "Panel")]
      
      # Rename the columns to remove the .x suffix
      colnames(merged_df) <- c("SampleID", "OlinkID", "NPX", "UniProt", "Assay", "Panel")
    
      # Updating Reactive values
      npx <- merged_df
      remove_modal_spinner()
    } 
    else{
      npx <- removemiss()$npx
      metadata <- removemiss()$metadata
    }
    
    return(list(npx = npx, metadata = metadata))
  })
  
  # Update Impute Reactive
  observeEvent(input$impute,{
    imputemiss()
  })
  
  # Notifications
  observeEvent(input$impute, {
    if (any(is.na(npx_data_short_t))){
      shinyalert::shinyalert(
        title = "Data Imputed",
        text = paste("NPX values of the missing proteins has been imputed successfully using",input$impute_method, "method."),
        type = "success"
      )
    }
    else{
      shinyalert::shinyalert(
        title = "Missingness not found in data for impution.",
        text = "No missigness found. Please proceed to Outlier detection step.",
        type = "success"
      )
    }
    
  })
  
  #### Imputed Data Dimension-Check ####
  observe({
    req(imputemiss())
    print("Check dataset after imputation")
    print(dim(imputemiss()$npx))
    print(dim(imputemiss()$metadata))
  })
 
  #3. Outlier Detection
  
  # Define a reactive
  datawithoutliers <- reactive({
    npx <- imputemiss()$npx
    metadata <- imputemiss()$metadata 
    return(list(npx = npx, metadata = metadata))
  })
  
  # Reactive to store outliers
  outliers <-  reactiveVal(NULL)
  
  # Detect and Plot outliers
  observeEvent(input$showoutlier,{
    
    # Define data
    npx <- datawithoutliers()$npx
    metadata <- datawithoutliers()$metadata

    # Transform data
    npx <- npx %>% pivot_wider(id_cols = OlinkID, #Instead of assay use olink ID
                                names_from = SampleID,
                                values_from = NPX,
                                values_fill = NA                        
                               )
    npx <- as.data.frame(npx)
    rownames(npx) <- npx$OlinkID
    npx <- subset(npx, select = -OlinkID)
    npx <- t(npx)
    npx <- as.data.frame(npx)
    npx$SampleID <- rownames(npx)
    long_data <- pivot_longer(npx, cols = -SampleID, names_to = "Protein", values_to = "NPX")
    
    # Outlier detection technique from UI
    technique <- input$outlierdetection
    
    # Outlier detection
    if (technique == "IQR") {
      
      # Outlier detection using IQR
      iqr <-  iqr(long_data,metadata)

      # Convert to interactive plot
      iqrplot <- ggplotly(iqr[[1]])
      reportProd$outlierDetection1 <- iqr[[1]]
      
      # Render the IQR plot
      output$outlierplot <- renderPlotly(iqrplot)
      
      # Outliers(samples) to remove
      outliers(iqr[[2]])
    } 
    else if (technique == "PCA") {
      
      # Outlier detection using PCA
      pca <- pca(long_data,metadata)
      
      # Convert to interactive plot
      pcaplot <- ggplotly(pca[[1]])
      reportProd$outlierDetection1 <- pca[[1]]
      
      # Render the PCA plot
      output$outlierplot <- renderPlotly(pcaplot)
      
      # Outliers(samples) to remove
      outliers(pca[[2]])
    } 
    else if (technique == "Cook's Distance") {
      
      # Outlier detection using Cook's Distance
      cooks <- cooks(long_data,metadata)
      
      # Convert to interactive plot
      cooksplot <- ggplotly(cooks[[1]])
      reportProd$outlierDetection1 <- cooks[[1]]
      
      # Render the Cooks plot
      output$outlierplot <- renderPlotly(cooksplot)
      
      # Outliers(samples) to remove
      outliers(cooks[[2]])
      }
  })
  
  # Updating reactive after removing outliers
  final_data <- reactive({
    if(input$removeoutliers>0){
      npx <- imputemiss()$npx
      metadata <- imputemiss()$metadata
      npx <- npx %>% filter(!SampleID %in% outliers()$SampleID)
      metadata <- metadata  %>% filter(!SampleID %in% outliers()$SampleID)
    }
    else{
      npx <- imputemiss()$npx
      metadata <- imputemiss()$metadata
    }
    return(list(npx = npx, metadata = metadata))
  })
  
  # Update oultier reactive based on observe event
  observeEvent(input$removeoutliers, {
    print(outliers())
    final_data()
  })
  
  # Notifications for outliers
  observeEvent(input$removeoutliers,{
    if(length(outliers()$SampleID)>0)
    {
      shinyalert::shinyalert(
        title = "Outliers Removed",
        text = paste("There are in total",length(unique(outliers()$SampleID)), "samples which are behaving as outliers and has been removed successfully."),
        type = "success"
      )
    }
    else{
      shinyalert::shinyalert(
        title = "Outliers Removed",
        text = paste("No outliers found."),
        type = "success"
      )
    }
  })
  
  #### Data after Outlier Dimension-Check ####
  observe({
    print("Checking dim of final data")
    print(dim(final_data()$npx))
    print(dim(final_data()$metadata))
  })
  
  #3. Dimension Reduction
  # Update colour and shape dropdown
  observeEvent(req(input$loadBtn | input$loadBtns),{
    
    #Metadata
    metadata <- as.data.frame(df3()[[2]])
    char_col1 <- names(metadata)
    char_col2 <- setdiff(names(metadata), c("SampleID"))
    
    #Update drop downs
    updateSelectInput(session, "color_var", choices = char_col1)
    updateSelectInput(session, "shape_var", choices = char_col2)
  }
  )
  
  # Dimension reduction and plots
  observeEvent(input$showdimr,{
    # Data Transformation NPX data: samples as row and columns as proteinID
    
    # Data
    npx_data <- final_data()$npx
    metadata <- as.data.frame(final_data()$metadata)
    
    # Convert long to wide format
    npx_data <- npx_data %>% pivot_wider(id_cols = OlinkID, #Instead of assay use olink ID
                                         names_from = SampleID,
                                         values_from = NPX,
                                         values_fill = NA
                                         )
    npx_data <- as.data.frame(npx_data)
    rownames(npx_data) <- npx_data$OlinkID
    npx_data <- subset(npx_data, select = -OlinkID)
    npx_data <- t(npx_data)
    
    # Remove rows with missing values
    npx_data <- na.omit(npx_data)
    sample_ids <- rownames(npx_data)
    metadata <- metadata %>% filter(SampleID %in% sample_ids)
    # Input from UI
    technique <- input$drmethod #Dimension reduction technique
    
    color <- input$color_var #Label for Colour
    reportProd$dimRed_color_var <- color
    
    shape <- input$shape_var # Label for shape
    reportProd$dimRed_shape_var <- shape
    
    
    if(technique == "PCA"){
      
      # Dimension reduction using PCA
      drpca <- drpca(npx_data,metadata,color,shape)
      
      # Plot for report
      reportProd$dimRedPlots1 = drpca
      
      # Render PCA plot
      output$dimplot <- renderPlotly(drpca)
    }
    else if (technique == "t-SNE"){
      
      # Perplexity input from UI
      p <- input$prexid
      
      # Dimension reduction using t-SNE
      drtsne <- drtsne(npx_data,metadata,color,shape,p)
      
      # Plot for report
      reportProd$dimRedPlots1 = drtsne
      
      # Render t-SNE Plot
      output$dimplot <- renderPlotly(drtsne)
    }
    else if(technique == "UMAP"){
      
      #Neighbours input from UI
      n <- input$umapnid
      
      # Dimension reduction using UMAP
      drumap <- drumap(npx_data,metadata,color,shape,n)
      
      # Plot for report
      reportProd$dimRedPlots1 = drumap
      
      # Render UMAP Plot
      output$dimplot <- renderPlotly(drumap)
    }
  }) 
 
  
  #################################################################################################################################
  ## reactive to merge data and metadata ## 
  
  merge_final_data <- reactive({
    
    mat = final_data()$npx
    meta = final_data()$metadata 
    mer_df = merge(x = mat, y = meta,by = "SampleID",all = TRUE)
    return(mer_df)
    
  })
  
  
  ## timecourse UI####
  
  output$timecourseanalysisUI <- renderUI({
    fluidPage(title = "Time course analysis",
              column(width = 12, 
                     box(
                       HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
               <h2> Time course analysis </h2>
                <p>Time course analysis is performed using the <i>lmer</i> function from OlinkAnalyze package. The following are the parameters to be used: </p>
                <li>variable: the variable of interest for performing time course analysis</li>
                <li>random: variable that generates random effect </li>
                <li>covariates: Other covariates to consider (optional)(Reference: <a href='https://cran.r-project.org/web/packages/OlinkAnalyze/vignettes/Vignett.html'> OlinkAnalyze vignette</a>) </li>
                </div>"),collapsible = TRUE,width = 12)),
              br(),
              tabsetPanel(id = "tcapanel",
                          tabPanel(title = "Run analysis",          
                                   fluidRow(
                                     br(),
                                     column(width=3,selectInput(inputId = "tca_var",label = "Select Time variable", choices = colnames(final_data()$metadata), selected = NULL, multiple = T )),
                                     bsTooltip(id = "tca_var",title = "variable of interest and other interacting terms.Should be 1 or more metadata columns.",placement = "top",trigger = "hover"),
                                     column(width=3, selectInput(inputId = "tca_rand", label = "Select Random variable", choices = colnames(final_data()$metadata), selected = NULL)),
                                     bsTooltip(id = "tca_rand",title = "variable with random effects; should be metadata column",placement = "top",trigger = "hover"),
                                     column(width=3, selectInput(inputId = "tca_cov", label = "Select Covariates", choices = colnames(final_data()$metadata), selected = NULL,multiple = TRUE)),
                                     bsTooltip(id = "tca_cov",title = "variable with confounding factors to include in the analysis. Should be metadata column(s)",placement = "top",trigger = "hover")
                                     
                                   ), 
                                   fluidRow(
                                     column(width=3, actionButton(inputId = "tca_submit",label = "Submit",width = '100px',style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                   ),
                                   br(),
                                   fluidRow(column(width=12, DTOutput("tca_table"))),
                                   fluidRow(column(width=8, textOutput("tca_guide_message"))),
                                   fluidRow(column(width=12, plotOutput("tca_plot"))), 
                                   br(),
                                   fluidRow(column(width=8, uiOutput("tca_posthoc_btn")))
                          ),
                          tabPanel(title = "Run Posthoc",
                                   fluidRow(column(width=3, uiOutput("tca_effect_choice")),
                                            bsTooltip(id = "tca_effect",title = "term of interest from lmer analysis to be used for posthoc analysis",placement = "top",trigger = "hover"),
                                            column(width=3, uiOutput("tcaph_proteins")),
                                            bsTooltip(id = "tca_ph_protein",title = "List of proteins of interest to be included in posthoc analysis",placement = "top","hover")
                                            
                                   ),
                                   fluidRow(
                                     column(width=3, uiOutput("run_posthoc_btn"))
                                   ),
                                   fluidRow(column(width=12, DTOutput("tca_table2"))),
                                   br(),
                                   fluidRow(column(width = 4, uiOutput("tca_heatmap_dropdown"))),
                                   br(),
                                   fluidRow(column(width = 12, plotlyOutput("tca_heatmap")))
                          )))
    
  })
  
  ###### 
  
  #### Timecourse analysis reactive ######
  
  
  ## Define reactive values to make the UI interactive 
  table_shown <- reactiveVal(FALSE) ## This value is true when the results of the lmer analysis 
  tca_plot_shown <- reactiveVal(FALSE) ## This value is set to true when boxplots are displayed for the selected proteins. 
  tca_proceed_clicked = reactiveVal(FALSE) ## this value is set to true when the "proceed" button is clicked for navigating to posthoc analysis
  tca_posthoc_table_shown = reactiveVal(FALSE) ## THis value is set to tru when the posthoc result table is displayed. 
  
  ## Run lmer upon choosing the inputs and clicking the submit button for timecourse.
  lmer_analysis <- eventReactive(input$tca_submit,{
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait while analysis is in progress. This may take a while...")
    
    
    
    req(input$tca_var, input$tca_rand, input$tca_cov)
    tryCatch({
    res = run_lmer(data=merge_final_data(),var=input$tca_var,rand=input$tca_rand,cov=input$tca_cov)        
    }, error = function(e){
      
      shinyalert(title = "ERROR!",text = "Please ensure that the number of levels in your variables are not equal to the number of samples/data points.")
    })
    remove_modal_spinner()
    return(res)
  })
  
  ## render the result table for lmer 
  output$tca_table <- renderDT({
    
    df = lmer_analysis()
    d = datatable(df, options = list(scrollX=TRUE,scrollY='200px',scrollCollapse=TRUE,paging=F),style = "bootstrap",class = 'cell-border stripe',selection = 'multiple')
    table_shown(TRUE)
    return(d)
  })
  
  # Render message after the table is rendered. 
  output$tca_guide_message <- renderText({
    if(table_shown()){
      m = "Please select upto 5 rows to compare.."
      return(m)
    }
    
  })
  
  ## observe the event where rows are selected in the result table. If more than 5 rows are selected, show notification
  observeEvent(input$tca_table_rows_selected,{
    r = lmer_analysis()
    print(head(r))
    selected = input$tca_table_rows_selected
    if(length(selected)>5){
      
      showNotification(ui = "Please make a selection of only upto 5 rows")
    } else {
      
      r_sel = r[selected,]
      print(r_sel)
      olinkIDs = as.character(r_sel$OlinkID)
      
      output$tca_plot <- renderPlot({
        
        p = lmer_plot(data = merge_final_data(), var = input$tca_var, rand = input$tca_rand, cov = input$tca_cov, olinkid_list = olinkIDs,xvar = input$tca_var) 
        reportProd$lmer_plot1 <- p
        p
        
      })
      
      tca_plot_shown(TRUE)
      
    }
    
    
  })
  
  #---------------------------------------
  
  # render the proceed button
  output$tca_posthoc_btn <- renderUI({
    
    if(tca_plot_shown()){
      
      actionButton(inputId = "tca_proceed",label = "Proceed to posthoc analysis",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      
    }
    
  })
  
  ##--------------------------------------
  
  ## clicking on proceed button takes the user to the next tab for posthoc analysis
  observeEvent(input$tca_proceed,{
    
    updateTabsetPanel(session,inputId = "tcapanel",selected = "Run Posthoc")
    tca_proceed_clicked(TRUE)
    
  })
  
  ## Time course posthoc UI ########
  
  #render the input fields for posthoc analysis 
  output$tca_effect_choice <- renderUI(
    if(tca_proceed_clicked()){
      res = lmer_analysis()
      
      selectInput(inputId = "tca_effect",label = "Select effect",choices = unique(res$term))
      
    }
    
  )
  
  #render input fields for posthoc analysis 
  output$tcaph_proteins <- renderUI({
    if(tca_proceed_clicked()){
      
      
      selectInput(inputId = "tca_ph_protein",label = "Select proteins", choices = c("All proteins","significant proteins","Proteins with pval < 0.05"))
      
    }
    
    
  })
  
  ## render proceed button for posthoc analysis
  
  output$run_posthoc_btn <- renderUI({
    if(tca_proceed_clicked()){
      
      actionButton(inputId = "tca_posthoc_submit",label = "Submit",width = '100px',style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      
    }
    
    
  })
  
  ## Timecourse posthoc reactives ##################
  
  ## upon clicking proceed, run the lmer posthoc analysis 
  lmer_posthoc_analysis <- eventReactive(input$tca_posthoc_submit,{
    print("analyzing...")
    req(input$tca_effect, input$tca_ph_protein)
    
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait while posthoc analysis is in progress. This may take a while...")
    print("analyzing 1 ....")
    
    
    lmer_r = lmer_analysis()
    
    ## Run lmer posthoc for proteins with pval < 0.05 
    if(input$tca_ph_protein == "Proteins with pval < 0.05"){
      
      lmer_rpval = lmer_r[lmer_r$p.value <= 0.05,]
      
      ph_res = run_lmer_ph(data=merge_final_data(),var=input$tca_var,rand=input$tca_rand,cov=input$tca_cov,olinkids=lmer_rpval$OlinkID,eff = input$tca_effect)
      
      
    }
    ## Run lmer posthoc for all proteins 
    else if (input$tca_ph_protein == "All proteins"){
      
      ph_res = run_lmer_ph(data=merge_final_data(),var=input$tca_var,rand=input$tca_rand,cov=input$tca_cov,olinkids=lmer_r$OlinkID,eff = input$tca_effect)
      
      ## Run lmer posthoc for only significant proteins ( proteins with adjusted pvalue < 0.05)
    } else if (input$tca_ph_protein == "significant proteins"){
      
      lmer_sig = lmer_r[lmer_r$Threshold == "Significant",]
      
      ph_res = run_lmer_ph(data=merge_final_data(),var=input$tca_var,rand=input$tca_rand,cov=input$tca_cov,olinkids=lmer_sig$OlinkID,eff = input$tca_effect)
      
      
    }
    
    remove_modal_spinner()
    return(ph_res)
    
  })
  
  ## render lmer posthoc result table 
  output$tca_table2 <- renderDT({
    
    df2 = lmer_posthoc_analysis()
    df2 = datatable(df2, options = list(scrollX=TRUE,scrollY='200px',scrollCollapse=TRUE,paging=F),style = "bootstrap",class = 'cell-border stripe',selection = 'multiple')
    
    tca_posthoc_table_shown(TRUE)
    return(df2)
  })
  
  ## ## render dropdown for choosing the contrast for heatmap
  output$tca_heatmap_dropdown <- renderUI({
    
    if(tca_posthoc_table_shown()){
      
      t = lmer_posthoc_analysis()
      selectInput(inputId = "tca_heatmap_choices", label = "Please select contrast to view heatmap",choices = unique(t$contrast),selected = NULL)
      
    }
    
    
  })
  
  ## render heatmap
    output$tca_heatmap <- renderPlotly({
      
      req(input$tca_heatmap_choices)
      ph_df = lmer_posthoc_analysis()
      p = plot_tca_heatmap(mat = merge_final_data(), ph=ph_df,cont = input$tca_heatmap_choices, varcol = input$tca_var)
      reportProd$tca_heatmap1 <- p
      p
      
      
    })

  ### ENd of Timecourse ###################
  
  
  ### EDA UI##### 
  # PCA UI#### 
  
  output$pcaUI <- renderUI({
    
    fluidPage(
      fluidRow(
        column(width = 3, selectInput(inputId = "pca_shape", label = "Shape vector",choices = colnames(final_data()$metadata))),
        bsTooltip(id = "pca_shape",title = "column in metadata to be represented as different shapes",placement = "top",trigger = "hover"),
        column(width = 3, selectInput(inputId = "pca_color", label = "Color vector", choices = colnames(final_data()$metadata))),
        bsTooltip(id = "pca_color",title = "column in metadata to be represented by different colors", placement = "top",trigger = "hover"),
        column(width=3, selectInput(inputId = "PC_1",label = "Select first PC", choices = c("1","2","3","4","5"), selected = "1")),
        bsTooltip(id = "PC_1",title = "Principal component plotted on x-axis",placement = "top",trigger = "hover"),
        column(width=3, selectInput(inputId = "PC_2", label = "Select second PC", choices=c("1","2","3","4","5"), selected = "2")),
        bsTooltip(id = "PC_2",title = "Principal component plotted on y-axis(cannot be same as PC_1)", placement = "top",trigger = "hover")
      ),
      fluidRow(
        column(width=4,
               actionButton(inputId = "pca_submit",label = "Plot"))
      ),
      fluidRow(column(width=6,plotlyOutput("pcaOut")),
               column(width=5, plotlyOutput("screeOut"))))     
    
  })
  
  
  #### Boxplot UI ##############################################
  
  output$boxpltUI <- renderUI({
    
    fluidPage(
      fluidRow(
        column(width=4, selectInput(inputId = "box_genenames", label = "Select Protein",choices = unique(final_data()$npx$UniProt))),
        bsTooltip(id = "box_genenames",title = "UniProt ID of interest",placement = "top",trigger = "hover"),
        column(width=4, selectInput(inputId = "box_group", label = "Select Group", choices = colnames(final_data()$metadata))),
        bsTooltip(id = "box_group",title = "metadata column according to which the NPX values are to be plotted",placement = "top",trigger = "hover")
      ), 
      fluidRow(
        column(width=4, 
               actionButton(inputId = "boxplt_submit",label = "Plot"))
      ),
      fluidRow(
        column(width=7,plotlyOutput("boxOut"))
      ))
    
    
    
  })
  
  ######### EDA Heatmap UI ########################
  
  output$hmUI <- renderUI({
    
    fluidPage(
      
      fluidRow(
        column(width=4, selectInput(inputId = "heatmapgroups",label = "Select group",choices = colnames(final_data()$metadata))),
        bsTooltip(id = "heatmapgroups",title = "metadata columns of interest to be plotted",placement = "top",trigger = "hover"),
        column(width=4, selectInput(inputId = "heatmap_clust",label = "Select an option for clustering",choices = c("none","row","column","both"))),
        
      ),
      fluidRow(
        actionButton(inputId = "hm_submit",label = "Plot")
      ),
      fluidRow(column(width=12,plotlyOutput("heatmap2_eda"))) %>% withSpinner(),
      
    )
    
    
  })
  
  ## convert data to wide format ## 

  mat <- reactive({
    d = final_data()$npx
    d = pivot_wider(data = d,id_cols = "SampleID",names_from = "UniProt",values_from = "NPX",values_fn = mean)
    d = as.data.frame(d)
    rownames(d) = d$SampleID
    d = d[,-1]
    d
  })
  
  
  
  
  ## Heirarchical clustering reactive ##
  hclust_plot <- eventReactive(input$clustbutton,{
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait while plotting is in progress...")
    m = mat()
    
    print("plotting...")
    req(input$dist, input$agg)
    reportProd$hclust_distMethod <- input$dist
    reportProd$hclust_aggMethod <- input$agg
    
      
    plt1 = plot_hclust(m,input$dist, input$agg)
    
    req(input$dist, input$agg)   

      
    plt1 = plot_hclust(m,input$dist, input$agg)
    
    remove_modal_spinner()
    reportProd$hclust_plot1 <- plt1
    return(plt1)
  })
  
  output$hclustOut <- renderPlotly({
    hclust_plot()
    
  })
  
  
  
  ### PAM clustering reactive ####
  pam_plot <- eventReactive(input$pam_submit,{
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait while plotting is in progress...")
    
    
    
    req(input$dist2)
    reportProd$PAM_distMethod <- input$dist2
    
    
    plt2 = plot_pam(mat(), input$dist2)
    
    
    remove_modal_spinner()
    reportProd$pam_plot1 <- plt2
    return(plt2)
  })
  
  output$pamOut <- renderPlotly({
    pam_plot()
    
  })
  
  
  ### PCA reactive ###
  pca_plot_eda <- eventReactive(input$pca_submit,{
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait while plotting is in progress...")
    m = mat()
    meta2 = final_data()$metadata
    req(input$pca_shape,input$pca_color,input$PC_1,input$PC_2)
    reportProd$pca_shape1 <- input$pca_shape
    reportProd$pca_color1 <- input$pca_color
    reportProd$PC_11 <- input$PC_1
    reportProd$PC_21 <- input$PC_2
    
    tryCatch({
    plts = plot_pca2(m, meta2,input$pca_shape, input$pca_color,as.numeric(input$PC_1), as.numeric(input$PC_2))
    }, error = function(e){
      
      shinyalert(title = "ERROR!",text = "There was an error. Please check your inputs.")
      
    }
    )
    remove_modal_spinner()
    
    return(plts)
  })
  
  ## render PCA plot 
  output$pcaOut <- renderPlotly({
    p = pca_plot_eda()
    pca = p[[1]]
    reportProd$pca_plot1 <- pca
    return(pca)
    
  })
  
  ## render scree plot 
  output$screeOut <- renderPlotly({
    p = pca_plot_eda()
    scree = p[[2]]
    reportProd$scree_plot1 <- scree
    return(scree)
    
  })
  
  
  ##### EDA Boxplot reactive #############################
  
  
  box_plot <- eventReactive(input$boxplt_submit,{
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait while plotting is in progress...")
    print("plotting")
    
    m = final_data()$npx
    meta2 = final_data()$metadata
    
    req(input$box_genenames,input$box_group)
    reportProd$box_genenames1 <- input$box_genenames
    reportProd$box_group1 <- input$box_group
    
    suppressWarnings({
      plt2 = plot_boxplt(m, meta2,input$box_genenames, input$box_group)
    })
    remove_modal_spinner()
    
    print("plotting 2.")
    reportProd$box_plot1 <- plt2
    return(plt2)
  })
  
  output$boxOut <- renderPlotly({
    
    bx = box_plot()
    return(bx)
  })  
  
  ## EDA Heatmap reactive ###############
  
  
  heatmap1_eda <- eventReactive(input$hm_submit,{
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait while plotting is in progress...")
    
    
    req(input$heatmapgroups, input$heatmap_clust)
    
    reportProd$heatmapgroups1 <- input$heatmapgroups
    reportProd$heatmap_clust1 <- input$heatmap_clust
    
    mat = mat()
    metadata = final_data()$metadata
    
    req(input$heatmapgroups)
    
    rownames(metadata) = metadata$SampleID
    metadata = metadata[rownames(mat),]
    metadata = metadata[order(metadata[[input$heatmapgroups]]),]
    datat = t(mat)
    colSide = as.factor(metadata[[input$heatmapgroups]])
    plt3 = heatmaply(x = datat ,xlab = "SampleID",ylab = "Protein",col_side_colors = colSide,dendrogram = input$heatmap_clust)
    remove_modal_spinner()
    reportProd$heatmap1_eda1<-plt3
    return(plt3)
    
  })
  
  output$heatmap2_eda <-renderPlotly({
    
    hm = heatmap1_eda()
    hm
  })
  
  #### EDA Correlation plot UI###
  output$correlationedaUI <- renderUI({
    fluidPage(column(width=12,
                     box(
                       HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
  <h2>Correlation</h2>
  <p>Correlation is calculated between any 2 numeric vectors in the dataset. For example, NPX columns of any 2 proteins.(Reference: <a href='https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/cor'> Correlation vignette</a>)</p>
  </div>"),width = 12,collapsible = T))
              ,
              shinyjs::useShinyjs(),
              fluidRow(
                       box(column(width = 5,
                                  selectInput(
                                    inputId = "corplot1",
                                    label = "Select X axis",
                                    choices = names(merge_final_data()),selected = "NPX",
                                    selectize = F
                                  )),
                           column(width = 5,selectInput(
                             inputId = "corplot2",
                             label = "Select Y axis",
                             choices = names(merge_final_data()),selected = "NPX",
                             selectize = F
                           )),
                           column(width = 5,selectInput(
                             inputId = "corplot9",
                             label = "Select feature for coloring",
                             choices = names(merge_final_data()),selected = "SampleID",
                             selectize = F
                           ))
                       )),
              fluidRow(box(
                column(width = 5,pickerInput(
                  inputId = "corplot3",
                  label = "Select X axis Value",
                  choices = c(""),
                  selected = ""
                )),
                column(width = 5,pickerInput(
                  inputId = "corplot4",
                  label = "Select Y axis Value",
                  choices = c(""),
                  selected = ""
                ))
              )),
              fluidRow(plotlyOutput("cor1_plot",width = "80%") %>% withSpinner()))
#              fluidRow(downloadButton("download10", "Download")))
  })
    #           fluidRow(h4("Correlation plot 2"),
    #                    fluidRow(
    #                      box(
    #                        column(
    #                          width = 5,
    #                          selectInput(
    #                            inputId = "corplot5",
    #                            label = "Select X axis",
    #                            choices = names(merge_final_data()),selected = "NPX",
    #                            selectize = F
    #                          )
    #                        ),
    #                        column(
    #                          width = 5,
    #                          selectInput(
    #                            inputId = "corplot6",
    #                            label = "Select Y axis",
    #                            choices = names(merge_final_data()),selected = "NPX",
    #                            selectize = F
    #                          )
    #                        )
    #                      )),
    #                    fluidRow(box(
    #                      column(
    #                        width = 5,
    #                        pickerInput(
    #                          inputId = "corplot7",
    #                          label = "Select X axis Value",
    #                          choices = "",
    #                          selected = ""
    #                        )
    #                      ),
    #                      column(
    #                        width = 5,
    #                        pickerInput(
    #                          inputId = "corplot8",
    #                          label = "Select Y axis Value",
    #                          choices = "", 
    #                          selected = ""
    #                        )
    #                      )
    #                    )),
    #                    fluidRow(plotlyOutput("cor2_plot", width = "80%") %>% withSpinner()),
    #                    fluidRow(downloadButton("download11", "Download"))
    #           ),
    #           fluidRow(style = 'padding-left:28px; padding-right:5px; padding-top:5px; padding-bottom:10px',
    #                    htmlOutput("message4")), 
    #           fluidRow(style = 'padding-left:28px; padding-right:5px; padding-top:5px; padding-bottom:10px',
    #                    htmlOutput("message5"))
    #           
    # )
  
  ########################## Correlation reactive ##################################################
  corel_plot <- reactive({
    req(input$corplot1)
    req(input$corplot2)
    # req(input$corplot3)
    # req(input$corplot4)
    
    
    P1<-correlation_plot1(final_data()$npx,
                          final_data()$metadata,
                          input$corplot1,
                          input$corplot2,
                          input$corplot3,
                          input$corplot4,
                          input$corplot9)
    reportProd$corel_plot1 <- P1
    return(P1)
      
  })
  # 
  # corel_plot2 <- reactive({
  #   req(input$corplot5)
  #   req(input$corplot6)
  #   # req(input$corplot7)
  #   # req(input$corplot8)
  #   P1<-correlation_plot2(final_data()$npx,
  #                         final_data()$metadata,
  #                         input$corplot5,
  #                         input$corplot6,
  #                         input$corplot7,
  #                         input$corplot8,
  #                         Panel = input$Panel_cor
  #   )
  #   P1
  # 
  # })

  # 
  #### correl plot 2 filters ####

  assays_list <- reactive({
    asylist<<- final_data()[[1]]$Assay
    asylist2<<-unique(asylist)
    asylist2
  })

  # observeEvent(input$corplot5, {
  #   updatePickerInput(
  #     session,
  #     inputId = "corplot7",
  #     label = "Select X axis Value",
  #     choices =   assays_list(),
  #     selected = "",
  #     options = pickerOptions(liveSearch = T, dropupAuto = FALSE)
  #   )
  # 
  # 
  # })

  # observeEvent(input$corplot6,{
  #   updatePickerInput(
  #     session,
  #     inputId = "corplot8",
  #     label = "Select Y axis Value",
  #     choices =   assays_list(),
  #     selected = "",
  #     options = pickerOptions(liveSearch = T, dropupAuto = FALSE)
  #   )
  # })
  # 
  # observeEvent(input$corplot2, {
  #   if (input$corplot2 %in% names(final_data()$metadata)) {
  #     disable("corplot4")
  #   } else{
  #     enable("corplot4")
  #   }
  # })
  # 
  # observeEvent(input$corplot5, {
  #   if (input$corplot5 %in% names(final_data$metadata)) {
  #     disable("corplot7")
  #   } else{
  #     enable("corplot7")
  #   }
  # })
  # 
  # observeEvent(input$corplot6, {
  #   if (input$corplot6 %in% names(final_data$metadata)) {
  #     disable("corplot8")
  #   } else{
  #     enable("corplot8")
  #   }
  # })


  # 
  # #### corelation plot ####
  # 
  output$cor1_plot<-renderPlotly(corel_plot())

  

  ###### MAchine learning UI ################################################################################
  
  ## Set a reactive value to aggregate results from this module 
  
  ML_results <- reactiveValues(SVM = NULL, XGB = NULL, RF = NULL, PLSDA = NULL, sPLSDA = NULL, KNN = NULL, LMM = NULL)

  ## PLS UI
  output$plsUI <- renderUI({
    fluidPage(
      column(width=12,
             box(collapsible = TRUE,width = 12,HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
                             <h2> Partial Least Squares</h2>
         <p>PLS is a supervised machine learning algorithm can be used of regression as well classification tasks. The main objective of this algorithm is to find latent variables (components) that capture the maximum covariance between the predictor variables and the class labels. In the current implementation, PLS is first tuned to identify the best parameter - <i>ncomp</i>. Optimal features are selected using sbf ( selection by filtering ) using pls as the model. The best parameter and features are used for training the final model. (Reference: <a href='topepo.github.io/caret/train-models-by-tag.html#partial-least-squares'>Partial Least Square Vignette</a>)</p></div>"))),
      fluidRow(
        column(width=4,
               selectInput(inputId = "pls_column",label = "Select dependent variable for classification",choices = names(final_data()$metadata[, sapply(final_data()$metadata, function(col) length(unique(col))) < nrow(final_data()$metadata)])),
               bsTooltip(id = "pls_column",title ="Class label/Dependent variable")
               ),
              
        column(width=2,
               textInput(inputId = "pls_perc",label = "Train percentage",value = 0.8,width = '120px'),
               bsTooltip(id = "pls_perc",title ="Train data split percentage. Default value: 0.8")
              ),
        column(width = 3, selectInput(inputId = "pls_fsel",label = "Select feature selection method",choices = c("sbf")),
               bsTooltip(id = "pls_fsel",title ="Selection by filtering")
               )
      ),
      fluidRow(
        column(width=4,
               actionButton(inputId = "submit_pls",label = "Submit"))
      ),
      br(),
      conditionalPanel(condition = "input.submit_pls > 0",
                       tabsetPanel(
                         tabPanel(title = "Model metrics",
                                  fluidRow(
                                    column(width=12,verbatimTextOutput("pls_text") %>% withSpinner())
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width=5, plotOutput("pls_cm_plot")),
                                    column(width=5, plotOutput("pls_roc"))
                                  )
                                  
                         ),
                         tabPanel(title = "Features",
                                  fluidRow(
                                    column(width=4, 
                                           selectInput(inputId = "pls_vi",label = "Select number of proteins to display",choices = c(10,20,30,40)))
                                  ),
                                  fluidRow(
                                    column(width=8,plotOutput("pls_imp"))
                                  ))
                       ))
      
    )
    
  })
  
  ## PLS Reactive 
  run_pls <- eventReactive(input$submit_pls,{
    
    req(input$pls_column, input$pls_perc, input$pls_fsel)
    
    reportProd$pls_column1 <- input$pls_column
    reportProd$pls_perc1 <- input$pls_perc
    reportProd$pls_fsel1 <- input$pls_fsel
    
    print("Preprocessing data...")
    df_pls <- prep_data(data = final_data()$npx,metadata = final_data()$metadata,group = input$pls_column)
    print("Preprocessing complete...")      

    print("Splitting data...")
    dataset_list <- split_data(data = df_pls,trainperc = input$pls_perc,group = input$pls_column)
    print("Splitting complete...")
    
    print("Tuning pls model...")
    cvmdl <- pls_model_tuning(traindx = dataset_list[[9]],traindy = dataset_list[[10]],testdx = dataset_list[[11]],group=input$pls_column)
    print("Tuning model complete...")    
    
    print("Selecting features...")
    cvsbf <- pls_feature_sel(traindx = dataset_list[[9]],traindy = dataset_list[[10]],group=input$pls_column)
    print("Selecting features complete...")
    
    print("Training pls with best parameters and best features...")
    final_model <- train_pls(traindx = dataset_list[[5]],traindy = dataset_list[[6]],group=input$pls_column,mdl1=cvmdl,mdl2=cvsbf)
    print("Training pls complete...")
    
    print("Generating confusion matrix...")
    cfmplot <- conf_matrix(model=final_model,group=input$pls_column,testd=dataset_list[[2]],mdl1=cvsbf)
    
    print("plotting roc curve...")
    rocplt <- plot_roc(model=final_model,testd = dataset_list[[2]], group=input$pls_column, mdl1=cvsbf)
    rocplt1 <- rocplt[[1]]
    
    ML_results$PLSDA = list(final_model,rocplt[[2]])
    
    return(list(final_model, cfmplot, rocplt1))
  })
  
  ## PLS Output
  output$pls_imp <- renderPlot({
    req(input$pls_vi)
    d <- run_pls()[[1]]
    tryCatch(
      expr = {
        plt = plot_imp2(model=d,n= input$pls_vi)
        reportProd$pls_imp1 <- plt
        plt
      },
      error = function(e){
        plt= plot_imp(model=d,n= input$pls_vi)
        reportProd$pls_imp1 <- plt
        plt
      }
    )
    
  })
  
  output$pls_roc <- renderPlot({
    reportProd$pls_roc1 <- run_pls()[[3]]
    run_pls()[[3]]
  })
  
  output$pls_text <- renderPrint({
    reportProd$pls_text1 <- run_pls()[[1]]
    run_pls()[[1]]
  })
  
  output$pls_cm_plot <- renderPlot({
    cf <- run_pls()[[2]]
    reportProd$pls_cm_plot1 <- cf
    cf
  })  
  
  ## SPLS UI
  output$splsUI <- renderUI({
    fluidPage(
      column(width=12,
             box(collapsible = TRUE,width = 12,HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
                             <h2> Sparse Partial Least Squares </h2>
         <p>SPLS is a supervised machine learning algorithm can be used of regression as well classification tasks. SPLS-DA aims to find a linear combination of the predictors that best discriminates between different classes while also promoting sparsity, meaning it selects only a subset of the predictors that are most relevant for classification. In the current implementation, first optimal features are selected using sbf ( selection by filtering ) using pls as the model. With the selected feature SPLS is fine tune to get the best parameters - <i>K</i> and <i>eta</i>. The best parameters and features are used for training the final model. (Reference: <a href='topepo.github.io/caret/train-models-by-tag.html#partial-least-squares'>Sparse Partial Least Square Vignette</a>)</p></div>"))),
      fluidRow(
        column(width=4,
               selectInput(inputId = "spls_column",label = "Select variable for classification",choices = names(final_data()$metadata[, sapply(final_data()$metadata, function(col) length(unique(col))) < nrow(final_data()$metadata)])),
               bsTooltip(id = "spls_column",title ="Class label/Dependent variable")
              ),
        column(width=2,
               textInput(inputId = "spls_perc",label = "Train percentage",value = 0.8,width = '120px'),
               bsTooltip(id = "spls_perc",title ="Train data split percentage. Default value: 0.8")
              ),
        column(width = 3, selectInput(inputId = "spls_fsel",label = "Select feature selection method",choices = c("sbf")),
               bsTooltip(id = "spls_fsel",title ="Selection by filtering")
              )
      ),
      fluidRow(
        column(width=4,
               actionButton(inputId = "submit_spls",label = "Submit"))
      ),
      br(),
      conditionalPanel(condition = "input.submit_spls > 0",
                       tabsetPanel(
                         tabPanel(title = "Model metrics",
                                  fluidRow(
                                    column(width=12,verbatimTextOutput("spls_text") %>% withSpinner())
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width=5, plotOutput("spls_cm_plot")),
                                    column(width=5, plotOutput("spls_roc"))
                                  )
                                  
                         ),
                         tabPanel(title = "Features",
                                  fluidRow(
                                    column(width=4, 
                                           selectInput(inputId = "spls_vi",label = "Select number of proteins to display",choices = c(10,20,30,40)))
                                  ),
                                  fluidRow(
                                    column(width=8,plotOutput("spls_imp"))
                                  ))
                       ))
      
    )
    
  })
  
  ## SPLS Reactive 
  run_spls <- eventReactive(input$submit_spls,{
    
    req(input$spls_column, input$spls_perc, input$spls_fsel)
    
    reportProd$spls_column1 <- input$spls_column
    reportProd$spls_perc1 <- input$spls_perc
    reportProd$spls_fsel1 <- input$spls_fsel
    
    print("Preprocessing data...")
    df_spls <- prep_data(data = final_data()$npx,metadata = final_data()$metadata,group = input$spls_column)
    print("Preprocessing complete...")      
    
    print("Splitting data...")
    dataset_list <- split_data(data = df_spls,trainperc = input$spls_perc,group = input$spls_column)
    print("Splitting complete...")
    
    print("Selecting features...")
    cvsbf <- spls_feature_sel(traindx = dataset_list[[9]],traindy = dataset_list[[10]],group=input$spls_column)
    print("Selecting features complete...")
    
    print("Tuning spls model with best features...")
    cvmdl <- spls_model_tuning(traindx = dataset_list[[9]],traindy = dataset_list[[10]],testdx = dataset_list[[11]],group=input$spls_column,mdl1=cvsbf)
    print("Tuning model complete...")    
  
    print("Training spls with best parameters and best features...")
    final_model <- train_spls(traindx = dataset_list[[5]],traindy = dataset_list[[6]],group=input$pls_column,mdl1=cvsbf,mdl2=cvmdl)
    print("Training spls complete...")
    
    print("Generating confusion matrix...")
    cfmplot <- conf_matrix_spls(model=final_model,group=input$spls_column,testd=dataset_list[[2]],mdl1=cvsbf)
    
    print("plotting roc curve...")
    rocplt <- plot_roc(model=final_model,testd = dataset_list[[2]], group=input$spls_column,mdl1=cvsbf)
    rocplt1 = rocplt[[1]]
    
    
    ML_results$sPLSDA = list(final_model,rocplt[[2]])
    
    return(list(final_model, cfmplot, rocplt1))
  })
  
  ## SPLS Output
  output$spls_imp <- renderPlot({
    req(input$spls_vi)
    d <- run_spls()[[1]]
    plt <- plot_imp(model=d,n= input$spls_vi)
    reportProd$spls_imp1 <- plt
    plt
  })
  
  output$spls_roc <- renderPlot({
    reportProd$spls_roc1 <- run_spls()[[3]]
    run_spls()[[3]]
  })
  
  output$spls_text <- renderPrint({
    reportProd$spls_text1 <- run_spls()[[1]]
    run_spls()[[1]]
  })
  
  output$spls_cm_plot <- renderPlot({
    cf <- run_spls()[[2]]
    reportProd$spls_cm_plot1 <- cf
    cf
  })  
  
  ## GLMM UI
  output$glmmUI <- renderUI({
    fluidPage(
      column(width=12,
             box(collapsible = TRUE,width = 12,HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
                             <h2> Generalised Linear Mixed Effects Models </h2>
         <p>GLMMs are a type of statistical model used for analyzing grouped or clustered data, where observations are not independent but are instead nested within groups or clusters. GLMMs extend the framework of Generalized Linear Models (GLMs) by incorporating random effects, which account for the correlation structure within groups. In the current implementation, first optimal features are selected using sbf ( selection by filtering ) using glm as the model. The selected features are used for training the glmer final model.</p> <b>NOTE:</b> GLM models can only use 2-class outcomes i.e for binary classification task. (Reference: <a href='https://www.rdocumentation.org/packages/lme4/versions/1.1-35.1/topics/glmer'>Generalized Linear Mixed-Effects Model Vignette</a>)</div>"))),
      fluidRow(
        column(width=4,
               selectInput(inputId = "glmm_column",label = "Select variable for classification",
                           choices = names(final_data()$metadata[, sapply(final_data()$metadata, function(col) length(unique(col))) < nrow(final_data()$metadata)])),
               bsTooltip(id = "glmm_column",title ="Class label/Dependent variable")
               ),
        column(width=4,
               selectizeInput(inputId = "glmm_random",
                              label = "Select variables for random effects",
                              choices = names(final_data()$metadata)[sapply(final_data()$metadata, function(col) length(unique(col)) > 1 & length(unique(col)) < nrow(final_data()$metadata))],
                              multiple = TRUE,
                              selected = NULL),
               bsTooltip(id = "glmm_random",title ="Random effects variables represent the random factors or grouping variables in the model.")
        ),
        column(width=2,
               textInput(inputId = "glmm_perc",label = "Train percentage",value = 0.8,width = '120px'),
               bsTooltip(id = "glmm_perc",title ="Train data split percentage. Default value: 0.8")
               ),
        column(width = 3, selectInput(inputId = "glmm_fsel",label = "Select feature selection method",choices = c("sbf")),
               bsTooltip(id = "glmm_fsel",title ="Selection by filtering")
              )
      ),
      fluidRow(
        column(width=4,
               actionButton(inputId = "submit_glmm",label = "Submit"))
      )
      ,
      br(),
      conditionalPanel(condition = "input.submit_glmm > 0",
                       tabsetPanel(
                         tabPanel(title = "Model metrics",
                                  fluidRow(
                                    column(width=12,verbatimTextOutput("glmm_text") %>% withSpinner())
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width=5, plotOutput("glmm_cm_plot")),
                                    column(width=5, plotOutput("glmm_roc"))
                                  )
                                  
                         ),
                         tabPanel(title = "Features",
                                  fluidRow(
                                    column(width=4, 
                                           selectInput(inputId = "glmm_vi",label = "Select number of proteins to display",choices = c(10,20,30,40)))
                                  ),
                                  fluidRow(
                                    column(width=8,plotOutput("glmm_imp"))
                                  ))
                       ))
      
    )
  })

  ## GLMM Reactive 
  run_glmm <- eventReactive(input$submit_glmm,{
    if(is.null(input$glmm_random)){
      shinyalert(
        title = "Warning",
        text = paste("Please select atleast one variable for random effects."),
        type = "warning"
      )
    }
    else{
      req(input$glmm_column, input$glmm_perc, input$glmm_fsel)
      print(unique(final_data()$metadata[[input$glmm_column]]))
      
      # Check Class variable
      if(length(unique(final_data()$metadata[[input$glmm_column]])) == 2){
        if(!(input$glmm_column %in% input$glmm_random)){
          print("Preprocessing data...")
          df_glmm <- prep_data2(data = final_data()$npx,metadata = final_data()$metadata,group = input$glmm_column,random = input$glmm_random)
          print("Preprocessing complete...")      
          
          print("Splitting data...")
          dataset_list <- split_data2(data = df_glmm,trainperc = input$glmm_perc,group = input$glmm_column,random = input$glmm_random)
          print("Splitting complete...")
          
          print("Selecting features...")
          cvsbf <- glmm_feature_sel(traindx = dataset_list[[3]],traindy = dataset_list[[4]],group=input$glmm_column)
          print("Selecting features complete...")
          
          print("Training glmer with best features...")
          final_model <- train_glmm(traind = dataset_list[[1]],group=input$glmm_column,random = input$glmm_random,mdl1=cvsbf)
          print("Training glmer complete...")
          
          print("Generating confusion matrix...")
          cfmplot <- conf_matrix2(model=final_model,group=input$glmm_column,traind=dataset_list[[1]],testd=dataset_list[[2]],mdl1=cvsbf,random=input$glmm_random)
          
          print("plotting roc curve...")
          rocplt <- plot_roc2(model=final_model,testd = dataset_list[[2]], group=input$glmm_column,mdl1=cvsbf,random = input$glmm_random)
          rocplt1 <- rocplt[[1]]
          ML_results$LMM = list(final_model, rocplt[[2]])
    
          return(list(final_model, cfmplot, rocplt1))
        }
        else{
          shinyalert(
            title = "Random effect variable is same as classification variable.",
            text = "Random effect variable should not be same as classification variable.",
            type = "warning"
          )
        }

      }
      else{
        shinyalert(
          title = "GLMM works for a binary classification problem.",
          text = paste("The variable ",input$glmm_column," is not binary in nature. It has the following classe label:",paste(unique(final_data()$metadata[[input$glmm_column]]), collapse = ", ")),
          type = "warning"
        )
      }
      
    }
  })
    
    
  
  ## GLMM Output
  output$glmm_roc <- renderPlot({
    reportProd$glmm_roc1 <- run_glmm()[[3]]
    run_glmm()[[3]]
    
  })
  
  output$glmm_text <- renderPrint({
    reportProd$glmm_text1 <- run_glmm()[[1]]
    run_glmm()[[1]]
  })
  
  output$glmm_cm_plot <- renderPlot({
    cf = run_glmm()[[2]]
    reportProd$glmm_cm_plot1 <- cf
    cf
  })  
  
  output$glmm_imp <- renderPlot({
    req(input$glmm_vi)
    d = run_glmm()[[1]]
    plt = plot_imp3(model=d,n=input$glmm_vi)
    reportProd$glmm_imp1 <- plt
    plt
  })  
  
  ## KNN UI
  output$knnUI <- renderUI({
    fluidPage(
      column(width=12,
             box(collapsible = TRUE,width = 12,HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
                             <h2> K-Nearest Neighbors </h2>
         <p>KNN is a supervised machine learning algorithm can be used of regression as well classification tasks. The primary principle behind KNN is to predict the class of a data point by considering the majority class of its neighboring data points. In the current implementation, KNN is first tuned to identify the best parameter - <i>k</i>. Optimal features are selected using sbf ( selection by filtering ) using knn as the model. The best parameter and features are used for training the final model. (Reference: <a href='https://topepo.github.io/caret/miscellaneous-model-functions.html#yet-another-k-nearest-neighbor-function'>K-Nearest Neighbour Vignette</a>)</p></div>"))),
      fluidRow(
        column(width=4,
               selectInput(inputId = "knn_column",label = "Select variable for classification",choices = names(final_data()$metadata[, sapply(final_data()$metadata, function(col) length(unique(col))) < nrow(final_data()$metadata)])),
               bsTooltip(id = "knn_column",title ="Class label/Dependent variable")
               ),
        column(width=2,
               textInput(inputId = "knn_perc",label = "Train percentage",value = 0.8,width = '120px'),
               bsTooltip(id = "knn_perc",title ="Train data split percentage. Default value: 0.8")
               ),
        column(width = 3, selectInput(inputId = "knn_fsel",label = "Select feature selection method",choices = c("sbf")),
               bsTooltip(id = "knn_fsel",title ="Selection by filtering")
               )
      ),
      fluidRow(
        column(width=4,
               actionButton(inputId = "submit_knn",label = "Submit"))
      ),
      br(),
      conditionalPanel(condition = "input.submit_knn > 0",
                       tabsetPanel(
                         tabPanel(title = "Model metrics",
                                  fluidRow(
                                    column(width=12,verbatimTextOutput("knn_text") %>% withSpinner())
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width=5, plotOutput("knn_cm_plot")),
                                    column(width=5, plotOutput("knn_roc"))
                                  )
                                  
                         ),
                         tabPanel(title = "Features",
                                  fluidRow(
                                    column(width=4, 
                                           selectInput(inputId = "knn_vi",label = "Select number of proteins to display",choices = c(10,20,30,40)))
                                    ),
                                  fluidRow(
                                    column(width=8,plotOutput("knn_imp"))
                                  ))
                       ))
      
    )
    
  })
  
  ## KNN Reactive 
  run_knn <- eventReactive(input$submit_knn,{
    
    req(input$knn_column, input$knn_perc, input$knn_fsel)
    
    reportProd$knn_column1 <- input$knn_column
    reportProd$knn_perc1 <- input$knn_perc
    reportProd$knn_fsel1 <- input$knn_fsel
    
    print("Preprocessing data...")
    df_knn <- prep_data(data = final_data()$npx,metadata = final_data()$metadata,group = input$knn_column)
    print("Preprocessing complete...") 
    
    print("Splitting data...")
    dataset_list <- split_data(data = df_knn,trainperc = input$knn_perc,group = input$knn_column)
    print("Splitting complete...")
    
    print("Tuning knn model...")
    cvmdl <- knn_model_tuning(traindx = dataset_list[[9]],traindy = dataset_list[[10]],testdx = dataset_list[[11]],group=input$knn_column)
    print("Tuning model complete...")    
    
    print("Selecting features...")
    cvsbf <- knn_feature_sel(traindx = dataset_list[[9]],traindy = dataset_list[[10]],group=input$knn_column)
    print("Selecting features complete...")
    
    print("Training knn with best parameters and best features...")
    final_model <- train_knn(traindx = dataset_list[[5]],traindy = dataset_list[[6]],group=input$knn_column,mdl1=cvmdl,mdl2=cvsbf)
    print("Training knn complete...")
    
    print("Generating confusion matrix...")
    cfmplot <- conf_matrix(model=final_model,group=input$knn_column,testd=dataset_list[[2]],mdl1=cvsbf)
    
    print("plotting roc curve...")
    rocplt = plot_roc(model=final_model,testd = dataset_list[[2]], group=input$knn_column, mdl1=cvsbf)
    rocplt1 = rocplt[[1]]
    
    ML_results$KNN = list(final_model, rocplt[[2]])
    return(list(final_model, cfmplot, rocplt1))
  })
  
  ## KNN Output
  output$knn_imp <- renderPlot({
    req(input$knn_vi)
    d = run_knn()[[1]]
    plt = plot_imp(model=d,n= input$knn_vi)
    reportProd$knn_imp1 <- plt
    plt
  })
  
  output$knn_roc = renderPlot({
    reportProd$knn_roc1 <- run_knn()[[3]]
    run_knn()[[3]]
  })
  
  output$knn_text = renderPrint({
    reportProd$knn_text1 <- run_knn()[[1]]
    run_knn()[[1]]
  })
  
  output$knn_cm_plot <- renderPlot({
    cf = run_knn()[[2]]
    reportProd$knn_cm_plot1 <- cf
    cf
  }) 
  
  ## SVM UI 
  output$svmUI <- renderUI({
    fluidPage(
      column(width=12,
      box(collapsible = TRUE,width = 12,HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
                             <h2> Support Vector Machine </h2>
         <p>SVM is a supervised machine learning algorithm best suited for classification. The main objective of this algorithm is to find the optimal hyperplane in an N-dimensional space that can separate the data points in different classes in the feature space. In the current implementation, <b> Linear SVM kernel</b> is first tuned to identify best <i>Cost</i> parameter, then optimal features are selected using sbf ( selection by filtering ) using svmLinear as the model. The best parameters and features are used for training the final model.(Reference: <a href='https://topepo.github.io/caret/train-models-by-tag.html#support-vector-machines'>SVM vignette</a>)</p></div>"))),
      
            fluidRow(
        column(width=4,
               selectInput(inputId = "svm_column",label = "Select variable for classification",choices = names(final_data()$metadata))),
        column(width=2,
               textInput(inputId = "svm_perc",label = "Train percentage",value = 0.8,width = '120px')),
        column(width = 3, selectInput(inputId = "svm_fsel",label = "Select feature selection method",choices = c("sbf")))
      ),
      fluidRow(
        column(width=4,
               actionButton(inputId = "submit_svm",label = "Submit"))
      )
      ,
      br(),
      conditionalPanel(condition = "input.submit_svm > 0",
                       tabsetPanel(
                         tabPanel(title = "Model metrics",
                                  fluidRow(
                                    column(width=12,verbatimTextOutput("svm_text") %>% withSpinner())
                                    
                                    
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width=5, plotOutput("svm_cm_plot")),
                                    column(width=5, plotOutput("svm_roc"))
                                    
                                  )
                                 
                          ),
                         tabPanel(title = "Features",
                                  fluidRow(
                                    
                                    column(width=4, 
                                           selectInput(inputId = "svm_vi",label = "Select number of proteins to display",choices = c(10,20,30,40)))
                                    
                                  ),
                                  fluidRow(
                                    column(width=8,plotOutput("svm_imp"))
                                  ))
                       ))
        
      )

  })
    
    ### SVM reactive #######################
  
    
    run_svm <- eventReactive(input$submit_svm,{
      
      req(input$svm_column, input$svm_perc, input$svm_fsel)
      reportProd$svm_column1 <- input$svm_column
      reportProd$svm_perc1 <- input$svm_perc
      reportProd$svm_fsel1 <- input$svm_fsel
      
      print("prepping data....")
      
      df_svm = prep_data(data = final_data()$npx,metadata = final_data()$metadata,group = input$svm_column)

      print("prepping data complete...")      
      
      print("splitting data....")
      dataset_list = split_data(data = df_svm,trainperc = input$svm_perc,group = input$svm_column)
      print("splitting data complete....")
      
      print("tuning svm model....")
      cvmdl = model_tuning(traind = dataset_list[[3]],testd = dataset_list[[4]],group=input$svm_column)
      print("tuning model complete..")    
      
      print("selecting features...")
      cvsbf = feature_sel(traind=dataset_list[[3]], testd=dataset_list[[4]],group=input$svm_column)
      print("selecting features complete")
      
      
      print("training svm with best parameters and best features...")
      final_model = train_svm(traind = dataset_list[[1]],testd=dataset_list[[2]],group=input$svm_column,mdl1=cvmdl,mdl2=cvsbf)
      print("training svm complete...")
      
      print("generating confusion matrix...")
      cfmplot = conf_matrix(model=final_model,group=input$svm_column,testd=dataset_list[[2]],mdl1 = cvsbf)
      
      print("plotting roc curve...")
      rocplt = plot_roc(model=final_model,testd = dataset_list[[2]], group=input$svm_column, mdl1 = cvsbf)
      rocplt1 = rocplt[[1]]
      
      ML_results$SVM = list(final_model, rocplt[[2]])
      
      return(list(final_model, cfmplot, rocplt))
      
      
    })
    
  
  ## render importance plot
  output$svm_imp <- renderPlot({
    req(input$svm_vi)
    #n = as.numeric(input$svm_vi)
    d = run_svm()[[1]]
    plt = plot_imp(model=d,n= input$svm_vi)
    reportProd$svm_imp1 <- plt
    plt
    
  })
  

  ## render roc plot 

  output$svm_roc = renderPlot({
    reportProd$svm_roc1 <- run_svm()[[3]]
    run_svm()[[3]]
    
    
    
  })
  
  ## render model statistics 
  
  output$svm_text = renderPrint({
    reportProd$svm_text1 <- run_svm()[[1]]
    run_svm()[[1]]
    
  })
  
  ## render confusion matrix
  output$svm_cm_plot <- renderPlot({
    
    cf = run_svm()[[2]]
    reportProd$svm_cm_plot1 <- cf
    cf
    
    
  })  
  
  
  
  ## XgBoost UI 
  
  output$xgbUI <- renderUI({
    fluidPage(
      column(width=12,
             box(collapsible = TRUE,width = 12,HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
                             <h2> eXtreme gradient Boosting </h2>
         <p>XGboost is an efficient and scalable implementation of gradient boosting framework.In the current implementation, Xgboost is first tuned to identify best parameters - <i>nrounds,max_depth,eta, gamma,colsample_bytree,min_child_weight,subsample</i> optimal features are selected using sbf ( selection by filtering ) using Xgboost as the model. The best parameters and features are used for training the final model.(Reference: <a href='https://topepo.github.io/caret/train-models-by-tag.html#boosting'>Xgboost vignette</a>)</p>
                                                    </div>"))),
      
      fluidRow(
        column(width=4, 
               selectInput(inputId = "xgb_column",label = "Select variable for classification",choices = names(final_data()$metadata))),
        column(width=2,
               textInput(inputId = "xgb_perc",label = "Train percentage",value = 0.8,width = '120px')),
        column(width = 3, selectInput(inputId = "xgb_fsel",label = "Select feature selection method",choices = c("sbf")))
      ),
      fluidRow(
        column(width=4,
               actionButton(inputId = "submit_xgb",label = "Submit"))
      )
      ,
      br(),
      conditionalPanel(condition = "input.submit_xgb > 0",
                       tabsetPanel(
                         tabPanel(title = "Model metrics",
                                  fluidRow(
                                    column(width=12,verbatimTextOutput("xgb_text") %>% withSpinner())
                                    
                                    
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width=5, plotOutput("xgb_cm_plot")),
                                    column(width=5, plotOutput("xgb_roc"))
                                    
                                  )
                                  
                         ),
                         tabPanel(title = "Features",
                                  fluidRow(
                                    
                                    column(width=4, 
                                           selectInput(inputId = "xgb_vi",label = "Select number of proteins to display",choices = c(10,20,30,40)))
                                    
                                  ),
                                  fluidRow(
                                    column(width=8,plotOutput("xgb_imp"))
                                  ))
                       ))
      
    )
    
  })
  
  ## Xgboost reactive 
  
  run_xgb <- eventReactive(input$submit_xgb,{
    
    req(input$xgb_column, input$xgb_perc, input$xgb_fsel)
    
    reportProd$xgb_column1 <- input$xgb_column
    reportProd$xgb_perc1 <- input$xgb_perc
    reportProd$xgb_fsel1 <- input$xgb_fsel
    
    print("prepping data....")
    
    df_xgb = prep_data(data = final_data()$npx,metadata = final_data()$metadata,group = input$xgb_column)
    
    print("prepping data complete...")      
    
    print("splitting data....")
    dataset_list = split_data(data = df_xgb,trainperc = input$xgb_perc,group = input$xgb_column)
    print("splitting data complete....")
    
    print("tuning Xgboost model....")
    cvmdl = xgb_model_tuning(traind = dataset_list[[3]],testd = dataset_list[[4]],group=input$xgb_column)
    print("tuning model complete..")    
    
    print("selecting features...")
    cvsbf = xgb_feature_sel(traind=dataset_list[[3]], testd=dataset_list[[4]],group=input$xgb_column)
    print("selecting features complete")
    
    
    print("training Xgboost with best parameters and best features...")
    final_model = train_xgb(traind = dataset_list[[1]],testd=dataset_list[[2]],group=input$xgb_column,mdl1=cvmdl,mdl2=cvsbf)
    print("training Xgboost complete...")
    
    print("generating confusion matrix...")
    cfmplot = conf_matrix(model=final_model,group=input$xgb_column,testd=dataset_list[[2]],mdl1 = cvsbf)
    
    print("plotting roc curve...")
    rocplt = plot_roc(model=final_model,testd = dataset_list[[2]], group=input$xgb_column, mdl1 = cvsbf)
    rocplt1 = rocplt[[1]]
    
    ML_results$XGB = list(final_model, rocplt[[2]])
    
    return(list(final_model, cfmplot, rocplt1))
    
    
  })
  
  output$xgb_imp <- renderPlot({
    req(input$xgb_vi)
    
    d = run_xgb()[[1]]
    plt = xgb_plot_imp(model=d,n= input$xgb_vi)
    reportProd$xgb_imp1 <- plt
    plt
    
  })

  output$xgb_roc = renderPlot({
    reportProd$xgb_roc1 <- run_xgb()[[3]]
    run_xgb()[[3]]
    
    
    
  })
  
  output$xgb_text = renderPrint({
    reportProd$xgb_text1 <- run_xgb()[[1]]
    run_xgb()[[1]]
    
  })
  
  

  output$xgb_cm_plot <- renderPlot({
    
    cf = run_xgb()[[2]]
    reportProd$xgb_cm_plot1 <- cf
    cf
    
    
  })  
  
  
  
  ##### Random Forest UI #################
  
  output$rfUI <- renderUI({
    fluidPage(
      column(width=12,
             box(collapsible = TRUE,width = 12,HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
                             <h2> Random Forest </h2>
         <p>Random Forest is an ensemble learning method that constructs decision trees during training and outputs the mode or mean of the classes. In the current implementation, the model <i> rf </i> is first tuned to identify best parameters - <i>mtry</i>, optimal features are selected using sbf ( selection by filtering ) using <i>rf</i> as the model. The best parameters and features are used for training the final model. (Reference: <a href='https://topepo.github.io/caret/train-models-by-tag.html#random-forest'>Random forest vignette</a>)</p></div>"))),
      
      fluidRow(
        column(width=4,
               selectInput(inputId = "rf_column",label = "Select variable for classification",choices = names(final_data()$metadata))),
        column(width=2,
               textInput(inputId = "rf_perc",label = "Train percentage",value = 0.8,width = '120px')),
        column(width = 3, selectInput(inputId = "rf_fsel",label = "Select feature selection method",choices = c("sbf")))
      ),
      fluidRow(
        column(width=4,
               actionButton(inputId = "submit_rf",label = "Submit"))
      )
      ,
      br(),
      conditionalPanel(condition = "input.submit_rf > 0",
                       tabsetPanel(
                         tabPanel(title = "Model metrics",
                                  fluidRow(
                                    column(width=12,verbatimTextOutput("rf_text") %>% withSpinner())
                                    
                                    
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width=5, plotOutput("rf_cm_plot")),
                                    column(width=5, plotOutput("rf_roc"))
                                    
                                  )
                                  
                         ),
                         tabPanel(title = "Features",
                                  fluidRow(
                                    
                                    column(width=4, 
                                           selectInput(inputId = "rf_vi",label = "Select number of proteins to display",choices = c(10,20,30,40)))
                                    
                                  ),
                                  fluidRow(
                                    column(width=8,plotOutput("rf_imp"))
                                  ))
                       ))
      
    )
    
  })
  
  ## Random Forest reactive #################
  
  
  run_rf <- eventReactive(input$submit_rf,{
    
    req(input$rf_column, input$rf_perc, input$rf_fsel)
    
    print("prepping data....")
    
    df_rf = prep_data(data = final_data()$npx,metadata = final_data()$metadata,group = input$rf_column)
    
    print("prepping data complete...")      
    
    print("splitting data....")
    dataset_list = split_data(data = df_rf,trainperc = input$rf_perc,group = input$rf_column)
    print("splitting data complete....")
    
    print("tuning rf model....")
    cvmdl = rf_model_tuning(traind = dataset_list[[3]],testd = dataset_list[[4]],group=input$rf_column)
    print("tuning model complete..")    
    
    print("selecting features...")
    cvsbf = rf_feature_sel(traind=dataset_list[[3]], testd=dataset_list[[4]],group=input$rf_column)
    print("selecting features complete")
    
    
    print("training rf with best parameters and best features...")
    final_model = train_rf(traind = dataset_list[[1]],testd=dataset_list[[2]],group=input$rf_column,mdl1=cvmdl,mdl2=cvsbf)
    print("training rf complete...")
    
    print("generating confusion matrix...")
    cfmplot = conf_matrix(model=final_model,group=input$rf_column,testd=dataset_list[[2]],mdl1 = cvsbf)
    
    print("plotting roc curve...")
    rocplt = plot_roc(model=final_model,testd = dataset_list[[2]], group=input$rf_column, mdl1 = cvsbf)
    rocplt1 = rocplt[[1]]
    
    ML_results$RF = list(final_model, rocplt[[2]])
    
    return(list(final_model, cfmplot, rocplt1))
    
    
  })
  
  
  
  
  output$rf_imp <- renderPlot({
    req(input$rf_vi)
    #n = as.numeric(input$svm_vi)
    d = run_rf()[[1]]
    plt = plot_imp(model=d,n= input$rf_vi)
    reportProd$rf_imp1 <- plt
    plt
    
  })
  
  
  
  
  output$rf_roc = renderPlot({
    reportProd$rf_roc1 <- run_rf()[[3]]
    run_rf()[[3]]
    
    
    
  })
  
  output$rf_text = renderPrint({
    reportProd$rf_text1 <- run_rf()[[1]]
    run_rf()[[1]]
    
  })
  
  
  output$rf_cm_plot <- renderPlot({
    
    cf = run_rf()[[2]]
    reportProd$rf_cm_plot1 <- cf
    cf
    
    
  })  
  
  #### Comparitive analysis (ROC) ################### 
  
  output$rocUI <- renderUI({
    
    fluidPage(
      column(width=12,
             box(collapsible = TRUE,width = 12,HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
                             <h2> Comparitive analysis between models </h2>
                             <p> This step compares all models run by the user using the following metrics: ROC, Accuracy, Sensitivity, Specificity. (Reference: <a href='https://rdrr.io/cran/pROC/man/plot.roc.html'> ROCplot vignette</a>) </p>
                             </div>"))),

      fluidRow(
        column(width=7,
          p("Please click this button to compare all models..."))),
      fluidRow(
        column(width=4,
               actionButton(inputId = "roc_submit",label = "Analyze")),
        ),
      conditionalPanel(condition = "input.roc_submit > 0",
                       tabsetPanel(
                         tabPanel(title = "Results",
                                  fluidRow(
                                    column(width=5,plotOutput("roc_plot_all"))
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width = 4,uiOutput("metric_dropdown"))),
                                  fluidRow(
                                    column(width=5,plotOutput("dot_plot"))
                                    
                                    )))))
                                    

  })
  
  
  ########## Comparitive analysis reactive ####################################################
  
  ## perform resampleing ( caret) for all models run 
  run_analysis <- eventReactive(input$roc_submit,{
    
      
      l = reactiveValuesToList(ML_results)
      res1 = comp_models(L=l)
      return(list(res1))
      
      
  })
  
  ## select the metric to view comparison
  output$metric_dropdown <- renderUI({

    res = run_analysis()[[1]]

    selectInput(inputId = "metric_choice",label = "Select a metric to compare models",choices = res[[2]])

  })
  
  ## render the dotplot based on choice of metric 
  output$dot_plot <- renderPlot({

    req(input$metric_choice)
    res = run_analysis()[[1]]
    p = dotplot(res[[1]], metric = input$metric_choice)
    reportProd$dot_plot1 = p
    p



  })
  
  ## plot roc curve for all models 
  output$roc_plot_all <- renderPlot({
    
    l = reactiveValuesToList(ML_results)
    p = plot_all_roc2(L=l)
    p

  })
  

  
  
  
  
   #### Diagnostic marker UI ####
  
  output$diagnosticmarkerUI = renderUI({
    fluidPage(
      shinyjs::useShinyjs(), # Enable shinyjs
      div(id = "ADV",
          fluidRow(uiOutput("descriptionUniavariate")), # description
          fluidRow(
            radioButtons(
              inputId = "DM1",
              label = "Select a test",
              choices = c("T-Test", "ANOVA", "Mann-Whitney U Test"),
              selected = "T-Test",
              inline = T # Display radio buttons inline
            ))),
      conditionalPanel(
        condition = "input.DM1 == 'T-Test'", # Condition for displaying T-Test options
        fluidRow(box(
          width = 6,
          selectInput(
            inputId = "CV1",
            label = "Comparision variable",
            choices = names(final_data()[[2]]),
            selectize = F, # Disable selectize
            selected = ""
          ),
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = "tt_cond1",
                label = "Case",
                choices = "",
                selectize = F
              )
            ),
            column(
              width = 6,
              selectInput(
                inputId = "tt_cond2",
                label = "Control",
                choices = "",
                selectize = F
              )
            )
          )),
          box(width = 6,
              tags$div(
                class = "info-icon",
                icon("info-circle", title = "Paired T-test compares means between two related groups, whereas unpaired T-test compares means between two independent groups.")
              ),
              fluidRow(
                column(
                  width = 6,
                  radioButtons(
                    inputId = "tt_type",
                    label = "Type of T-Test",
                    choices = c("Paired", "Unpaired"),
                    selected = "Unpaired",
                    inline = T
                  )
                ),
                column(
                  width = 6,
                  conditionalPanel(
                    condition = "input.tt_type == 'Paired'",
                    selectInput(
                      inputId = 'pairid1',
                      label = 'Pairid',
                      choices = names(final_data()[[2]]),
                      selectize = F
                    )
                  )
                )
              ),
              actionButton(inputId = "ttest_run", label = "Run")
              
          )),
        tabsetPanel(
          id = "testResults",  # Add an ID to the tabsetPanel
          type = "tabs",
          tabPanel(
            title = "T-Test Volcano plot",
            value = "T-TestVolcanoPlot",
            id= "T-Test-Volcano-plot",
            fluidRow(style = 'padding-left:30px; padding-right:5px; padding-top:5px; padding-bottom:10px',
                     dataTableOutput("ttest_table" )%>% withSpinner()),
            box(width = 12,
                tags$div(
                  class = "info-icon",
                  icon("info-circle", title = "T-Test Volcano Plot is a graphical representation used to visualize the results of t-tests. It highlights proteins or variables that exhibit significant differences in expression levels between experimental groups, aiding in the identification of potential biomarkers or targets for further analysis.")
                ),
                fluidRow(column(
                  width = 6,
                  radioButtons(
                    inputId = "volploty",
                    label = "Select the y axis(-log10 of 0.05 = 1.30103)",
                    choices = c("pvalue", "adjusted_pvalue"),
                    selected = "pvalue",
                    inline = T
                  )
                )),
                fluidRow(column(width=3,
                                textInput(
                                  inputId = "maxpval",
                                  label = "Lower limit for P value(-log10)",
                                  width = '100%',
                                  placeholder = "Values greater than entered values will be filtered"
                                )
                ),
                column(
                  width = 3,
                  textInput(
                    inputId = "maxlogfc",
                    label = "Left boundary for logfc",
                    width = '100%',
                    placeholder = "Values greater than entered values will be filtered"
                  )
                ),
                ),
                fluidRow(column(width=3,
                                textInput(
                                  inputId = "minpval",
                                  label = "Upper limit for p value(-log10)",
                                  width = '100%',
                                  placeholder = "Values less than entered values will be filtered"
                                )
                ),
                column(
                  width = 3,
                  textInput(
                    inputId = "minlogfc",
                    label = "Right boundary for logfc",
                    width = '100%',
                    placeholder = "Values greater than entered values will be filtered"
                  )
                ),
                column(
                  width = 5,
                  selectInput(
                    inputId = "ttbx2",
                    label = "Select variable2 xaxis for boxplot(optional)",
                    choices = c("",names(final_data()[[2]])),
                    selected = "",
                    selectize = F
                  )
                )
                ),
                fluidRow(
                  column(
                    width = 6,
                    fluidRow(plotlyOutput("vplt1") %>% withSpinner()) ,
                    fluidRow(
                      downloadButton("download4FOUR", "Download")
                    )
                  ),
                  column(width = 6,
                         fluidRow(plotOutput("bplt1") %>% withSpinner()),
                         fluidRow(column(
                           width = 5,
                           textInput(
                             inputId = "width3",
                             label = "Select the dimension of width",
                             value = 600
                           )
                         ),
                         column(
                           width = 5,
                           textInput(
                             inputId = "height3",
                             label = "Select the dimension of Height",
                             value = 800
                           )
                         ),downloadButton("download5", "Download")),
                         )), 
            )
          ),
          tabPanel(
            title = "T-Test Heatmap",
            value = "T-TestHeatmap",
            id= "T-Test-Heatmap",
            tags$div(
              class = "info-icon",
              icon("info-circle", title = "The T-Test Heatmap visually represents the results of t-tests, displaying variations in expression levels of proteins or other variables across different experimental conditions or groups.")
            ),
            fluidRow(
              column(width = 12,
                     style = 'padding-left:15px; padding-right:5px; padding-top:0px; padding-bottom:10px',
                     textInput(
                       inputId = "Hmap_pval",
                       label = "Select cutoff of pval",
                       value = "0.05",width = "150px"
                     ))),
            fluidRow(
              column(width = 12,
                     style = 'padding-left:15px; padding-right:5px; padding-top:0px; padding-bottom:10px',      
                     fluidRow(plotlyOutput("heatmap_ttest2") %>% withSpinner())
              )),
            fluidRow(
              downloadButton("download6", "Download")
            )
          )
        )
        ), 
      conditionalPanel(
        condition = "input.DM1 == 'ANOVA'", # Condition for displaying ANOVA options
        box(width = 12,
            fluidRow(
              column(
                width = 3,
                selectInput(
                  inputId = "an_CV1",
                  label = "Comparision variable1",
                  choices = names(final_data()[[2]]),
                  selectize = F
                )
              ),
              column(
                width = 3,
                selectInput(
                  inputId = "an_CV3",
                  label = "Comparision variable2",
                  choices = c("",names(final_data()[[2]])),
                  selectize = F
                )
              ),
              column(
                width = 3,
                selectInput(
                  inputId = "an_CV4",
                  label = "Covariate variable",
                  choices = c("",names(final_data()[[2]])),
                  selectize = F
                )
              ),
              column(
                width = 3,
                radioButtons(
                  inputId = "posthoc1",
                  label = "Perform Post-hoc",
                  choices = c("Yes", "No"),
                  selected = "Yes"
                )
              )
            ), 
            selectInput(
              inputId = "an_CV2",
              label = "Pair ID variable(This Column is used for removing singletons and replicates)",
              choices = names(final_data()[[2]]),
              selectize = F
            ),
            actionButton(inputId = "anova_run", label = "Run")
        ),
        uiOutput("posthoc_tabset"),
      ),
      conditionalPanel(
        condition = "input.DM1 == 'Mann-Whitney U Test'",
        fluidRow(box(
          width = 6,
          selectInput(
            inputId = "mwutCV1",
            label = "Comparision variable",
            choices = names(final_data()[[2]]),
            selectize = F
          ),
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = "mwut_cond1",
                label = "Case",
                choices = "",
                selectize = F
              )
            ),
            column(
              width = 6,
              selectInput(
                inputId = "mwut_cond2",
                label = "Control",
                choices = "",
                selectize = F
              )
            )
          )),
          box(width = 6,
              tags$div(
                class = "info-icon",
                icon("info-circle", title = "Paired assesses the difference between two related samples or matched pairs.Unpaired, assesses the difference between two independent groups without assuming normality.")
              ),
              fluidRow(
                column(
                  width = 6,
                  radioButtons(
                    inputId = "mwut_type",
                    label = "Type of Mann-Whitney U Test",
                    choices = c("Paired", "Unpaired"),
                    selected = "Unpaired",
                    inline = T
                  )
                ),
                column(
                  width = 6,
                  conditionalPanel(
                    condition = "input.mwut_type == 'Paired'",
                    selectInput(
                      inputId = 'mwutpairid1',
                      label = 'Pairid',
                      choices = names(final_data()[[2]]),
                      selectize = F
                    )
                  )
                )
              ),
              actionButton(inputId = "mwutest_run", label = "Run")
              
          )),
        tabsetPanel(
          id = "mwutestResults",  # Add an ID to the tabsetPanel
          type = "tabs",
          tabPanel(
            title = "Mann-Whitney U Test Results",
            value = "MannWhitneyUTestVolcanoPlot",
            id= "Mann-Whitney-U-Test-Volcano-plot",
            fluidRow(
              style = 'padding-left:30px; padding-right:5px; padding-top:5px; padding-bottom:10px',
              dataTableOutput("mwutest_table" )%>% withSpinner()),
            box(width = 12,
                tags$div(
                  class = "info-icon",
                  icon("info-circle", title = "The Mann-Whitney U Test, or Wilcoxon rank-sum test, compares two independent groups' distributions. It's non-parametric, meaning it doesn't assume specific distributions. It yields a U statistic and p-value. A low p-value (<0.05) suggests the groups differ significantly.")
                ),
                fluidRow(
                column(
                  width = 5,
                  selectInput(
                    inputId = "mwutbx2",
                    label = "Select variable2 xaxis for boxplot(optional)",
                    choices = c("",names(final_data()[[2]])),
                    selected = "",
                    selectize = F
                  )
                )
                ),
                fluidRow(
                  column(width = 6,
                         fluidRow(plotOutput("mwutbplt1") %>% withSpinner()),
                         fluidRow(column(
                           width = 5,
                           textInput(
                             inputId = "width3",
                             label = "Select the dimension of width",
                             value = 600
                           )
                         ),
                         column(
                           width = 5,
                           textInput(
                             inputId = "mwutheight3",
                             label = "Select the dimension of Height",
                             value = 800
                           )
                         )),
                         
                         fluidRow(
                           downloadButton("mwutdownload5", "Download")
                         ))), 
            )
          ),
          tabPanel(
            title = "Mann-Whitney U Test Heatmap",
            value = "Mann-Whitney-U-TestHeatmap",
            id= "Mann-Whitney-U-Test-Heatmap",
            tags$div(
              class = "info-icon",
              icon("info-circle", title = "The Mann-Whitney U Test Heatmap visualizes differential expression between two groups using color intensity. It's useful for identifying significant differences in protein expression across conditions.")
            ),
            fluidRow(
              column(width = 12,
                     style = 'padding-left:15px; padding-right:5px; padding-top:0px; padding-bottom:10px',
                     # h4("Heat Map"),
                     textInput(
                       inputId = "mwutHmap_pval",
                       label = "Select cutoff of pval",
                       value = "0.05",width = "150px"
                     ))),
            fluidRow(
              column(width = 12,
                     style = 'padding-left:15px; padding-right:5px; padding-top:0px; padding-bottom:10px',      
                     fluidRow(plotlyOutput("heatmap_mwutest2") %>% withSpinner())
              )),
            fluidRow(
              downloadButton("mwutdownload6", "Download")
            )
          )
        )
      )
    )
    
  })
  
  # Data Impot Description
  output$descriptionDataImport <- renderUI({
    HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #3498db;'>
       <p>Data Import is to import the data from different sources:</p>
       <ul>
         <li><strong>File Import</strong> - Import the data(.csv, .xlsx, .parquet ) from local pc.</li>
         <li><strong>File select/Test data</strong> - Select the data from snowflake database.</li>
         <li><strong>Column names</strong> - Please make sure that SampleID column name must be same in the Olink & Meta data file.</li>
       </ul>      
     </div>")
  })
  
  # Univariate Description
  output$descriptionUniavariate <- renderUI({
    HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #3498db;'>
       <p><strong>Univariate Analysis:</strong></p>
       <ul>
         <li><strong>T-Test</strong> - Statistical test used to compare the means of two independent groups to determine if there is a significant difference between them. (Reference: <a href='https://cran.r-project.org/web/packages/OlinkAnalyze/vignettes/Vignett.html#t-test-analysis-olink_ttest'>OlinkAnalyze Package Vignette</a>)</li>
         <li><strong>ANOVA (Analysis of Variance)</strong> - Statistical method used to compare means across multiple groups to determine if there are significant differences between them. (Reference: <a href='https://cran.r-project.org/web/packages/OlinkAnalyze/vignettes/Vignett.html#analysis-for-variance-anova-olink_anova'>ANOVA Vignette</a>, <a href='https://cran.r-project.org/web/packages/OlinkAnalyze/vignettes/Vignett.html#post-hoc-anova-analysis-olink_anova_posthoc'>ANOVA Post-Hoc Analysis Vignette</a>)</li>
         <li><strong>Mann-Whitney U Test</strong> - It is also known as the Wilcoxon rank-sum test, is a non-parametric statistical test used to compare the distributions of two independent groups. (Reference: <a href='https://cran.r-project.org/web/packages/OlinkAnalyze/vignettes/Vignett.html#mann-whitney-u-test-analysis-olink_wilcox'>Mann-Whitney U Test Vignette</a>)</li>
       </ul>      
     </div>")
  })
  
  output$posthoc_tabset <- renderUI({
    if (input$posthoc1 == "Yes") {
      tabsetPanel(
        id = "AnovaPostHocRes",  # Add an ID to the tabsetPanel
        type = "tabs",
        tabPanel(
          title = "Anova Results",
          value = "Anova_results",
          id= "anova-results",
          fluidRow(
            tags$div(
              class = "info-icon",
              icon("info-circle", title = "The ANOVA Results display statistical differences in protein expression across multiple groups or conditions, aiding in identifying significant variations among them.")
            ),
            style = 'padding-left:30px; padding-right:5px; padding-top:5px; padding-bottom:10px',
            fluidRow(dataTableOutput("anovatest_table")%>% withSpinner())
          ),
          fluidRow(
            column(
              width = 6,
              textInput(inputId = "Hmap2_pval",
                        label = "Select cutoff fo pval",
                        value = "0.05"
              ),
              selectInput(
                inputId = "heatmapcol",
                label = "Select a group to sort",
                choices = names(final_data()[[2]]),
                selected = "",
                multiple = F,selectize = F
              ),
              fluidRow(plotlyOutput("heatmap_anova") %>% withSpinner()),
              fluidRow(
                downloadButton("download7", "Download")
                # verbatimTextOutput("messageDAT1"),
                # verbatimTextOutput("messageDAT2")
              )),
            column(
              width = 6,
              selectInput(
                inputId = "anbx1",
                label = "Select variable1 for xaxis (Mandatory)",
                choices = c("",names(final_data()[[2]])),
                selected = input$an_CV1,
                selectize = F
              ),
              selectInput(
                inputId = "anbx2",
                label = "Select variable2 for xaxis (optional)",
                choices = c("",names(final_data()[[2]])),
                selected = "",
                selectize = F
              ),
              fluidRow(plotOutput("anplt1") %>% withSpinner()),
              fluidRow(column(
                width = 5,
                textInput(
                  inputId = "width4",
                  label = "Select the dimension of width",
                  value = 600
                )
              ),
              column(
                width = 5,
                textInput(
                  inputId = "height4",
                  label = "Select the dimension of Height",
                  value = 800
                )
              )),
              fluidRow(
                downloadButton("download8", "Download")
              )
            ))
        ),
        tabPanel(
          title = "Post-hoc Results",
          value = "Post_hoc_results",
          id= "Post-hoc-results",
          fluidRow(
            tags$div(
              class = "info-icon",
              icon("info-circle", title = "The Post-hoc Results provide further analysis following ANOVA, helping identify specific group differences when significant variation is observed among multiple conditions or groups.")
            ),
            style = 'padding-left:30px; padding-right:5px; padding-top:5px; padding-bottom:10px',
            fluidRow(dataTableOutput("posthoctest_table")%>% withSpinner())
          )
        )
      )
    } else {
      # NULL  # Return NULL to hide the tabsetPanel
      tabsetPanel(
        id = "AnovaPostHocRes",  # Add an ID to the tabsetPanel
        type = "tabs",
        tabPanel(
          title = "Anova Results",
          value = "Anova_results",
          id= "anova-results",
          fluidRow(
            # h3("Anova results"),
            tags$div(
              class = "info-icon",
              icon("info-circle", title = "The ANOVA Results display statistical differences in protein expression across multiple groups or conditions, aiding in identifying significant variations among them.")
            ),
            style = 'padding-left:30px; padding-right:5px; padding-top:5px; padding-bottom:10px',
            fluidRow(dataTableOutput("anovatest_table")%>% withSpinner())
          ),
          fluidRow(
            column(
              width = 6,
              textInput(inputId = "Hmap2_pval",
                        label = "Select cutoff fo pval",
                        value = "0.05"
              ),
              selectInput(
                inputId = "heatmapcol",
                label = "Select a group to sort",
                choices = names(final_data()[[2]]),
                selected = "",
                multiple = F,selectize = F
              ),
              fluidRow(plotlyOutput("heatmap_anova") %>% withSpinner()),
              fluidRow(
                downloadButton("download7", "Download")
                # verbatimTextOutput("messageDAT1"),
                # verbatimTextOutput("messageDAT2")
              )),
            column(
              width = 6,
              selectInput(
                inputId = "anbx1",
                label = "Select variable1 for xaxis (Mandatory)",
                choices = c("",names(final_data()[[2]])),
                selected = input$an_CV1,
                selectize = F
              ),
              selectInput(
                inputId = "anbx2",
                label = "Select variable2 for xaxis (optional)",
                choices = c("",names(final_data()[[2]])),
                selected = "",
                selectize = F
              ),
              # plotOutput("anplt1"),
              fluidRow(plotOutput("anplt1") %>% withSpinner()),
              fluidRow(column(
                width = 5,
                textInput(
                  inputId = "width4",
                  label = "Select the dimension of width",
                  value = 600
                )
              ),
              column(
                width = 5,
                textInput(
                  inputId = "height4",
                  label = "Select the dimension of Height",
                  value = 800
                )
              )),
              fluidRow(
                downloadButton("download8", "Download")
                # verbatimTextOutput("messageDAT3"),
                # verbatimTextOutput("messageDAT4")
              )
            ))
        )
      )
    }
  })
 
  
  
  #### Geneset Pathway enrichment analysis UI ####
  output$genesetenrichmentanalysisUI = renderUI({
    fluidPage(
      column(width=12,
             box(collapsible = TRUE,width = 12,HTML("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; border: 1px solid #34dbd3;'>
                             <h2> Gene Set Enrichment Analysis </h2>
         <p>Gene set enrichment can be performed on the results obtained from ttest, ANOVA, Man whitney test, Machine learning models to identify the pathways/genesets enriched in the samples. Current implementation supports all MSigDB genesets ( Molecular signature database).Alternately, custom genesets can be uploaded.(Reference: <a href='https://www.rdocumentation.org/packages/clusterProfiler/versions/3.0.4/topics/GSEA'> GSEA vignette</a>)</p></div>"))),
      
      fluidRow(
        box(
          width = 8,
          fluidRow(
            style = 'padding-left:20px; padding-right:0px; padding-top:5px; padding-bottom:10px',
            radioButtons(
              inputId = "GSEA_option",
              label = "Select an option to perform GSEA",
              choices = c("ttest", "ANOVA", "Man Whitney U-Test","Timecourse", "Machine Learning"),
              selected = "ttest",
              inline = T
            )
          ),
          conditionalPanel(condition = "input.GSEA_option == 'Timecourse'",
                           fluidRow(column=5,
                                    selectInput(inputId = "tca_contrast1",
                                                label = "Select a contrast",
                                                choices = c(""),
                                                selectize = F))),
          conditionalPanel(condition ="input.GSEA_option == 'Machine Learning'",
                          fluidRow(column(width=5,
                                   selectInput(inputId = "ml_model",
                                                label = "Select a model",
                                               choices = names(Filter(Negate(is.null), reactiveValuesToList(ML_results))))),
                                   column(width=5,
                                          selectInput(inputId = "gsea_group",
                                                      label = "Select the group",
                                                      choices = c(""))))),
          conditionalPanel(
            condition = "input.GSEA_option == 'ANOVA'",
            fluidRow(
              selectInput(
                inputId = "contrast1",
                label = "Select a contrast",
                choices = c(""),
                multiple = F,
                selectize = F
              )
            )
          ),
          fluidRow(
            style = 'padding-left:20px; padding-right:0px; padding-top:5px; padding-bottom:10px',
            radioButtons(
              inputId = "GSEA_option2",
              label = "Select an option to perform GSEA",
              choices = c("msigdb", "Customized Gene Set"),
              selected = "msigdb",
              inline = T
            )
          ),
          conditionalPanel(
            condition = "input.GSEA_option2 == 'Customized Gene Set'",
            fluidRow(
              
              column(width = 8,
                     fileInput(
                       inputId = "ipfile1",
                       width = "100%",
                       multiple = TRUE,
                       label = strong('Please select a geneset file')
                       
                     )),
              column(width = 4, style = 'padding-left:5px; padding-right:0px; padding-top:25px; padding-bottom:10px',
                     actionButton(
                       style = 'border-radius: 50%;',
                       inputId = "help2",
                       label = "",
                       icon = icon("exclamation-circle"))
              )
            )), 
          conditionalPanel(
            condition = "input.GSEA_option2 == 'msigdb'",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "gsspecies",
                  label = "Please select species",
                  choices = msigdbr::msigdbr_species()[, 1],
                  selected = "Homo sapiens",
                  selectize = F
                )
              ),
              column(
                style = 'padding-left:5px; padding-right:0px; padding-top:5px; padding-bottom:10px',
                width = 3,
                checkboxGroupInput(
                  inputId = "gscategory",
                  label = "Please Select a Category",
                  choiceNames = c(
                    "H-hallmark gene sets",
                    "C1-positional gene sets",
                    "C2-curated gene sets",
                    "C3-motif gene sets",
                    "C4-computational gene sets",
                    "C5-GO gene sets",
                    "C6-oncogenic signatures",
                    "C7-immunologic signatures",
                    "C8-cell type signature"
                  ),
                  choiceValues = c("H", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8")
                )
              ),
              column(
                #   tags$style(HTML("
                # .btn {
                #   padding-top:3px;
                #   padding-left:5px;
                #   display:block;
                #   height: 17px;
                #   width: 17px;
                #   border-radius: 50%;
                #   }
                #
                #   ")),
                style = 'padding-left:0px; padding-right:0px; padding-top:4px; padding-bottom:10px',
                width = 4,
                actionButton(
                  style = 'font-size:50% ;padding-top:3px;padding-left:5px; display:block;height: 17px; width: 17px; border-radius: 50%;',
                  inputId = "help1",
                  label = "",
                  icon = icon("exclamation-circle")
                )
              )
            )),
          
          fluidRow(
            style = 'padding-left:15px; padding-right:0px; padding-top:4px; padding-bottom:10px',
            textInput(
              inputId = "p_adjust_cutoff",
              label = "p adjust cutoff",
              value = 0.05
            )
          ),
          actionButton(inputId = "GSEA_run", label = "Run GSEA")
        )
      ),
      fluidRow(h3("GSEA results"), style = 'padding-left:30px; padding-right:5px; padding-top:5px; padding-bottom:10px',
               dataTableOutput("GSEA_table")),
      tabsetPanel(
        tabPanel(title = "Gene ratio",
                 fluidRow(
                   h3("Dot Plot"),
                   plotOutput("dotplot_GSEA"),
                   fluidRow(column(
                     width = 4,
                     textInput(
                       inputId = "width5",
                       label = "Select the dimension of width",
                       value = 600
                     )
                   ),
                   column(
                     width = 4,
                     textInput(
                       inputId = "height5",
                       label = "Select the dimension of Height",
                       value = 800
                     )
                   )),
                   downloadButton("download12", "Download")
                   # verbatimTextOutput("messageGS1"),
                   # verbatimTextOutput("messageGS2")
                 )
                 ),
        tabPanel(title = "Enrichment Score",
                 fluidRow(h3("Ridge Plot"),
                          plotOutput("ridgeplot_GSEA"),
                          fluidRow(column(
                            width = 4,
                            textInput(
                              inputId = "width6",
                              label = "Select the dimension of width",
                              value = 600
                            )
                          ),
                          column(
                            width = 4,
                            textInput(
                              inputId = "height6",
                              label = "Select the dimension of Height",
                              value = 800
                            )
                          )),
                          downloadButton("download13", "Download")
                          # verbatimTextOutput("messageGS3"),
                          # verbatimTextOutput("messageGS4")
                 )
                 )
      )
      
      
    )
  })

  #### Back End code ####
  
  #### non reactive functions ####

  
  t_test <- function(olink_data, olink_data_metadata, type, variable, pair_id){
    
    # Join metadata to NPX data
    # Make SampleID column name same for both dataframes.
    colnames(olink_data_metadata )[1] <- "SampleID"
    # Perform Inner Join
    joined_olink_data <<- inner_join(olink_data,olink_data_metadata, by="SampleID")
    joined_olink_data <<- data.frame(joined_olink_data)
    joined_olink_data[[variable]] <- as.factor(joined_olink_data[[variable]])
    # Use olink_ttest function from olinkAnalyze package to perform t test. Variable must have only 2 possible values/conditions
    tryCatch(
      {
        # Use olink_ttest function from olinkAnalyze package to perform t test. Variable must have only 2 possible values/conditions
        if( type == "Paired"){
          ttest_result_paired <- olink_ttest(df = joined_olink_data, variable = variable, pair_id = pair_id)
          # Select only required columns from t test result
          required_columns <- c("Assay", "OlinkID", "UniProt", "Panel", "estimate", "p.value", "method","alternative", "Adjusted_pval")
          ttest_result_paired <- ttest_result_paired[, required_columns]
          ttest_result_paired <- rename(ttest_result_paired, LogFC = estimate)
          return(ttest_result_paired)
        }
        
        if(type == "Unpaired"){
          ttest_result_unpaired <- olink_ttest(df = joined_olink_data, variable = variable)
          required_columns <- c("Assay", "OlinkID", "UniProt", "Panel", "estimate", "p.value", "method","alternative", "Adjusted_pval")
          ttest_result_unpaired <- ttest_result_unpaired[, required_columns]
          ttest_result_unpaired <- rename(ttest_result_unpaired, LogFC = estimate)
          return(ttest_result_unpaired)
        }
      },
      error = function(e) {
        # Create and display a shinyalert dialog if an error occurs
        shinyalert(
          title = "Error",
          text = "Please select suitable combination of columns and try again."
        )
        return(NULL)
      }
    )
    
  }
  
  mwu_test <- function(olink_data, olink_data_metadata, type, variable, pair_id){
    # Join metadata to NPX data
    # Make SampleID column name same for both dataframes.
    colnames(olink_data_metadata )[1] <- "SampleID"
    # Perform Inner Join
    joined_olink_data <<- inner_join(olink_data,olink_data_metadata, by="SampleID")
    joined_olink_data <<- data.frame(joined_olink_data)
    joined_olink_data[[variable]] <- as.factor(joined_olink_data[[variable]])
    # Use olink_wilcox function from olinkAnalyze package to perform t test. Variable must have only 2 possible values/conditions
    tryCatch(
      {
        if( type == "Paired"){
          mwutest_result_paired <- olink_wilcox(df = joined_olink_data, variable = variable,pair_id = pair_id)
          #Select only required columns from t test result
          required_columns <- c("Assay", "OlinkID", "UniProt", "Panel", "estimate", "p.value", "method","alternative", "Adjusted_pval")
          mwutest_result_paired <- mwutest_result_paired[,required_columns]
          mwutest_result_paired <- rename(mwutest_result_paired, LogFC = estimate)
          return(mwutest_result_paired)
        }
        
        if(type == "Unpaired"){
          mwutest_result_unpaired <- olink_wilcox(df = joined_olink_data, variable = variable)
          required_columns <- c("Assay", "OlinkID", "UniProt", "Panel", "estimate", "p.value", "method","alternative", "Adjusted_pval")
          mwutest_result_unpaired = mwutest_result_unpaired[,required_columns]
          mwutest_result_unpaired <- rename(mwutest_result_unpaired, LogFC = estimate)
          return(mwutest_result_unpaired)
        }
      },
      error = function(e) {
        # Create and display a shinyalert dialog if an error occurs
        shinyalert(
          title = "Error",
          text = "Please select suitable combination of columns and try again."
        )
        return(NULL)
      }
    )      
  }
  
  anova_test <- function(olink_data_test, olink_data_metadata, variable1, variable2, covariate) {
    tryCatch({
      # Join metadata to NPX data
      # Make SampleID column name same for both dataframes.
      colnames(olink_data_metadata)[1] <- "SampleID"
      # Perform Inner Join
      joined_olink_data <- inner_join(olink_data_test, olink_data_metadata, by = "SampleID")
      joined_olink_data <- data.frame(joined_olink_data)
      
      if (variable2 == "" && covariate == "") {
        # ANOVA analysis with comparison variable
        joined_olink_data[[variable1]] <- as.factor(joined_olink_data[[variable1]])
        anova_result <- olink_anova(df = joined_olink_data, variable = variable1, outcome = "NPX")
        if (input$posthoc1 == "Yes") {
          anova_post_hoc <- olink_anova_posthoc(df = joined_olink_data,
                                                effect = variable1,
                                                variable = variable1)
        } else {
          anova_post_hoc <- "Not Selected"
          print("Post hoc is not selected")
        }
      } else if (variable2 == "" && covariate != "") {
        # ANOVA analysis with comparison variable and covariate
        joined_olink_data[[variable1]] <- as.factor(joined_olink_data[[variable1]])
        joined_olink_data[[covariate]] <- as.factor(joined_olink_data[[covariate]])
        anova_result <- olink_anova(df = joined_olink_data, variable = variable1, outcome = "NPX", covariates = covariate)
        if (input$posthoc1 == "Yes") {
          anova_post_hoc <- olink_anova_posthoc(df = joined_olink_data,
                                                effect = variable1,
                                                variable = variable1,
                                                covariates = covariate)
        } else {
          anova_post_hoc <- "Not Selected"
          print("Post hoc is not selected")
        }
      } else if (variable2 != "" && covariate != "") {
        # ANOVA analysis with interaction effect and covariate
        joined_olink_data[[variable1]] <- as.factor(joined_olink_data[[variable1]])
        joined_olink_data[[variable2]] <- as.factor(joined_olink_data[[variable2]])
        joined_olink_data[[covariate]] <- as.factor(joined_olink_data[[covariate]])
        anova_result <- olink_anova(df = joined_olink_data, variable = paste0(variable1, ":", variable2), outcome = "NPX", covariates = covariate)
        if (input$posthoc1 == "Yes") {
          anova_post_hoc <- olink_anova_posthoc(df = joined_olink_data,
                                                effect = paste0(variable1, ":", variable2),
                                                variable = paste0(variable1, ":", variable2),
                                                covariates = covariate)
        } else {
          anova_post_hoc <- "Not Selected"
          print("Post hoc is not selected")
        }
      } else {
        # ANOVA analysis with interaction effect
        joined_olink_data[[variable1]] <- as.factor(joined_olink_data[[variable1]])
        joined_olink_data[[variable2]] <- as.factor(joined_olink_data[[variable2]])
        anova_result <- olink_anova(df = joined_olink_data, variable = paste0(variable1, ":", variable2), outcome = "NPX")
        if (input$posthoc1 == "Yes") {
          anova_post_hoc <- olink_anova_posthoc(df = joined_olink_data,
                                                effect = paste0(variable1, ":", variable2),
                                                variable = paste0(variable1, ":", variable2))
        } else {
          anova_post_hoc <- "Not Selected"
          print("Post hoc is not selected")
        }
      }
      return(list(anova_result, anova_post_hoc))
    }, error = function(e) {
      # Create and display a shinyalert dialog if an error occurs
      shinyalert(
        title = "Error",
        text = "Please select suitable combination of columns and try again."
      )
      return(NULL)
    })
  }

   #### End of Non reactive Functions #### 

  ### Reactive Functions ###
  
  ## Uncomment and this and remove below df1 code for DB integration
  # df1 <- reactive({
  #   if(input$datatype1 == "File select/Test data"){
  #     D<-input$Dfile1
  #     print("D")
  #     print(D)
  #     # D1<<-paste0(getwd(),"/data/",D)
  #     D1<<-D
  #     print("D1")
  #     print(D1)
  #     D2<<-input$Dfile1
  #     print("D2")
  #     print(D2)
  #   }else{
  #     D1 <<- input$file1$datapath
  #     D2 <<- input$file1$name
  #   }
  #   return(list(D1, D2))
  # })

  df1 <- reactive({
    if(input$datatype1 == "File select/Test data"){
      D<-input$Dfile1
      D1<<-paste0(getwd(),"/data/proteinData/",D)
      D2<<-input$Dfile1
    }else{
      D1 <<- input$file1$datapath
      D2 <<- input$file1$name
    }
    return(list(D1, D2))
  })    
  
 # df2 <- reactive({
 #    if (input$datatype1 == "File select/Test data") {
 #      D <- input$Dfile2
 #      # D3 <<- paste0(getwd(), "/data/", D)
 #      # D4 <<- paste0(getwd(), "/data/", D)
 #      D3 <<- D
 #      D4 <<- D
 #    } else{
 #      D3 <<- input$file2$datapath
 #      D4 <<- input$file2$name
 #      
 #    }
 #    return(list(D3, D4))
 #  })
 
 df2 <- reactive({
   if (input$datatype1 == "File select/Test data") {
     D <- input$Dfile2
     D3 <<- paste0(getwd(), "/data/proteinData/", D)
     D4 <<- paste0(getwd(), "/data/proteinData/", D)
   } else{
     D3 <<- input$file2$datapath
     D4 <<- input$file2$name
     
   }
   return(list(D3, D4))
 })
 
 ## Uncomment and this and remove below df1 code for DB integration  
 # dff3 <- eventReactive(input$loadBtns, {
 #    show_modal_spinner(spin = "atom",
 #                       color = "firebrick",
 #                       text = "Please wait uploading files...")
 #    
 #    # olink_data <<- read_NPX(df1()[[1]])
 #    olink_data <<- get_snowflake_npx_data(df1()[[1]])
 #    # if (endsWith(x = df2()[[1]], suffix = ".csv")) {
 #    #   Metadata <- read.csv(df2()[[2]],fileEncoding="latin1")
 #    #   Metadata <- data.frame(Metadata)
 #    # } else{
 #    #   Metadata <- readxl::read_xlsx(df2()[[1]])
 #    #   Metadata <- data.frame(Metadata)
 #    # }
 #    
 #    # Metadata <<- data.frame(Metadata)
 #    Metadata <<- get_snowflake_metadata_data(df2()[[1]])
 # #Call the function to reset plots and clear variables
 #    resetPlotsAndVariables()
 #    remove_modal_spinner()
 #    return(list(olink_data, Metadata))
 #    
 #  })
 
 dff3 <- eventReactive(input$loadBtns, {
   rm(list = ls(all.names = TRUE))
   show_modal_spinner(spin = "atom",
                      color = "firebrick",
                      text = "Please wait uploading files...")
   
   olink_data <<- read_NPX(df1()[[1]])
   if (endsWith(x = df2()[[1]], suffix = ".csv")) {
     Metadata <- read.csv(df2()[[2]],fileEncoding="latin1")
     Metadata <- data.frame(Metadata)
   } else{
     Metadata <- readxl::read_xlsx(df2()[[1]])
     Metadata <- data.frame(Metadata)
   }
   
   Metadata <<- data.frame(Metadata)
   # Call the function to reset plots and clear variables
   resetPlotsAndVariables()
   remove_modal_spinner()
   return(list(olink_data, Metadata))
   
 })
 
   
  dff4 <- eventReactive(input$loadBtn, {
    rm(list = ls(all.names = TRUE))
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait uploading files...")
    
    # Check if the "Olink HT" checkbox is checked
    is_olink_ht <- input$olinkHTCheckbox
    
    if (endsWith(df1()[[1]], ".npx") || endsWith(df1()[[1]], ".csv")) {
      olink_data <- read_NPX(df1()[[1]])
    } else if (endsWith(df1()[[1]], ".parquet")) {
      if (is_olink_ht) {
        olink_data <- read_parquet(df1()[[1]])
        # Rename columns if it's Olink HT
        colnames(olink_data)[17] <- "Assay_Warning"
        colnames(olink_data)[18] <- "QC_Warning"
      } else {
        olink_data <- read_parquet(df1()[[1]])
      }
    } else {
      remove_modal_spinner()
      showNotification("Unsupported file format for data. Please select an NPX (.npx or .csv) or Parquet (.parquet) file.", type = "error")
      return(NULL)
    }
    
    if (endsWith(df2()[[1]], ".csv")) {
      Metadata <- read.csv(df2()[[1]], fileEncoding = "latin1")
    } else if (endsWith(df2()[[1]], ".xlsx")) {
      Metadata <- readxl::read_xlsx(df2()[[1]])
    } else if (endsWith(df2()[[1]], ".parquet")) {
      Metadata <- read_parquet(df2()[[1]])
    } else {
      remove_modal_spinner()
      showNotification("Unsupported file format for metadata. Please select a CSV, Excel, or Parquet file.", type = "error")
      return(NULL)
    }
    # Extract file names
    # olink_table_name <- tools::file_path_sans_ext(basename(df1()$name))
    table_name <- df1()[[2]]
    table_name <- tools::file_path_sans_ext(table_name)
    olink_table_name <- tools::file_path_sans_ext(df1()[[2]])
    metadata_table_name <- tools::file_path_sans_ext(df2()[[2]])
    ##### Upload data to snowflake
    ##### Commented because this should be done in the last
    # con <- DBI::dbConnect(
    #   odbc::odbc(),
    #   Driver = "SnowflakeDSIIDriver",
    #   Server = "el57643.ap-south-1.aws.snowflakecomputing.com",
    #   UID = "AMANJOLLY",
    #   PWD = "Hitman@047",
    #   Warehouse = "COMPUTE_WH",
    #   Database = "SANOFI_OLINK",
    #   Schema = "RSHINY"
    # )
    
    # Upload olink_data to Snowflake
    # dbWriteTable(con, olink_table_name, olink_data, overwrite = TRUE, row.names = FALSE)
    
    # Upload Metadata to Snowflake
    # dbWriteTable(con, metadata_table_name, Metadata, overwrite = TRUE, row.names = FALSE)
    
    # Close connection
    # dbDisconnect(con)
    # Call the function to reset plots and clear variables
    resetPlotsAndVariables()
    remove_modal_spinner()
    list(olink_data, Metadata)
  })
  
 df3 <- reactive({
    if(input$datatype1 == "File select/Test data"){
      olink_data <<- dff3()[[1]]
      Metadata <<- dff3()[[2]]
    }else{
      olink_data <<- dff4()[[1]]
      print(unique(olink_data$SampleID))
      Metadata <<- dff4()[[2]]
      print(unique(Metadata$SampleID))
      no_matchs_olink <- setdiff(olink_data$SampleID, Metadata$SampleID)
      no_matchs_meta <- setdiff(Metadata$SampleID, olink_data$SampleID)
      notMatchedSampleIds$no_matchd_olink_SampleIDs <- no_matchs_olink
      notMatchedSampleIds$no_matchd_meta_SampleIDs <- no_matchs_meta
    }
    return(list(olink_data, Metadata))
  })
  
  df_msg2<- reactive({
    msg <- c('<p style="color:#FF0000">Warning! Entire data is considered for plotting. No QC filters are applied<br></p>')
    reportProd$df_msg21 <- "Warning! Entire data is considered for plotting. No QC filters are applied"
    msg
  })
   
 df_msg3<- reactive({
    
    olink_data <- olink_data
    olink_data_metadata <- Metadata
    colnames(olink_data_metadata)[1] <- "SampleID"
    joined_olink_data <- inner_join(olink_data,olink_data_metadata, by="SampleID")
    d <- data.frame(joined_olink_data$Panel, joined_olink_data$Assay)
    #Find common genes from multiple panels
    test <-
      dcast(
        d,
        data.table::rowid(joined_olink_data.Panel) ~ joined_olink_data.Panel,
        value.var = "joined_olink_data.Assay"
      )
    vals <- lapply(test[, -1], unique)
    # new_vals <- data.frame(vals)
    common_genes<-Reduce(intersect,vals)
    
    msg <- paste0(c("Following Genes", common_genes ," are excluded from the plot"),collapse = " ")
    msg1 <- paste0('<p style="color:#FF0000">',msg,'<br></p>')
    msg
  })
 
 ## remove this code and un comment below one for DB integration
 df_msg_FS <- eventReactive(input$loadBtns,{
   req(df1()[[1]])
   req(df2()[[1]])
   olink_data <- read_NPX(df1()[[1]])
   if (endsWith(x = df2()[[1]], suffix = ".csv")) {
     Metadata <- read.csv(df2()[[1]],fileEncoding="latin1")
   } else{
     Metadata <- readxl::read_xlsx(df2()[[1]])
   }
   unique_samples_test <- unique(olink_data$SampleID)
   unique_samples_metadata <- unique(Metadata$OlinkExplorer_SampleID)
   if (length(unique_samples_test) == length(unique_samples_metadata)) {
     msg <- c('<p style="color:#FF0000">Unique samples in NPX data and metadata match!<br>
        Please go to subsetting tab in the diagnostic markers if you want select a subset of data</p>')
     reportProd$df_msg_FS1 <- "Unique samples in NPX data and metadata match!
        Please go to subsetting tab in the diagnostic markers if you want select a subset of data."
   } else{
     msg <- c('<p style="color:#FF0000">Unique samples in NPX data and metadata do not match!
        Only intersecting samples will be used in downstream analysis except QC plots<br>
        Please go to subsetting tab in the diagnostic markers if you want select a subset of data</p>'
     )
     reportProd$df_msg_FS1 <- "Unique samples in NPX data and metadata do not match!
        Only intersecting samples will be used in downstream analysis except QC plots
        Please go to subsetting tab in the diagnostic markers if you want select a subset of data. Unnmatched samples are listed above in the Data Summary section"
   }
   
   msg
   
   return(msg)
   
 })
   
  #  df_msg_FS <- eventReactive(input$loadBtns,{
  #   req(df1()[[1]])
  #   req(df2()[[1]])
  #   # olink_data <- read_NPX(df1()[[1]])
  #   olink_data <<- get_snowflake_npx_data(df1()[[1]])
  #   # if (endsWith(x = df2()[[1]], suffix = ".csv")) {
  #   #   Metadata <- read.csv(df2()[[1]],fileEncoding="latin1")
  #   # } else{
  #   #   Metadata <- readxl::read_xlsx(df2()[[1]])
  #   # }
  #   Metadata <<- get_snowflake_metadata_data(df2()[[1]])
  #   unique_samples_test <- unique(olink_data$SampleID)
  #   unique_samples_metadata <- unique(Metadata$OlinkExplorer_SampleID)
  #   if (length(unique_samples_test) == length(unique_samples_metadata)) {
  #     msg <- c('<p style="color:#FF0000">Warning! Unique samples in NPX data and metadata match!<br>
  #       Please go to subsetting tab in the diagnostic markers if you want select a subset of data</p>')
  #   } else{
  #     msg <- c('<p style="color:#FF0000">Warning! Unique samples in NPX data and metadata do not match!
  #       Only intersecting samples will be used in downstream analysis except QC plots<br>
  #       Please go to subsetting tab in the diagnostic markers if you want select a subset of data</p>'
  #     )
  #   }
  #   
  #   msg
  #   
  #   return(msg)
  #   
  # })
 
  df_msg_FI <- eventReactive(input$loadBtn,{
    req(df1()[[1]])
    req(df2()[[1]])
    olink_data <- read_NPX(df1()[[1]])
    if (endsWith(x = df2()[[1]], suffix = ".csv")) {
      Metadata <- read.csv(df2()[[1]],fileEncoding="latin1")
    } else{
      Metadata <- readxl::read_xlsx(df2()[[1]])
    }
    unique_samples_test <- unique(olink_data$SampleID)
    unique_samples_metadata <- unique(Metadata$OlinkExplorer_SampleID)
    if (length(unique_samples_test) == length(unique_samples_metadata)) {
      msg <- c('<p style="color:#FF0000">Warning! Unique samples in NPX data and metadata match!<br>
        Please go to subsetting tab in the diagnostic markers if you want select a subset of data</p>')
      reportProd$df_msg_FI1 <- "Warning! Unique samples in NPX data and metadata match!
        Please go to subsetting tab in the diagnostic markers if you want select a subset of data"
    } else{
      msg <- c('<p style="color:#FF0000">Warning! Unique samples in NPX data and metadata do not match!
        Only intersecting samples will be used in downstream analysis except QC plots<br>
        Please go to subsetting tab in the diagnostic markers if you want select a subset of data</p>'
      )
      reportProd$df_msg_FI1 <- "Warning! Unique samples in NPX data and metadata do not match!
        Only intersecting samples will be used in downstream analysis except QC plots
        Please go to subsetting tab in the diagnostic markers if you want select a subset of data. Please find the unmatched sampleIDs are listed in above data summary section."
    }
    
    msg
    
    return(msg)
    
  })
    
   df_msg <- reactive({
    if(input$datatype1 == "File import"){
      msg<- df_msg_FI()
      msg
    } else if (input$datatype1 == "File select/Test data"){
      msg<- df_msg_FS()
      msg
    }
    return(msg)
  })
  observeEvent(req(input$loadBtn | input$loadBtns), {
    msg <- df_msg()
    print(msg)
    if (!is.null(msg) && length(msg) > 0 && nchar(msg) > 0) {
      showModal(
        modalDialog(
          title = "Message",
          HTML(msg),
          footer = tagList(
            actionButton(inputId = "closeModalBtn", label = "Close", icon = icon("times"))
          ),
          easyClose = FALSE,
          size = "l"
        )
      )
    }
  })
  
  observeEvent(input$closeModalBtn, {
    removeModal()
  })
  
  df4<-reactive({
    # print(head(df_normalize()[[1]]))
    DF<-data.frame(
      "uniquesample"=length(unique(df3()[[1]]$SampleID)),
      "uniquepanels"=length(unique(df3()[[1]]$Panel)),
      "uniqueAssay"=length(unique(df3()[[1]]$Assay)),
      "QC_warningspass"=sum(df3()[[1]]$QC_Warning == "PASS"),
      "QC_warningsfail"= sum(df3()[[1]]$QC_Warning != "PASS")
    )
    return(DF)
  })
  
  df_normalize <- eventReactive(input$run_norm1,{
    if( input$QCopts == "Normalization"){
      olink_data <-  normalize_meadianwise(df3()[[1]])
      olink_data$NPX <- olink_data$normalized_NPX
      olink_data$normalized_NPX <- NULL
      olink_data <<- olink_data
    }else {
      olink_data <<- df3()[[1]]
    }
    return(olink_data)
  })
  
  df51 <- reactive({
    if (input$QC1 == TRUE) {
      olink_data1 <- olink_data[(olink_data$QC_Warning != "WARN"), ]
      Metadata1 <- Metadata
    } else{
      olink_data1 <- olink_data
      Metadata1 <- Metadata
    }
    return(list(olink_data1,Metadata1))
  })
  
  df5 <- reactive({
    olink_data <<- df51()[[1]]
    Metadata <<- df51()[[2]]
    
    tryCatch(
      expr = {
        if (input$subset_data > 0) {
          Metadata <<- Metadata[input$subset2_rows_selected, ]
          olink_data <<- olink_data
          
          if (nrow(Metadata) == 0) {
            Metadata <<- df51()[[2]]
          }
        } else {
          olink_data <<- df51()[[1]]
          Metadata <<- df51()[[2]]
        }
      },
      error = function(e) {
        olink_data <<- df51()[[1]]
        Metadata <<- df51()[[2]]
      }
    )
    
    return(list(olink_data, Metadata))
  })
  
  panel_list <- reactive({
    lst<-unique(df3()[[1]]$Panel)
    lst})
  
  coropts1 <- reactive({
    opts1<- unique( df_joined()[,grep(paste0("^","Assay","$"),names( df_joined()))])
    opts2<- unique( df_joined()[,grep(paste0("^","Assay","$"),names( df_joined()))])
    return(list(opts1,opts2))
  })
  
  df_joined<-reactive({
    olink_data <-olink_data
    olink_data_metadata <-Metadata
    colnames(olink_data_metadata)[1] <- "SampleID"
    joined_olink_data <<- inner_join(olink_data, olink_data_metadata, by = "SampleID")
    joined_olink_data
  })
  
  output$df = renderDataTable({
    datatable(
      df3()[[1]],
      class = "display",
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        dom = 'frtBip',
        buttons = c('csv', 'excel')
      ),
      filter = "top",
      selection = "single"
    )
  },server = F)
  
  # Render Metadata DataTable
  output$data_table_metadata <- renderDataTable({
      datatable(
        df3()[[2]],
        class = "display",
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          dom = 'frtBip',
          buttons = c('csv', 'excel'),
          pageLength = 5  # Set the number of rows per page to 5
        ),
        filter = "top",
        selection = "single"
      )
  })
  
  
  output$dfstats = renderDataTable({
    datatable(
      df4(),
      class = "display",
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        dom = 'frtBip',
        buttons = c('csv', 'excel'),
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      filter = "top",
      selection = "single"
    )
  }, server = F)
  
  output$dfstats1 <- renderPlotly({
    DF <- df4()
    DF1 <- DF[, 1:3]
    P1 <- plot_ly(DF1,
                  x = names(DF1),
                  y = unlist(DF1[1, ]),
                  type = "bar")
    
    return(P1)
  })
  
  output$dfstats2 <- renderPlotly({
    DF <- df4()
    DF2 <- DF[, 4:5]
    P2 <-
      plot_ly(DF2,
              labels = names(DF2),
              values = unlist(DF2[1,]),
              type = "pie")
    return(P2)
  })
  
  observeEvent(req(input$loadBtn | input$loadBtns), {
    updateCheckboxGroupInput(
      session,
      inputId = "Panel_1",
      label = "Select A panel",
      choices =  panel_list(),
      inline = T,
      selected = panel_list()
    )
  })
  
  observeEvent(input$corplot1, {
    updatePickerInput(
      session,
      inputId = "corplot3",
      label = "Select a Value",
      choices = coropts1()[[1]],
      options = pickerOptions(liveSearch = T, dropupAuto = FALSE)
    )
  })
  
  observeEvent(input$corplot2, {
    updatePickerInput(
      session,
      inputId = "corplot4",
      label = "Select a Value",
      choices = coropts1()[[1]],
      options = pickerOptions(liveSearch = T, dropupAuto = FALSE)
    )
    
  })
  
  #### Download buttons ####
  
  plotInput = function() {
    showModal(
      modalDialog(
        title = "Please Type the file name here",
        fluidRow(column(
          width = 4,
          textInput(
            inputId = "width1",
            label = "Select the dimension of width",
            value = 600
          )
        ),
        column(
          width = 4,
          textInput(
            inputId = "height1",
            label = "Select the dimension of Height",
            value = 800
          )
        )), 
        actionButton(inputId = "saveid", label = "Save file"),
        easyClose = TRUE,
        footer = NULL
      ))
    H1<<-input$height1
    W1<<-input$width1
    return(list(H1,W1))
  }
  
  
  
  
  #   result <-paste0("The data is downloaded in ",dir1)
  #   return(result)
  
  
  output$run_filtered_data_download <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.zip', sep = '')
    },
    
    content = function(file) {
      filename <- file
      validate(need(exists("sample1"),"Please run QC first"))
      A<-input$folder
      dir1<-"workingd"
      show_modal_spinner(spin = "atom",
                         color = "firebrick",
                         text = "Please wait while the data downloads...")
      colnames(Metadata)[1] <- "SampleID"
      # olink_data_test <- olink_data[!olink_data$SampleID %in% sample1,]
      olink_data_test <- olink_data
      # olink_data_metadata <-Metadata[!Metadata$SampleID %in% sample1,]
      olink_data_metadata <-Metadata
      file1<-paste0(dir1,"/","filtered_NPXdata.csv")
      file2<-paste0(dir1,"/","filtered_metadata.csv")
      write.csv(olink_data_test,file1,row.names = F)
      write.csv(olink_data_metadata,file2,row.names = F)
      remove_modal_spinner()
      #create the zip file
      zip(filename, files = c(file1,file2))
    }
    
  )
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.html', sep = '')
    },
    content = function(file) {
      filename <- file
      htmlwidgets::saveWidget(as_widget(boxp1()[[1]]),
                              selfcontained = TRUE,
                              filename)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.png', sep = '')
    },
    content = function(file) {
      filename <- file
      H1<<-input$height1
      W1<<-input$width1
      width1 <- as.numeric(W1)
      height1<- as.numeric(H1)
      png(file,width = width1,height = height1)
      plot(qc_plot1())
      dev.off()
      
    }
  )
  
  output$download4FOUR <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.html', sep = '')
    },
    content = function(file) {
      filename <- file
      htmlwidgets::saveWidget(as_widget(volcanoplt1()[[1]]),
                              selfcontained = TRUE,
                              filename)
    }
  )
  
  output$download5 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.png', sep = '')
    },
    content = function(file) {
      filename <- file
      # filename1 <- paste0(filename,"_1")
      # filename2 <- paste0(filename,"_2")
      width <- as.numeric(input$width3)
      height <- as.numeric(input$height3)
      png(filename=filename,width = width, height = height )
      P1 <- bxplt1()
      plot(P1)
      dev.off()
      
    }
  )
  
  output$download6 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.html', sep = '')
    },
    content = function(file) {
      filename <- file
      htmlwidgets::saveWidget(as_widget(heatmap_ttest()[[1]]),
                              selfcontained = TRUE,
                              filename)
    }
  )
  
  output$download7 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.html', sep = '')
    },
    content = function(file) {
      filename <- file
      htmlwidgets::saveWidget(as_widget(heatmap_Anova()[[1]]),
                              selfcontained = TRUE,
                              filename)
    }
  )
  
  output$download8 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.png', sep = '')
    },
    content = function(file) {
      filename <- file
      # filename1 <- paste0(filename,"_1")
      # filename2 <- paste0(filename,"_2")
      width <- as.numeric(input$width4)
      height <- as.numeric(input$height4)
      png(filename=filename,width = width, height = height )
      P1 <- anovabxplt1()
      plot(P1)
      dev.off()
      
    }
  )
  
  output$download9 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.html', sep = '')
    },
    content = function(file) {
      filename <- file
      htmlwidgets::saveWidget(as_widget(heatmap1()[[1]]),
                              selfcontained = TRUE,
                              filename)
    }
  )
  
  
  output$download12 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.png', sep = '')
    },
    content = function(file) {
      filename <- file
      # filename1 <- paste0(filename,"_1")
      # filename2 <- paste0(filename,"_2")
      width <- as.numeric(input$width5)
      height <- as.numeric(input$height5)
      png(filename=filename,width = width, height = height )
      P1<- dotplot( GSEA_test()[[1]], showCategory=20, split=".sign") + facet_grid(.~.sign)
      plot(P1)
      dev.off()
      
    }
  )
  
  output$download13 <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.png', sep = '')
    },
    content = function(file) {
      filename <- file
      # filename1 <- paste0(filename,"_1")
      # filename2 <- paste0(filename,"_2")
      width <- as.numeric(input$width6)
      height <- as.numeric(input$height6)
      png(filename=filename,width = width, height = height )
      P1<- ridgeplot( GSEA_test()[[1]], showCategory=20)+ labs(x = "enrichment distribution")
      plot(P1)
      dev.off()
      
    }
  )
  
  #### end of download buttons####
  
  #### downloaded messages ####
  output$message1 <- renderPrint(downp2())
  output$message2 <- renderPrint(downp1()[[1]])
  
  output$messageq1 <- renderPrint(downqc2())
  output$messageq2 <- renderPrint(downqc1()[[1]])
  
  output$messageM1 <- renderPrint(downM2())
  output$messageM2 <- renderPrint(downM1()[[1]])
  
  output$messageDT1 <- renderPrint(downDT2())
  output$messageDT2 <- renderPrint(downDT1()[[1]])
  
  output$messageDT3 <- renderPrint(downDT4())
  output$messageDT4 <- renderPrint(downDT3()[[1]])
  
  output$messageDHT1 <- renderPrint(downDHT2())
  output$messageDHT2 <- renderPrint(downDHT1()[[1]])
  
  output$messageDAT1 <- renderPrint(downDAT2())
  output$messageDAT2 <- renderPrint(downDAT1()[[1]])
  
  output$messageDAT3 <- renderPrint(downDAT4())
  output$messageDAT4 <- renderPrint(downDAT3()[[1]])
  
  output$messageHP1 <- renderPrint(downHP2())
  output$messageHP2 <- renderPrint(downHP1()[[1]])
  
  output$messageGS1 <- renderPrint(downGS2())
  output$messageGS2 <- renderPrint(downGS1()[[1]])
  
  output$messageGS3 <- renderPrint(downGS4())
  output$messageGS4 <- renderPrint(downGS3()[[1]])
  
  output$filtered_dwnld_msg <- renderPrint(filtered_data_download())
  
  #### End of downloaded messages ####
  
  output$message3 <- renderText(NULL)
  output$message4 <- renderText(df_msg2())
  output$message5 <- renderText(df_msg3())
  
  
  #### TTest Reactive ####
  # Reactive expression for performing T-Test
  ttest2 <- eventReactive(input$ttest_run,{
    # Show spinner while T-Test is in progress
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait T-test in progress this may take some while...")
    # Extract data and metadata
    olink_data <- final_data()[[1]]
    print("str(olink_data)")
    print(str(olink_data))
    Metadata <- final_data()[[2]]
    colnames(Metadata )[1] <- "SampleID"
    print("str(Metadata)")
    print(str(Metadata))
    # olink_data_test <- olink_data[!olink_data$SampleID %in% sample1,]
    olink_data_test <- olink_data
    
    # Read metadata (Make sure that IDs are not represented as scientific exponential values. eg. ID=4.8E+11 is not allowed. ID should be completely visible)
    # (Replicated samples are manually deleted from metadata file.)
    # olink_data_metadata <-Metadata[!Metadata$SampleID %in% sample1,]
    olink_data_metadata <-Metadata
    ####NOTE: What if there are multiple conditions in the comparison variable?
    ####Answer: T test can be performed only on two conditions. Therefore, if comparison variable has more than 2 conditions then take input from user for the two desired conditions and then filter metadata accordingly. Send the filtered metadata to the t test function.
    
    # Determine type of T-Test (Paired/Unpaired) and comparison variables
    type_of_t_test <- input$tt_type
    comparison_variable <<- input$CV1
    pair_id <- input$pairid1
    condition1 <- input$tt_cond1
    condition2 <- input$tt_cond2
    reportProd$ttest_comparison_variable1 <- comparison_variable
    reportProd$ttest_pair_id1 <- pair_id
    print(reportProd$ttest_pair_id1)
    reportProd$ttest_condition11 <- condition1
    reportProd$ttest_condition21 <- condition2
    
    # Filter metadata based on conditions
    condition1_data <-
      olink_data_metadata %>% filter(olink_data_metadata[c(comparison_variable)] == condition1)
    condition2_data <-
      olink_data_metadata %>% filter(olink_data_metadata[c(comparison_variable)] == condition2)
    
    condition1_data$newgroup <- "Case" #V2
    condition2_data$newgroup <- "Control" #V6
    olink_data_metadata1 <- rbind(condition1_data, condition2_data)
    
    # Perform T-Test based on type (Paired/Unpaired)
    if (type_of_t_test == "Paired") {
      new_uniq <-
        olink_data_metadata1[!duplicated(olink_data_metadata1[c(pair_id, comparison_variable)]), ]
      
      olink_data_metadata <-
        new_uniq %>% group_by(new_uniq[c(pair_id)]) %>% filter(n() > 1)
    }else if (type_of_t_test == "Unpaired"){
      olink_data_metadata <- olink_data_metadata1
    }
    # Execute T-Test and store results
    t_test_result <- t_test(olink_data = olink_data_test,
                             olink_data_metadata =olink_data_metadata,
                             type = type_of_t_test,
                             variable =  "newgroup",
                             pair_id =  pair_id)
    # Hide spinner after T-Test is completed
    remove_modal_spinner()
    return(list(t_test_result)) # Return the T-Test result
  })
  
  #### MWU Test Reactive ####
  # Reactive expression for performing Mann-Whitney U Test
  mwutest2 <- eventReactive(input$mwutest_run,{
    # Show spinner while MWU Test is in progress
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait Mann-Whitney U test in progress this may take some while...")
    # Extract data and metadata
    olink_data <- final_data()[[1]]
    Metadata <- final_data()[[2]]
    colnames(Metadata )[1] <- "SampleID"
    # olink_data_test <- olink_data[!olink_data$SampleID %in% sample1,]
    olink_data_test <- olink_data
    
    # Read metadata (Make sure that IDs are not represented as scientific exponential values. eg. ID=4.8E+11 is not allowed. ID should be completely visible)
    # (Replicated samples are manually deleted from metadata file.)
    # olink_data_metadata <-Metadata[!Metadata$SampleID %in% sample1,]
    olink_data_metadata <-Metadata
    ####NOTE: What if there are multiple conditions in the comparison variable?
    ####Answer: MWU test can be performed only on two conditions. Therefore, if comparison variable has more than 2 conditions then take input from user for the two desired conditions and then filter metadata accordingly. Send the filtered metadata to the t test function.
    
    # Determine type of MWU Test (Paired/Unpaired) and comparison variables
    type_of_mwu_test <<- input$mwut_type
    comparison_variable <<- input$mwutCV1
    pair_id <<- input$mwutpairid1
    
    condition1 <<- input$mwut_cond1
    condition2 <<- input$mwut_cond2
    reportProd$type_of_mwu_test1 <- type_of_mwu_test
    reportProd$mwut_comparison_variable <- comparison_variable
    reportProd$mwut_pair_id <- pair_id
    reportProd$mwut_condition1 <- condition1 
    reportProd$mwut_condition2 <- condition2
    # Filter metadata based on conditions
    condition1_data <-
      olink_data_metadata %>% filter(olink_data_metadata[c(comparison_variable)] == condition1)
    condition2_data <-
      olink_data_metadata %>% filter(olink_data_metadata[c(comparison_variable)] == condition2)
    
    condition1_data$newgroup <- "Case" #V2
    condition2_data$newgroup <- "Control" #V6
    olink_data_metadata1 <- rbind(condition1_data, condition2_data)
    # Perform MWU Test based on type (Paired/Unpaired)
    if (type_of_mwu_test == "Paired") {
      new_uniq <-
        olink_data_metadata1[!duplicated(olink_data_metadata1[c(pair_id, comparison_variable)]), ]
      olink_data_metadata <-
        new_uniq %>% group_by(new_uniq[c(pair_id)]) %>% filter(n() > 1)
    }else if (type_of_mwu_test == "Unpaired"){
      olink_data_metadata <- olink_data_metadata1
    }
    # Execute MWU Test and store results
    mwu_test_result <<- mwu_test(olink_data = olink_data_test,
                                 olink_data_metadata =olink_data_metadata,
                                 type = type_of_mwu_test,
                                 variable =  "newgroup",
                                 pair_id =  pair_id)
    remove_modal_spinner() # Hide spinner after MWU Test is completed
    return(list(mwu_test_result)) # Return the MWU Test result
  })
  
  #### ANOVA Reactive #### 
  # Reactive expression for performing ANOVA Test
   Anova_test <- eventReactive(input$anova_run, {
     # Show spinner while ANOVA Test is in progress
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait anova in progress this may take some while...")
     # Extract data and metadata
    olink_data <- final_data()[[1]]
    Metadata <- final_data()[[2]]
    
    # olink_data_test <- olink_data[!olink_data$SampleID %in% sample1,]
    olink_data_test <- olink_data
    # Read metadata (Make sure that IDs are not represented as scientific exponential values. eg. ID=4.8E+11 is not allowed. ID should be completely visible)
    # (Replicated samples are manually deleted from metadata file.)
    colnames(Metadata)[1] <- "SampleID"
    # olink_data_metadata <-Metadata[!Metadata$SampleID %in% sample1,]
    olink_data_metadata <-Metadata
    
    comparison_variable1 <- input$an_CV1
    comparison_variable2 <- input$an_CV3
    covariate <- input$an_CV4
    pair_id <- input$an_CV2
    
    reportProd$anova_comparison_variable1 <- comparison_variable1
    reportProd$anova_comparison_variable2 <- comparison_variable2
    reportProd$anova_covariate <- covariate
    reportProd$anova_pair_id <- pair_id
    
    # comparison_variable<- "Visit.Name"
    # pair_id <- "Subject.ID"
    new_uniq <-
      olink_data_metadata[!duplicated(olink_data_metadata[c(pair_id, comparison_variable1)]), ]
    olink_data_metadata <<-
      new_uniq %>% group_by(new_uniq[c(pair_id)]) %>% filter(n() > 1)
    
    result <-
      anova_test(olink_data_test, olink_data_metadata, comparison_variable1,comparison_variable2,covariate)
    result[[1]]$Threshold<-NULL
    result<- result
    remove_modal_spinner()
    return(result) # return the results
    
  })
  
  Anova_posthoc <- reactive({
    ddf2<<-Anova_test()[[2]]
    rownum2<<-Anova_test()[[1]][input$anovatest_table_rows_selected,1]
    # rownum2<-result[[1]][36,1]
    ddf3<<-ddf2[ddf2$Assay %in% rownum2[1,1],]
    return(ddf3)
  })
  
  ## select contrast based ont he test chosen 
  unq_contrasts <- eventReactive(input$GSEA_option, {
    if (exists("ddf2")) {
      unq1 <- unique(ddf2$contrast)
    } else{
      unq1 <-
        c("No Contrasts are present as Post-hoc test is not done with Anova")
    }
    
  })
  
  observeEvent(input$GSEA_option,{
    updateSelectInput(session,
                      inputId = "contrast1",
                      label = "Select a contrast",
                      choices = unq_contrasts())
  })
  
  ## select contrast for timecourse analysis results 
  observeEvent(input$GSEA_option,{
    updateSelectInput(session,inputId = "tca_contrast1",label = "Select a contrast",choices = unique(lmer_posthoc_analysis()$contrast))
    
  })
  
  ## select contrast or group from Machine learning analysis 
  observeEvent(input$ml_model,{
    
    req(input$ml_model)

    L = reactiveValuesToList(x = ML_results)
    L2 = lapply(L, function(i) i[[1]])
    res = L2[[input$ml_model]]
    if ( input$ml_model == "LMM"){
      print("model is lmm")
      vi = imp_lmm(res)
      nms = rownames(vi)
      vi = as.data.frame(vi[,-1])
      rownames(vi) = nms
      colnames(vi) = "Overall"
      choices1 = colnames(vi)
      View(vi)
      
    } else {
      print("this is other models")
      res2 = varImp(res)
      res2 = res2$importance 
      choices1 = colnames(res2)
      
    }
    
    
    
    updateSelectInput(session,
                      inputId = "gsea_group",
                      label = "Select a group",
                      choices = choices1)
                        
    
  })
   
  
  #### Heat map UI ####
  
  ##### heat map 1 #####
  # olink_data = mat_full
  # 
  # heatmap1 <- reactive({
  #   #olink_data <- olink_data[!olink_data$SampleID %in% sample1,]
  #   # olink_data <- olink_hm
  #   olink_data_test <-
  #     data.frame(
  #       "SampleID" = olink_data$SampleID,
  #       "Assay" = olink_data$Assay,
  #       "NPX" = olink_data$NPX
  #     )
  #   
  #   
  #   parms <-
  #     olink_data_test %>% group_by(Assay) %>% summarise(NPX_mean = mean(NPX), NPX_sd = sd(NPX))
  #   LFJ <- left_join(olink_data_test, parms, by = "Assay")
  #   
  #   olink_data_test$NPX <- (LFJ$NPX + (-LFJ$NPX_mean)) / (LFJ$NPX_sd)
  #   
  #   olink_data_test.wide <-
  #     pivot_wider(olink_data_test,
  #                 names_from = Assay,
  #                 values_from = NPX)
  #   
  #   df1 <- olink_data_test %>%
  #     group_by(Assay) %>%
  #     mutate(row = row_number()) %>%
  #     tidyr::pivot_wider(names_from = Assay, values_from = NPX) %>%
  #     select(-row)
  #   
  #   df<-as.data.frame(olink_data_test.wide)
  #   row.names(df)<-df[,1]
  #   df=df[,-1]
  #   df2 <- mutate_all(df, function(x) as.numeric(as.character(x)))
  #   
  #   anyMissing(df2)
  #   df3 <- as.matrix.data.frame(df2[,colSums(is.na(df2)) == 0])
  #   
  #   #if colv = TRUE, column clustering is enabled, colv = FALSE, column clustering is disabled
  #   #if Rowv = TRUE, column clustering is enabled, Rowv = FALSE, column clustering is disabled
  #   #showticklabels : To enable labels make it to TRUE
  #   
  #   
  #   
  #   x  <- as.matrix(df3)
  #   rc <- colorspace::rainbow_hcl(nrow(x))
  #   
  #   df3[df3 < as.numeric(input$heatmaplgnd)] <-
  #     as.numeric(input$heatmaplgnd)
  #   
  #   P1<-heatmaply(df3,
  #                 xlab = "Assay", 
  #                 ylab = "SampleID", 
  #                 dynamicTicks= FALSE,
  #                 showticklabels=FALSE,
  #                 Colv = input$heatmapopts2,
  #                 Rowv = input$heatmapopts1,
  #                 RowSideColors = rc,
  #                 colors = cool_warm)
  #   P1$x$layout$showlegend<-F
  #   P1$x$layout$xaxis2$ticktext<-""
  #   return(P1)
  # })
  # 
  ##### heatmap ttest #####
  
  heatmap_ttest <- reactive({
    t_test_result <<- ttest2()[[1]] 
    val <<- as.numeric(input$Hmap_pval)
    # olink_data <- olink_data[!olink_data$SampleID %in% sample1,]
    olink_hm <-
      olink_data[olink_data$Assay %in% t_test_result[t_test_result$p.value < val, ]$Assay, ]
    olink_data <- olink_hm
    olink_data_metadata1<-Metadata
    names(olink_data_metadata1)[1] <- "SampleID"
    olink_full<-left_join(olink_data,olink_data_metadata1,by = "SampleID")
    olink_data<-olink_full[order(olink_full[[input$CV1]]),]
    # olink_data<-olink_full[order(olink_full[["Visit.Name"]]),]
    olink_data_test <-
      data.frame(
        "SampleID" = olink_data$SampleID,
        "Assay" = olink_data$Assay,
        "NPX" = olink_data$NPX
      )
    
    parms <-
      olink_data_test %>% group_by(Assay) %>% summarise(NPX_mean = mean(NPX), NPX_sd = sd(NPX))
    LFJ <- left_join(olink_data_test, parms, by = "Assay")
    olink_data_test$NPX <- (LFJ$NPX + (-LFJ$NPX_mean)) / (LFJ$NPX_sd)
    olink_data_test.wide <-
      pivot_wider(olink_data_test,
                  names_from = Assay,
                  values_from = NPX)
    
    df1 <- olink_data_test %>%
      group_by(Assay) %>%
      mutate(row = row_number()) %>%
      tidyr::pivot_wider(names_from = Assay, values_from = NPX) %>%
      select(-row)
    
    df<-as.data.frame(olink_data_test.wide)
    row.names(df)<-df[,1]
    df=df[,-1]
    df2 <- mutate_all(df, function(x) as.numeric(as.character(x)))
    
    anyMissing(df2)
    df3 <- as.matrix.data.frame(df2[,colSums(is.na(df2)) == 0])
    
    #if colv = TRUE, column clustering is enabled, colv = FALSE, column clustering is disabled
    #if Rowv = TRUE, column clustering is enabled, Rowv = FALSE, column clustering is disabled
    #showticklabels : To enable labels make it to TRUE
    #
    
    x  <- as.matrix(df3)
    col1 <- input$CV1
    A1<-olink_data[(!duplicated(olink_data$SampleID)),]
    A2<-A1
    unq<-c(na.omit(unique(A2[[col1]])))
    colrs<-c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f')
    A3<-colrs[1:length(unq)]
    names(A3)<-unq
    
    
    P1 <- heatmaply(
      df3,
      xlab = "Assay",
      ylab = "SampleID",
      dynamicTicks = FALSE,
      showticklabels = FALSE,
      Colv = T,
      Rowv = T,
      row_side_colors = as.factor(A2[[col1]]),
      row_side_palette = A3,
      colors = cool_warm
    )
    P1$x$layout$showlegend<-F
    P1$x$layout$xaxis2$ticktext<-""
    reportProd$heatmap_ttest1 <- P1
    # heatmap_file <- "plots/ttest/heatmap.html"
    # saveWidget(P1, file = heatmap_file)
    # webshot(heatmap_file, "plots/ttest/heatmap.png")
    return(P1)
  })
  
  ##### heatmap mwutest #####
  heatmap_mwutest <- reactive({
    mwu_test_result <<- mwutest2()[[1]] 
    val <<- as.numeric(input$mwutHmap_pval)
    # olink_data <- olink_data[!olink_data$SampleID %in% sample1,]
    olink_hm <-
      olink_data[olink_data$Assay %in% mwu_test_result[mwu_test_result$p.value < val, ]$Assay, ]
    olink_data <- olink_hm
    olink_data_metadata1<-Metadata
    names(olink_data_metadata1)[1] <- "SampleID"
    olink_full<-left_join(olink_data,olink_data_metadata1,by = "SampleID")
    olink_data<-olink_full[order(olink_full[[input$CV1]]),]
    # olink_data<-olink_full[order(olink_full[["Visit.Name"]]),]
    olink_data_test <-
      data.frame(
        "SampleID" = olink_data$SampleID,
        "Assay" = olink_data$Assay,
        "NPX" = olink_data$NPX
      )
    
    parms <-
      olink_data_test %>% group_by(Assay) %>% summarise(NPX_mean = mean(NPX), NPX_sd = sd(NPX))
    LFJ <- left_join(olink_data_test, parms, by = "Assay")
    olink_data_test$NPX <- (LFJ$NPX + (-LFJ$NPX_mean)) / (LFJ$NPX_sd)
    olink_data_test.wide <-
      pivot_wider(olink_data_test,
                  names_from = Assay,
                  values_from = NPX)
    
    df1 <- olink_data_test %>%
      group_by(Assay) %>%
      mutate(row = row_number()) %>%
      tidyr::pivot_wider(names_from = Assay, values_from = NPX) %>%
      select(-row)
    
    df<-as.data.frame(olink_data_test.wide)
    row.names(df)<-df[,1]
    df=df[,-1]
    df2 <- mutate_all(df, function(x) as.numeric(as.character(x)))
    
    anyMissing(df2)
    df3 <- as.matrix.data.frame(df2[,colSums(is.na(df2)) == 0])
    
    #if colv = TRUE, column clustering is enabled, colv = FALSE, column clustering is disabled
    #if Rowv = TRUE, column clustering is enabled, Rowv = FALSE, column clustering is disabled
    #showticklabels : To enable labels make it to TRUE
    #
    
    x  <- as.matrix(df3)
    col1 <- input$CV1
    A1<-olink_data[(!duplicated(olink_data$SampleID)),]
    A2<-A1
    unq<-c(na.omit(unique(A2[[col1]])))
    colrs<-c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f')
    A3<-colrs[1:length(unq)]
    names(A3)<-unq
    
    
    P1 <- heatmaply(
      df3,
      xlab = "Assay",
      ylab = "SampleID",
      dynamicTicks = FALSE,
      showticklabels = FALSE,
      Colv = T,
      Rowv = T,
      row_side_colors = as.factor(A2[[col1]]),
      row_side_palette = A3,
      colors = cool_warm
    )
    P1$x$layout$showlegend<-F
    P1$x$layout$xaxis2$ticktext<-""
    reportProd$heatmap_mwutest1 <- P1
    # heatmap_file <- "plots/mwutest/heatmap.html"
    # saveWidget(P1, file = heatmap_file)
    # webshot(heatmap_file, "plots/mwutest/heatmap.png")
    return(P1)
  })
  
  ##### heatmap Anova #####
  heatmap_Anova <- reactive({
    Anova_result <<- Anova_test()[[1]] 
    #val <- 0.05
    # olink_data[!olink_data$SampleID %in% sample1,]
    val <<- as.numeric(input$Hmap2_pval)
    olink_hm <<-
      olink_data[olink_data$Assay %in% Anova_result[Anova_result$p.value < val, ]$Assay, ]
    olink_data <- olink_hm
    olink_data_metadata1<-Metadata
    names(olink_data_metadata1)[1] <- "SampleID"
    olink_full<-left_join(olink_data,olink_data_metadata1,by = "SampleID")
    #olink_data<-olink_full[order(olink_full$Gender),]
    olink_data<-olink_full[order(olink_full[[input$an_CV1]]),]
    olink_data_test <-
      data.frame(
        "SampleID" = olink_data$SampleID,
        "Assay" = olink_data$Assay,
        "NPX" = olink_data$NPX
      )
    parms <-
      olink_data_test %>% group_by(Assay) %>% summarise(NPX_mean = mean(NPX), NPX_sd = sd(NPX))
    
    LFJ <- left_join(olink_data_test, parms, by = "Assay")
    
    olink_data_test$NPX <- (LFJ$NPX + (-LFJ$NPX_mean)) / (LFJ$NPX_sd)
    
    
    olink_data_test.wide <-
      pivot_wider(olink_data_test,
                  names_from = Assay,
                  values_from = NPX)
    
    df1 <- olink_data_test %>%
      group_by(Assay) %>%
      mutate(row = row_number()) %>%
      tidyr::pivot_wider(names_from = Assay, values_from = NPX) %>%
      select(-row)
    
    df<-as.data.frame(olink_data_test.wide)
    row.names(df)<-df[,1]
    df=df[,-1]
    df2 <- mutate_all(df, function(x) as.numeric(as.character(x)))
    
    anyMissing(df2)
    df3 <<- as.matrix.data.frame(df2[,colSums(is.na(df2)) == 0])
    
    #if colv = TRUE, column clustering is enabled, colv = FALSE, column clustering is disabled
    #if Rowv = TRUE, column clustering is enabled, Rowv = FALSE, column clustering is disabled
    #showticklabels : To enable labels make it to TRUE
    #
    
    x  <- as.matrix(df3)
    col1 <- input$heatmapcol
    # col1 <- "Gender"
    A1<-olink_data[(!duplicated(olink_data$SampleID)),]
    A2<-A1
    unq<-c(na.omit(unique(A2[[col1]])))
    colrs<-c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f')
    A3<-colrs[1:length(unq)]
    names(A3)<-unq
    
    P1 <- heatmaply(
      df3,
      xlab = "Assay",
      ylab = "SampleID",
      dynamicTicks = FALSE,
      showticklabels = FALSE,
      Colv = T,
      Rowv = T,
      row_side_colors = as.factor(A2[[col1]]),
      row_side_palette = A3,
      colors = cool_warm
    )
    P1$x$layout$showlegend<-F
    P1$x$layout$legend$title$text <- "r"
    P1$x$layout$xaxis2$ticktext<-""
    reportProd$heatmap_Anova1 <- P1
    # heatmap_file <- "plots/ANOVA/heatmap.html"
    # saveWidget(P1, file = heatmap_file)
    # webshot(heatmap_file, "plots/ANOVA/heatmap.png")
    return(P1)
  })
  
  
  
  #### GSEA Reactive ####
  
  GSEA_test <- eventReactive(input$GSEA_run,{
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait while GSEA is in progress....")
    #Read Olink NPX data in long format
    olink_data_test <<- final_data()$npx
    # olink_data_test <- olink_data[!olink_data$SampleID %in% sample1,]
    
    # Read metadata 
    olink_data_metadata <- final_data()$metadata    
    colnames(olink_data_metadata )[1] <- "SampleID"
    # olink_data_metadata <- olink_data_metadata[!olink_data_metadata$SampleID %in% sample1,]
    olink_data_metadata <<-data.frame(olink_data_metadata)
    
    
    # Check sampleIDs from metadata and NPX data. if missing samples are present, then give error
    unique_samples_test <<- unique(olink_data_test$SampleID)
    unique_samples_metadata <<- unique(olink_data_metadata$SampleID)
    if (length(unique_samples_test) == length(unique_samples_metadata)) {
      print("Warning! Unique samples in NPX data and metadata match!")
    } else{
      print(
        "Warning! Unique samples in NPX data and metadata do not match! Only intersecting samples will be used in T test"
      )
    }
    
    
    
    # Identify diagnostic markers by performing t test
    if (input$GSEA_option == "ttest") {
      result <- ttest2()[[1]]
    } else if(input$GSEA_option == "ANOVA") {
      
      ddf2 = Anova_test()[[2]]
      ddf = ddf2[ddf2$contrast == input$contrast1,]
      result <<- ddf
      result$LogFC <- result$estimate
      print("anova result..")
      print(head(result))
      
    } else if(input$GSEA_option == "Man Whitney U-Test"){
      
      result = mwutest2()[[1]]

    } else if (input$GSEA_option == "Timecourse"){
      
      result = lmer_posthoc_analysis()
      result = result[result$contrast == input$tca_contrast1,]
      result$LogFC <- result$estimate
      
    } else if (input$GSEA_option == "Machine Learning"){
      
      
      L = reactiveValuesToList(x = ML_results)
      L = Filter(Negate(is.null), L)
      Ml_res = lapply(L, function(inner_list) inner_list[[1]])
      mdl = Ml_res[[input$ml_model]]
      print(mdl)
      
      if ( input$ml_model == "LMM"){
        print("model is lmm")
        vi = imp_lmm(mdl)
        nms = rownames(vi)
        vi = as.data.frame(vi[,-1])
        rownames(vi) = nms
        colnames(vi) = "Overall"
        vi$Assay = rownames(vi)
        result = data.frame("LogFC" =vi[[input$gsea_group]], "Assay" = vi$Assay)
        
      } else {
        print("this is other models")
        vi = varImp(mdl)
        vi_df = vi$importance
        vi_df$Assay = rownames(vi_df)
        result = data.frame("LogFC"= vi_df[[input$gsea_group]], "Assay" = vi_df$Assay)
      }
      
      
      
    }
    
    # Construct genelist from t test result
    print("starting gsea analysis..")
    genelist <<- construct_genelist(result)
    
    # Take input from user for genesets: mSigDB gene sets
    if(input$GSEA_option2 == "msigdb"){
      
      # Extract selected Genesets from msigdbr library
      msigDB_gene_Sets <<- extract_geneset(input$gsspecies, input$gscategory)
      
    } else {
      
      IPA_Gene_set_Human <- read_excel(input$ipfile1$datapath)                                                                            
      IPA_Gene_set_Human <- data.frame(IPA_Gene_set_Human)
      msigDB_gene_Sets <<- IPA_Gene_set_Human
    }
    
    
    # Perform Gene set enrichment analysis
    p_adjust_cutoff <<- input$p_adjust_cutoff 
    gsea_result <<- gsea(genelist, msigDB_gene_Sets, p_adjust_cutoff)
    print(head(gsea_result))
    gsea_output <<- as.data.frame(gsea_result)
    gsea_output <- mutate_if(gsea_output, is.numeric, ~round(., digits=2))
    remove_modal_spinner()
    return(list(gsea_result,gsea_output))
    
    print(gsea_result)
    print(gsea_output)
  })
  
  #### diagnostic marker plots ####
  
  volcanoplt1<- reactive({
    validate(need("LogFC"%in%names(ttest2()[[1]])==TRUE,
                  message = "LogFC Value should be present for the plot to dispaly"))
    df<- ttest2()[[1]]
    df_ttest<<-df
    
    val1<<-as.numeric(input$maxpval)
    val2<<-as.numeric(input$minpval)
    val3<<-as.numeric(input$maxlogfc)
    val4<<-as.numeric(input$minlogfc)
    
    
    if (input$volploty == "pvalue"){
      df$p.valuelog = -log10(df$p.value)
      df2<-df%>%filter(p.valuelog < val2 & p.valuelog > val1 | 
                         LogFC > val4 | LogFC < val3) 
      df3<-df[!df$Assay %in% df2$Assay,] 
      y1 = df2$p.valuelog
      y2 = df3$p.valuelog
      ytit<-"pvalue(-log10)"
    }else if (input$volploty == "adjusted_pvalue"){
      df$Adjusted_pvallog = -log10(df$Adjusted_pval)
      df2<-df%>%filter(Adjusted_pvallog < val2 & Adjusted_pvallog > val1)
      df3<-df[!df$Assay %in% df2$Assay,] 
      y1 = df2$Adjusted_pvallog
      y2 = df3$Adjusted_pvallog
      ytit<-"adjpvalue(-log10)"
    }
  
    p <- plot_ly() %>%
      add_trace(
        data = df2,
        x = ~ LogFC,
        y = y1,
        text = df2$Assay,
        marker = list(color = "red"),name = "Un-Selected points"
      ) %>%
      add_trace(
        data = df3,
        x = ~ LogFC,
        y = y2,
        text = df3$Assay,
        marker = list(color = "blue"),name = "Selected points"
      ) %>%
      layout(yaxis = list( title = ytit))
    reportProd$volcanoplt_ttest1 <- p
    # saveWidget(p, file = "plots/ttest/volcono_plotly_plot.html")
    # export(p, "plots/ttest/volcano_plotly_plot.png")
    return(p)
  })
  
  ###
  mwuvolcanoplt1<- reactive({
    validate(need("LogFC"%in%names(mwutest2()[[1]])==TRUE,
                  message = "LogFC Value should be present for the plot to dispaly"))
    df<- mwutest2()[[1]]
    df_mwutest<<-df
    
    val1<<-as.numeric(input$maxpval)
    val2<<-as.numeric(input$minpval)
    val3<<-as.numeric(input$maxlogfc)
    val4<<-as.numeric(input$minlogfc)
    
    
    if (input$volploty == "pvalue"){
      df$p.valuelog = -log10(df$p.value)
      df2<-df%>%filter(p.valuelog < val2 & p.valuelog > val1 | 
                         LogFC > val4 | LogFC < val3) 
      df3<-df[!df$Assay %in% df2$Assay,] 
      y1 = df2$p.valuelog
      y2 = df3$p.valuelog
      ytit<-"pvalue(-log10)"
    }else if (input$volploty == "adjusted_pvalue"){
      df$Adjusted_pvallog = -log10(df$Adjusted_pval)
      df2<-df%>%filter(Adjusted_pvallog < val2 & Adjusted_pvallog > val1)
      df3<-df[!df$Assay %in% df2$Assay,] 
      y1 = df2$Adjusted_pvallog
      y2 = df3$Adjusted_pvallog
      ytit<-"adjpvalue(-log10)"
    }
    
    p <- plot_ly() %>%
      add_trace(
        data = df2,
        x = ~ LogFC,
        y = y1,
        text = df2$Assay,
        marker = list(color = "red"),name = "Un-Selected points"
      ) %>%
      add_trace(
        data = df3,
        x = ~ LogFC,
        y = y2,
        text = df3$Assay,
        marker = list(color = "blue"),name = "Selected points"
      ) %>%
      layout(yaxis = list( title = ytit))
    # saveWidget(p, file = "plots/mwutest/volcono_plotly_plot.html")
    # export(p, "plots/mwutest/volcano_plotly_plot.png")
    return(p)
  })
  
  #####
  nboxplot <- function(df_singlegene, var1 = NULL, var2 = NULL){
    
    if( !is.null(var1) & !is.null(var2)){
      # If both var_1 and var_2 are selected
      p <- ggplot(df_singlegene, aes_string(x=var1, y="NPX", fill=var1)) +
        geom_boxplot() +
        geom_boxplot(aes(color = var1),fatten = NULL, fill = NA, coef = 0, outlier.alpha = 0,show.legend = F)+
        theme_bw()+theme(strip.text = element_text(size=20,face="bold.italic"), 
                         axis.title = element_text(size = 16),
                         axis.text= element_text(size = 14))
      print(p + facet_grid(.~get(var2)))
    }
    
    else{
      # If only var_1 is selected
      p <- ggplot(df_singlegene, aes_string(x=var1, y="NPX", fill=var1)) +
        geom_boxplot() +
        geom_boxplot(aes(color = var1),fatten = NULL, fill = NA, coef = 0, outlier.alpha = 0,show.legend = F)+
        theme_bw()+theme(strip.text = element_text(size=20,face="bold.italic"), axis.title = element_text(size = 20),axis.text= element_text(size = 18))
      p
      
    }
    
  }
  #####
  
  bxplt1 <- reactive({
    NPX <- olink_data
    metadata <- Metadata
    # convert all columns of metadata to factors.
    col_names <- names(metadata)
    metadata[, col_names] <- lapply(metadata[, col_names], factor)
    names(metadata)[1] <- "SampleID"
    
    # Check if any row is selected in the ttest_table
    if (is.null(input$ttest_table_rows_selected) || length(input$ttest_table_rows_selected) == 0) {
      return(
        div(
          style = "color: red; font-size: 14px;",
          "Please select a row from the table to generate the plot."
        )
      )
    }
    
    #------------The following patch of code has already been implemented in the pipeline. Please do not repeat it------------
    
    # Check sampleIDs from metadata and NPX data. if missing samples are present, then give error
    unique_samples_NPX <- unique(NPX$SampleID)
    unique_samples_metadata <- unique(metadata$SampleID)
    joined_olink_data <- inner_join(NPX, metadata, by = "SampleID")
    
    var_1 <<- input$CV1
    var_2 <<- input$ttbx2
    
    #------Input for Assay name--------
    # The "Assay" should be the protein that user selects from T test/Anova result for visualizing box plot. The selected protein name should be taken as an input for Assay variable.
    rownum1 <<- ttest2()[[1]][input$ttest_table_rows_selected, 1]
    pval <- ttest2()[[1]][input$ttest_table_rows_selected, 6]
    adjpval <- ttest2()[[1]][input$ttest_table_rows_selected, 9]
    pval <- scientific(pval, digits = 3)
    adjpval <- scientific(adjpval, digits = 3)
    Assays <- unique(joined_olink_data$Assay)
    rownum <<- grep(rownum1[1, 1], Assays)
    Assay_data <- joined_olink_data %>% filter(Assay == Assays[rownum])
    AA <<- unique(Assay_data$Assay)
    joined_olink_data_one_gene <<- joined_olink_data[which(joined_olink_data$Assay == AA), ]
    
    BB <<- input$CV1
    if (input$ttbx2 == "") {
      CC <<- NULL
    } else {
      CC <<- input$ttbx2
    }
    fig <- nboxplot(joined_olink_data_one_gene, BB, CC)
    fig <- fig + annotate("text", x = Inf, y = Inf, label = paste0("pval : ", pval), vjust = 1, hjust = 1)
    fig <- fig + annotate("text", x = Inf, y = Inf, label = paste0("adjpval : ", adjpval), vjust = 2, hjust = 1)
    fig <- fig + ggtitle(rownum1)
    fig <- fig + theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5))
    reportProd$bxplt1_ttest1 <- fig
    fig
  })
  
  mwutbxplt1<- reactive({
    NPX<- olink_data
    metadata <- Metadata
    # convert all columns of metadata to factors.
    col_names <- names(metadata)
    metadata[,col_names] <- lapply(metadata[,col_names] , factor)
    names(metadata)[1]<- "SampleID"
    
    #------------The following patch of code has already been implemented in the pipeline. Please do not repeat it------------
    
    # Check sampleIDs from metadata and NPX data. if missing samples are present, then give error
    unique_samples_NPX = unique(NPX$SampleID)
    unique_samples_metadata = unique(metadata$SampleID)
    joined_olink_data <- inner_join(NPX, metadata, by="SampleID")
    
    var_1 <- input$mwutCV1
    var_2 <- input$mwutbx2
    
    #------Input for Assay name--------
    # The "Assay" should be the protein that user selects from T test/Anova result for visualizing box plot. The selected protein name should be taken as an input for Assay variable.
    rownum1<-mwutest2()[[1]][input$mwutest_table_rows_selected,1]
    pval <- mwutest2()[[1]][input$mwutest_table_rows_selected,6]
    adjpval <- mwutest2()[[1]][input$mwutest_table_rows_selected,9]
    pval <- scientific(pval,digits = 3)
    adjpval <- scientific(adjpval,digits = 3)
    Assays<-unique(joined_olink_data$Assay)
    rownum<-grep(rownum1[1,1],Assays)
    Assay_data<-joined_olink_data%>% filter(Assay == Assays[rownum])
    AA <- unique(Assay_data$Assay)
    joined_olink_data_one_gene <- joined_olink_data[which(joined_olink_data$Assay==AA), ]
    
    BB <- input$mwutCV1
    if (input$mwutbx2 == "") {
      CC <- NULL
    } else{
      CC <- input$mwutbx2
    }
    fig<-nboxplot(joined_olink_data_one_gene,BB,CC)
    fig<- fig + annotate("text",  x=Inf, y = Inf, label = paste0("pval : ",pval), vjust=1, hjust=1)
    fig<- fig + annotate("text",  x=Inf, y = Inf, label = paste0("adjpval : ",adjpval), vjust=2, hjust=1)
    fig<- fig+ggtitle(rownum1)
    fig<- fig + theme(plot.title = element_text(size = 40, face = "bold",hjust = 0.5))
    # ggsave("plots/mwutest/nboxplot.png", plot = fig, width = 10, height = 8, units = "in")
    reportProd$mwutbxplt1_mwut <- fig
    fig
  })
  
  anovabxplt1 <- reactive({
    NPX<- olink_data
    metadata <- Metadata
    # convert all columns of metadata to factors.
    col_names <- names(metadata)
    metadata[,col_names] <- lapply(metadata[,col_names] , factor)
    names(metadata)[1]<- "SampleID"
    #------------The following patch of code has already been implemented in the pipeline. Please do not repeat it------------
    
    # Check sampleIDs from metadata and NPX data. if missing samples are present, then give error
    unique_samples_NPX = unique(NPX$SampleID)
    unique_samples_metadata = unique(metadata$SampleID)
    joined_olink_data <- inner_join(NPX, metadata, by="SampleID")
    
    var_1 <<- input$anbx1
    var_2 <<- input$anbx2
    
    #------Input for Assay name--------
    # The "Assay" should be the protein that user selects from T test/Anova result for visualizing box plot. The selected protein name should be taken as an input for Assay variable.
    rownum1<<-Anova_test()[[1]][input$anovatest_table_rows_selected,1]
    Assays<-unique(joined_olink_data$Assay)
    pval <- Anova_test()[[1]][input$anovatest_table_rows_selected,9]
    adjpval <- Anova_test()[[1]][input$anovatest_table_rows_selected,10]
    pval <- scientific(pval,digits = 3)
    adjpval <- scientific(adjpval,digits = 3)
    rownum<<-grep(rownum1[1,1],Assays)
    Assay_data<-joined_olink_data%>% filter(Assay == Assays[rownum])
    AA <<- unique(Assay_data$Assay)
    joined_olink_data_one_gene <<- joined_olink_data[which(joined_olink_data$Assay==AA), ]
    
    BB <<- input$anbx1
    if (input$anbx2 == "") {
      CC <<- NULL
    } else{
      CC <<- input$anbx2
    }
    fig<-nboxplot(joined_olink_data_one_gene,BB,CC)
    fig<- fig + annotate("text",  x=Inf, y = Inf, label = paste0("pval : ",pval), vjust=1, hjust=1)
    fig<- fig + annotate("text",  x=Inf, y = Inf, label = paste0("adjpval : ",adjpval), vjust=2, hjust=1)
    fig<- fig+ggtitle(rownum1)
    fig<- fig + theme(plot.title = element_text(size = 40, face = "bold",hjust = 0.5))
    # ggsave("plots/ANOVA/nboxplot.png", plot = fig, width = 10, height = 8, units = "in")
    reportProd$anovabxplt1_an<-fig
    fig
    
  })
  
  #### qc plots render ####
  
  output$plt2 <- renderPlot({
    qc_plot1()
  })
  
  output$plt3 <- renderPlot({
    missingprtplot()[[1]]
  })
  
  output$plt4 <- renderPlot({
    missingprtplot()[[2]]
  })
  
  #### Diagnostic marker tables ####
  output$ttest_table <- renderDataTable({
    datatable(
      ttest2()[[1]],
      class = "display",
      filter = "top",
      selection = "single",
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        scrollY = "250px",
        # dom = 'frtBip',
        buttons = c('csv', 'excel')
      ),
    ) %>%
      formatRound(columns=c('LogFC', 'p.value', 'Adjusted_pval'), digits=4)
  }, server = F)
  
  output$mwutest_table <- renderDataTable({
    datatable(
      mwutest2()[[1]],
      class = "display",
      filter = "top",
      selection = "single",
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        scrollY = "250px",
        # dom = 'frtBip',
        buttons = c('csv', 'excel')
      ),
    ) %>%
      formatRound(columns=c('LogFC', 'p.value', 'Adjusted_pval'), digits=4)
  }, server = F)
  
 output$anovatest_table <- renderDataTable({
    datatable(
      Anova_test()[[1]],
      class = "display",
      filter = "top",
      selection = "single",
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        scrollY = "250px",
        autoWidth = TRUE,
        buttons = c('csv', 'excel')
      ),
    )%>%
     formatRound(columns=c('sumsq', 'meansq', 'statistic', 'p.value', 'Adjusted_pval'), digits=4)
  }, server = F)
  
  output$posthoctest_table <- renderDataTable({
    datatable(
      Anova_posthoc(),
      class = "display",
      filter = "top",
      selection = "single",
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        scrollY = "250px",
        autoWidth = TRUE,
        buttons = c('csv', 'excel')
      ),
    )
    
  }, server = F)
   
  
  #### GSEA Table ####
  
  output$GSEA_table <- renderDataTable({
    datatable(
      GSEA_test()[[2]],
      class = "display",
      filter = "top",
      selection = "single",
      extensions = 'Buttons',
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'frtBip',
        buttons = c('csv', 'excel')
      )
    )
  }, server = F)
  
  #### Diagnostic marker plots #### 
  output$vplt1<- renderPlotly({volcanoplt1()})
  
  # output$mwutvplt1<- renderPlotly({mwuvolcanoplt1()})
  
  output$bplt1<- renderPlot({
    if (is.null(input$ttest_table_rows_selected) || length(input$ttest_table_rows_selected) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data selected.\nPlease select a row from the above table to generate box plot.")
    } else {
      bxplt1()
    }
  })
  
  output$mwutbplt1<- renderPlot({
    if (is.null(input$mwutest_table_rows_selected) || length(input$mwutest_table_rows_selected) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data selected.\nPlease select a row from the above table to generate box plot.")
    } else {
      mwutbxplt1()
    }
    
  })
  
  output$anplt1<- renderPlot({
    if (is.null(input$anovatest_table_rows_selected) || length(input$anovatest_table_rows_selected) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data selected.\nPlease select a row from the above table to generate box plot.")
    } else {
      anovabxplt1()
    }
  })
  
  #### heat Map Plot ####
  
  output$heatmap2 <-renderPlotly(heatmap1()[[1]])
  
  output$heatmap_ttest2 <-renderPlotly(heatmap_ttest()[[1]])
  
  output$heatmap_mwutest2 <-renderPlotly(heatmap_mwutest()[[1]])
  
  output$heatmap_anova <-renderPlotly(heatmap_Anova()[[1]])
  
  
  #### GSEA Plots ####
  
  output$dotplot_GSEA <- renderPlot({
#    P1<- dotplot( GSEA_test()[[1]], showCategory=15, split=".sign") + facet_grid(.~.sign)
    
    P1 <- enrichplot::dotplot( GSEA_test()[[1]], showCategory=15)
    reportProd$dotplot_GSEA1 <- P1
    P1
  })
  
  output$ridgeplot_GSEA <- renderPlot({
    
    
    P1<- ridgeplot( GSEA_test()[[1]], showCategory=15)+ labs(x = "enrichment distribution")
    
    reportProd$ridgeplot_GSEA1 <- P1
    P1
  })

  #### observe events #### 
  
  observeEvent(input$CV1, {
    updateSelectInput(
      session,
      inputId = "tt_cond1",
      label = "Case",
      choices = unique(final_data()[[2]][, grep(pattern = input$CV1, names(final_data()[[2]]))])
    )
  })
  
  observeEvent(input$CV1, {
    updateSelectInput(
      session,
      inputId = "tt_cond2",
      label = "Control",
      choices = unique(final_data()[[2]][, grep(pattern = input$CV1, names(final_data()[[2]]))])
    )
  })
  
  #### observe events muw test #### 
  
  observeEvent(input$mwutCV1, {
    updateSelectInput(
      session,
      inputId = "mwut_cond1",
      label = "Case",
      choices = unique(final_data()[[2]][, grep(pattern = input$mwutCV1, names(final_data()[[2]]))])
    )
  })
  
  observeEvent(input$mwutCV1, {
    updateSelectInput(
      session,
      inputId = "mwut_cond2",
      label = "Control",
      choices = unique(final_data()[[2]][, grep(pattern = input$mwutCV1, names(final_data()[[2]]))])
    )
  })
  
  observeEvent(input$CV1_G, {
    updateSelectInput(
      session,
      inputId = "gtt_cond1",
      label = "Condition 1",
      choices = unique(final_data()[[2]][, grep(pattern = input$CV1_G, names(final_data()[[2]]))])
    )
  })
  
  observeEvent(input$CV1_G, {
    updateSelectInput(
      session,
      inputId = "gtt_cond2",
      label = "Condition 2",
      choices = unique(final_data()[[2]][, grep(pattern = input$CV1_G, names(final_data()[[2]]))])
    )
  })
  
  output$dt <- DT::renderDT(data.frame(
    "symbol" = c("H", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"),
    "description" = c(
      "hallmark gene sets  are coherently expressed signatures derived by aggregating many MSigDB gene sets to represent well-defined biological states or processes.",
      "positional gene sets  for each human chromosome and cytogenetic band.",
      "curated gene sets  from online pathway databases, publications in PubMed, and knowledge of domain experts.",
      "regulatory target gene sets  based on gene target predictions for microRNA seed sequences and predicted transcription factor binding sites.",
      "computational gene sets  defined by mining large collections of cancer-oriented microarray data.",
      "ontology gene sets  consist of genes annotated by the same ontology term.",
      "oncogenic signature gene sets  defined directly from microarray gene expression data from cancer gene perturbations.",
      "immunologic signature gene sets  represent cell states and perturbations within the immune system.",
      "cell type signature gene sets  curated from cluster markers identified in single-cell sequencing studies of human tissue."
    )
  ))
  
  output$dt2 <- DT::renderDT(data.frame(
    PATHWAY_NAME = c(
      "1,25-dihydroxyvitamin D3 Biosynthesis",
      "1,25-dihydroxyvitamin D3 Biosynthesis",
      "14-3-3-mediated Signaling",
      "14-3-3-mediated Signaling"
    ),
    NODE_NAME = c("CYP2R1",
                  "POR",
                  "SFN",
                  "YWHAB")
  )
  )
  
  
  observeEvent(input$help1, {
    showModal(
      modalDialog(
        title = "Help info",
        DT::DTOutput("dt"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  observeEvent(input$help2, {
    showModal(
      modalDialog(
        title = "Help info",
        renderText("The uploaded geneset should follow the same format shown below"),
        DT::DTOutput("dt2"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  observeEvent(input$volploty,{
    if (input$volploty == "pvalue"){
      updateTextInput(
        inputId = "maxpval",
        label = "Lower limit for P value(-log10)",
        placeholder = "Values greater than entered values will be filtered"
      )} else{
        updateTextInput(
          inputId = "maxpval",
          label = "Lower limit for adj P value(-log10)",
          placeholder = "Values greater than entered values will be filtered"
        )}
  })  
  
  observeEvent(input$volploty,{
    if (input$volploty == "pvalue"){
      updateTextInput(
        inputId = "minpval",
        label = "Upper limit for P value(-log10)",
        placeholder = "Values greater than entered values will be filtered"
      )} else{
        updateTextInput(
          inputId = "minpval",
          label = "Upper limit for adj P value(-log10)",
          placeholder = "Values greater than entered values will be filtered"
        )}
  })
  
  observeEvent(input$ttest_reset, {
    shinyjs::reset("ADV")
  })   
  
  
  observeEvent(input$corplot1, {
    if (input$corplot1 %in% names(final_data()$metadata)) {
      disable("corplot3")
    } else{
      enable("corplot3")
    }
  })

  
  observe({
    print(input$run_norm1)
    if (is.null(input$run_norm1) || input$run_norm1<=0) {
      disable("run_filtered_data_download")
    } else{
      enable("run_filtered_data_download")
    }
  })
  
  values_barplot <- reactiveValues(selected_values_barplot = list())
  
  # Observe changes in the dropdown selection and save the values
  observeEvent(input$column_barplot, {
    selected_value <- input$column_barplot
    print("selected_value barplot")
    print(selected_value)
    if (!is.null(selected_value) && selected_value != "") {
      values_barplot$selected_values_barplot[[length(values_barplot$selected_values_barplot) + 1]] <- selected_value
    }
    print(values_barplot$selected_values_barplot)
  })
  
  values_pieChart <- reactiveValues(selected_values_pieChart = list())
  
  # Observe changes in the dropdown selection and save the values
  observeEvent(input$column_piechart, {
    selected_value <- input$column_piechart
    print("selected_value peichart")
    print(selected_value)
    if (!is.null(selected_value) && selected_value != "") {
      values_pieChart$selected_values_pieChart[[length(values_pieChart$selected_values_pieChart) + 1]] <- selected_value
    }
    print(values_pieChart$selected_values_pieChart)
  })

  # Define download handler for generating and downloading the report
  output$report <- downloadHandler(
    # Specify the filename and format for the downloaded report
    filename = "report.html", 
    content = function(file) {
      # Show a modal dialog indicating that the report is being downloaded
      showModal(modalDialog("Please Wait Downloading Report", footer=NULL))
      tempReport <- file.path(tempdir(), "report.Rmd") # Define the path to the temporary report file
      file.copy("report.Rmd", tempReport, overwrite = TRUE) # Copy the report template (Rmd file) to the temporary directory
      
      params <- reactiveValuesToList(reportProd) # Extract reactive values from reportProd reactiveValues object
      print(params)
      # Use tryCatch to handle any potential errors during report rendering
      tryCatch({
        # Knit the Rmd document to HTML, passing parameters and evaluating it in a clean environment
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }, error = function(e) {
        # Hide the modal dialog if an error occurs during report rendering
        removeModal()
        # Show a modal dialog indicating the error
        showModal(modalDialog("An error occurred. Please try again.", footer=NULL))
      })
      
      # Hide the modal dialog after the report is downloaded
      on.exit(removeModal())
    }
  )
  
  # Function to create an empty ggplot object
  createEmptyPlot <- function(title) {
    ggplot() +
      theme_void() +  
      ggtitle(title)
  }
  
  # Define download handler for downloading plots as a .zip file
  output$plotsDownloadButton <- downloadHandler(
    # Specify the filename for the downloaded .zip file
    filename = "myplots.zip", 
    content = function(file) {
      # Show a modal dialog indicating that the .zip file is being created and downloaded
      showModal(modalDialog("Please Wait Downloading zip file", footer=NULL))
      
      # Create a folder for storing the plots using a timestamp as part of the directory name
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      plot_dir <- paste0("plots/", timestamp)
      dir.create(plot_dir)
      # Use tryCatch to handle any potential errors during plot generation
      tryCatch({
        # Function to generate PNG plots from Plotly objects
        generatePngfromPlotly <- function(plotObject, filename) {
          if (is.null(plotObject)) {
            # Create an empty plot if the Plotly object is NULL
            empty_plot <- createEmptyPlot(paste("You have not selected", filename, "section"))
            ggsave(paste0(plot_dir, "/", filename, ".png"), empty_plot)
          } else {
            # Export the Plotly object to a PNG file
            export(plotObject, file = paste0(plot_dir, "/", filename, ".png"))
          }
          print(paste("Downloaded", filename))
        }
        
        # Function to generate PNG plots from ggplot objects
        generatePngfromGgplot <- function(plotObject, filename) {
          if (is.null(plotObject)) {
            # Create an empty plot if the ggplot object is NULL
            empty_plot <- createEmptyPlot(paste("You have not selected", filename, "section"))
            ggsave(paste0(plot_dir, "/", filename, ".png"), empty_plot)
          } else {
            # Save the ggplot object as a PNG file
            ggsave(paste0(plot_dir, "/", filename, ".png"), plotObject)
          }
          print(paste("Downloaded", filename))
        }
        
        # Call the appropriate plot generation function for each plot
        generatePngfromPlotly(reportProd$column_barplot1, "dataSummaryBarPlot")
        generatePngfromPlotly(reportProd$column_piechart1, "dataSummaryPiePlot")
        generatePngfromPlotly(reportProd$missingValuePlots1, "missingValuePlots1")
        generatePngfromPlotly(reportProd$missingValuePlots2, "missingValuePlots2")
        generatePngfromGgplot(reportProd$outlierDetection1, "outlierDetection1")
        generatePngfromPlotly(reportProd$dimRedPlots1, "dimRedPlots1")
        generatePngfromPlotly(reportProd$hclust_plot1, "hclust_plot1")
        generatePngfromPlotly(reportProd$pam_plot1, "pam_plot1")
        generatePngfromPlotly(reportProd$pca_plot1, "pca_plot1")
        generatePngfromPlotly(reportProd$scree_plot1, "scree_plot1")
        generatePngfromPlotly(reportProd$heatmap1_eda1, "heatmap1_eda1")
        generatePngfromPlotly(reportProd$box_plot1, "box_plot1")
        generatePngfromPlotly(reportProd$corel_plot1, "corel_plot1")
        generatePngfromPlotly(reportProd$corel_plot1, "corel_plot1")
        generatePngfromPlotly(reportProd$heatmap_ttest1, "heatmap_ttest1")
        generatePngfromPlotly(reportProd$volcanoplt_ttest1, "volcanoplt_ttest1")
        generatePngfromGgplot(reportProd$bxplt1_ttest1, "bxplt1_ttest1")
        generatePngfromPlotly(reportProd$heatmap_Anova1, "heatmap_Anova1")
        generatePngfromGgplot(reportProd$anovabxplt1_an, "anovabxplt1_an")
        generatePngfromPlotly(reportProd$heatmap_mwutest1, "heatmap_mwutest1")
        generatePngfromGgplot(reportProd$mwutbxplt1_mwut, "mwutbxplt1_mwut")
        generatePngfromPlotly(reportProd$tca_heatmap1, "tca_heatmap1")
        generatePngfromGgplot(reportProd$pls_cm_plot1, "pls_cm_plot1")
        generatePngfromGgplot(reportProd$spls_cm_plot1, "spls_cm_plot1")
        generatePngfromGgplot(reportProd$svm_cm_plot1, "svm_cm_plot1")
        generatePngfromGgplot(reportProd$rf_cm_plot1, "rf_cm_plot1")
        generatePngfromGgplot(reportProd$xgb_cm_plot1, "xgb_cm_plot1")
        # generatePngfromGgplot(reportProd$lmer_plot1, "lmer_plot1")
        # generatePngfromGgplot(reportProd$dot_plot1, "dot_plot1")
        # generatePngfromGgplot(reportProd$bw_plot1, "bw_plot1")
        # generatePngfromGgplot(reportProd$dotplot_GSEA1, "dotplot_GSEA1")
        # generatePngfromGgplot(reportProd$ridgeplot_GSEA1, "ridgeplot_GSEA1")
        
        # Create the .zip file containing all the PNG plots
        zip::zip(file, files = list.files(plot_dir, full.names = TRUE))
        # Remove the temporary directory containing the plots once zipped
        unlink(plot_dir, recursive = TRUE)
      }, error = function(e) {
        # Hide the modal dialog if an error occurs during plot generation
        removeModal()
        # Show a modal dialog indicating the error
        showModal(modalDialog("An error occurred. Please try again.", footer=NULL))
      })
      # Hide the modal dialog after the .zip file is downloaded
      on.exit(removeModal())
    }
  )
  
})