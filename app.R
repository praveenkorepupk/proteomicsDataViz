#### Loading required packages ####
library(shiny) # Load shiny for creating interactive web applications in R
library(shinydashboard) # Load shinydashboard for creating interactive dashboards
library(dashboardthemes) # Load dashboardthemes for customizing dashboard themes
library(shinyBS) # Load shinyBS for additional shiny Bootstrap components
library(shinybusy) # Load shinybusy for showing loading/busy indicators
library(DT) # Load DT for interactive tables
library(umap) # Load umap for UMAP dimensionality reduction
library(Rtsne) # Load Rtsne for t-Distributed Stochastic Neighbor Embedding (t-SNE) dimensionality reduction
library(shinycssloaders) # Load shinycssloaders for CSS loading spinners
library(plotly) # Load plotly for interactive plots
library(dplyr) # Load dplyr for data manipulation
library(shinytoastr) # Load shinytoastr for displaying notifications/toasts in Shiny apps
library(shinyjs) # Load shinyjs for client-side JavaScript functions in Shiny apps
library(OlinkAnalyze) # Load OlinkAnalyze for analyzing Olink proteomics data
library(ggpubr) # Load ggpubr for easy ggplot2-based publication-ready plots 
library(ggplot2) # Load ggplot2 for data visualization 
library(reshape2) # Load reshape2 for data reshaping 
library(naniar) # Load naniar for missing data visualization
library(tidyr) # Load tidyr for data tidying
library(stringr) # Load stringr for string manipulation functions
library(gridExtra) # Load gridExtra for arranging multiple grid-based plots
library(grid) # Load grid for grid-based layout
library(purrr) # Load purrr for functional programming tools
library(clusterProfiler) # Load clusterProfiler for functional enrichment analysis
library(enrichplot) # Load enrichplot for visualization of functional enrichment results
library(DOSE) # Load DOSE for semantic similarity computation and enrichment analysis
library(msigdbr) # Load msigdbr for accessing Molecular Signatures Database (MSigDB)
library(heatmaply) # Load heatmaply for interactive heatmaps
library(checkmate) # Load checkmate for checking function inputs
library(rvcheck) # Load rvcheck for validating function inputs
library(readxl) # Load readxl for reading Excel files
library(corrplot) # Load corrplot for correlation matrix visualization
library(sass) # Load sass for CSS preprocessing
library(shinyWidgets) # Load shinyWidgets for additional Shiny widgets
library(scales) # Load scales for data transformation functions
library(ggridges) # Load ggridges for ridgeline plots
library(shinyFiles) # Load shinyFiles for file input/output in Shiny apps
library(cluster) # Load cluster for cluster analysis
library(NbClust) # Load NbClust for determining the optimal number of clusters
library(factoextra) # Load factoextra for visualizing multivariate analysis results
library(data.table) # Load data.table for efficient data manipulation
library(zoo) # Load zoo for handling irregular time series data
library(stats) # Load stats for statistical functions
library(tibble) # Load tibble for data frame creation
library(shinyalert) # Load shinyalert for displaying custom alerts in Shiny apps
library(ggdendro) # Load ggdendro for dendrogram visualizations with ggplot2
library(ggplotify) # Load ggplotify for ggplot2 to plotly conversion
library(pheatmap) # Load pheatmap for heatmap creation
library(tidyverse) # Load tidyverse for tidy data analysis
library(caret) # Load caret for machine learning model training and evaluation
library(mlbench) # Load mlbench for machine learning benchmark datasets
library(pROC) # Load pROC for ROC curve analysis
library(doParallel) # Load doParallel for parallel computing in R
library(lme4) # Load lme4 for linear mixed-effects modeling
library(arrow) # Load arrow for efficient reading and writing of Apache Arrow files
library(DBI) # Load DBI for database interface
library(odbc) # Load odbc for ODBC database connectivity
library(RODBC) # Load RODBC for ODBC database access
library(ggfortify) # Load ggfortify for fortify methods for ggplot2-based objects
library(htmlwidgets) # Load htmlwidgets for creating HTML widgets in Shiny apps
library(orca) # Load orca for exporting static images of plotly graphics
library(webshot) # Load webshot for capturing screenshots of web pages
library(kernlab) # Load kernlab for kernel-based machine learning algorithms
library(pls) # Load pls for partial least squares regression and principal component regression
library(MLmetrics) # Load MLmetrics for machine learning model evaluation metrics
library(randomForest) # Load randomForest for random forest modeling
library(xgboost) # Load xgboost for extreme gradient boosting
library(spls) # Load spls for sparse partial least squares regression


# Load UI modules
source("ui/ui.R") # Load UI layout definition from separate files
source("server/server.R") # Load server-side logic from separate files
source("functions/data_qcplots.R") # Load custom functions for Data QC plots
source("functions/missingness.R") # Load custom functions for plotting missing data percentages and removing missingness
source("functions/outliers.R") # Load custom functions for plotting outliers and removing outiers
source("functions/dimred_plots.R") # Load custom function for dimension reduction plots.
source("functions/anova_test.R") # Load custom functions for ANOVA tests
source("functions/construct_genelist.R") # Load custom functions for constructing gene lists
source("functions/correlation_plot1.R") # Load custom functions for creating correlation plots
source("functions/correlation_plot2.R") # Load custom functions for creating correlation plots
source("functions/extract_geneset.R") # Load custom functions for extracting gene sets
source("functions/gsea.R") # Load custom functions for gene set enrichment analysis (GSEA)
source("functions/normalize_meadianwise.R") # Load custom functions for median-wise normalization
source("functions/select_random_samples_for_QC.R") # Load custom functions for selecting random samples for quality control (QC)
source("functions/t_test.R") # Load custom functions for conducting t-tests
source("functions/EDA_plots.R") # Load custom functions for exploratory data analysis (EDA) plots
source("functions/timecourseanalysis.R") # Load custom functions for time course analysis
source("functions/ML_scripts.R") # Load custom functions for machine learning scripts


# Combine the UI and server components to create and launch the Shiny app
shinyApp(ui = ui, server = server)
