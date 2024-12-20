#### Scatter QC plots ####
# Function to generate quality control plot for a specific panel
scatter_plot <- function(olink_data, panel_name) {
  # Filter data for the specified panel
  filtered_data <- olink_data %>% filter(Panel == panel_name)
  
  # Generate scatter plot
  plot <- filtered_data %>% olink_qc_plot(color_g = "QC_Warning",label_outliers=TRUE) +
    # Set plot title
    labs(title = "Sample IQR vs Sample Median") +
    # Customize plot title appearance
    theme(plot.title = element_text(size = 13, color = "black", hjust = 0.5))
  
  # Get failed samples
  df <- olink_qc_plot(filtered_data ,color_g = "QC_Warning",label_outliers=TRUE)
  df <- df$data[df$data$QC_Warning == "Warning", ]
  failed_samples <- unique(paste(df$Panel,df$SampleID,sep='-'))
  
  # Return the plot
  return(list(plot,failed_samples))
}

#### Frequency distributiom QC plots ####
frq_plot <- function(olink_data, panel_name){
  # Filter data for the specified panel
  filtered_data <- olink_data %>% filter(Panel == panel_name)
  
  # Generate frequency distribution plot
  plot <- filtered_data %>% olink_dist_plot() +
    # Set plot title
    labs(title = "NPX Distribution per Sample           ") +
    # Customize plot title appearance
    theme(plot.title = element_text(size = 13, color = "black", hjust = 0.5),axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Return the plot
  return(plot)
}
 
#### Outlier QC plots ####
generate_plots <- function(pca_data) {
  # Create PCA plot
  pca <- olink_pca_plot(df = pca_data, color_g = "QC_Warning", byPanel = TRUE, outlierDefX = 2.5, outlierDefY = 4)
  
  # Return PCA plot
  return(pca)
}

