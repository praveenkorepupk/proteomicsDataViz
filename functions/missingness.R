#Function for missingness plots
missing_percentage_plot <- function(npx_data_short){
  
  # Finding missingness for samples and protein using gg_miss_var functiom
  p <- gg_miss_var(npx_data_short, show_pct = TRUE) + labs(x = 'patient_sample') #sampleIds will be columns and proteins will be rows
  p1 <- gg_miss_var(as.data.frame(t(npx_data_short)), show_pct = TRUE) + labs(x = 'protein') #proteinID will be columns and sample will be rows
  
  #Creating bins
  binsample <- cut(p[["data"]][["pct_miss"]], breaks = seq(0, 100, by = 5), include.lowest = FALSE)
  binprotein <- cut(p1[["data"]][["pct_miss"]], breaks = seq(0, 100, by = 5), include.lowest = FALSE)
 
  # Count the number of samples in each range
  count_data <- table(binsample)
  count_data <- as.data.table(count_data)
  count_data <- count_data[N > 0]
  names <- count_data$binsample
 
  # Count the number of protein in each range
  count_data1 <- table(binprotein)
  count_data1 <- as.data.table(count_data1)
  count_data1 <- count_data1[N > 0]
  names1 <- count_data1$binprotein
  
  # Find missing percentage of proteins for each sample ( Variable: samples)
  pie_chart <- plot_ly(labels = names, values = count_data$N, type = "pie") %>% layout(title = "Sample Counts by Missingness Percentage Range",height = 300,width = 500,titlefont = list(size = 12))
 
  # Find missing percentage samples for each protein. (Variable: proteins)
  pie_chart1 <- plot_ly(labels = names1, values = count_data1$N, type = "pie") %>% layout(title = "Protein Counts by Missingness Percentage Range",height = 300,width = 500,titlefont = list(size = 12))
 
  return(list(pie_chart,pie_chart1))
  
}

# Function for removing proteins with specific sample missingness
removeprotein <- function(npx,miss_num){
  
  # Convert long to wide format
  test <- npx %>% pivot_wider(id_cols = OlinkID, 
                              names_from = SampleID,
                              values_from = NPX,
                              values_fill = NA)
  npx_data_short <- as.data.frame(subset(test, select = -OlinkID))
  rownames(npx_data_short) <- test$OlinkID
  
  # Calculate missingness of each protein acrosss all the samples
  p1 <- gg_miss_var(as.data.frame(t(npx_data_short)), show_pct = TRUE) + labs(x = 'protein') #proteinID will be columns and sample will be rows
  
  # Input from UI
  pct1 <- as.numeric(miss_num)
  
  # Proteins to be removed
  remove_proteins <- p1$data$variable[p1$data$pct_miss > pct1]
  
  # Remove proteins from data
  npx <- npx[!npx$OlinkID %in% remove_proteins,]
  
  return(list(remove_proteins,npx))
}