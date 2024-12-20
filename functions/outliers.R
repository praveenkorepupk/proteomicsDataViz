# Function for IQR outlier detection technique
iqr <- function(long_data,metadata){
  
  # Calculate the IQR for each protein
  iqr_values <- long_data %>% group_by(Protein) %>% summarize(IQR = IQR(NPX, na.rm = TRUE))
  
  # Define a threshold for identifying outliers
  threshold <- 1.5
  
  # Identify outliers based on specified threshold
  outliers <- long_data %>% left_join(iqr_values, by = "Protein") %>% mutate(Outlier = NPX > (quantile(NPX,0.75,na.rm = TRUE) + threshold * IQR) | NPX < (quantile(NPX,0.25,na.rm = TRUE) - threshold * IQR))
  
  # Combining outlier samples with normal samples
  data_with_outliers <- long_data %>% left_join(outliers, by = "SampleID") %>% filter(Protein.x == Protein.y & NPX.x == NPX.y) %>% select(-Protein.y, -NPX.y)
  colnames(data_with_outliers)[colnames(data_with_outliers) == "Protein.x"] <- "Protein"
  colnames(data_with_outliers)[colnames(data_with_outliers) == "NPX.x"] <- "NPX"
  
  # Samples as outliers
  remove_sample <- unique(data_with_outliers[data_with_outliers$Outlier == TRUE,"SampleID"])
  
  # Adding metadata for plots
  data_with_outliers <- merge(data_with_outliers, metadata, by = "SampleID", all.x = TRUE)
  
  # Plotting
  p <- ggplot(data_with_outliers, aes(x = Protein, y = NPX, color = Outlier, text = apply(data_with_outliers[, c(1, 6:ncol(data_with_outliers))], 1, function(row) paste(names(data_with_outliers)[c(1, 6:ncol(data_with_outliers))], ": ", row, collapse = "<br>")))) +
    geom_point() +
    theme(axis.text.x = element_blank()) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "green")) +
    ggtitle("Outlier using IQR")
  
  return(list(p,remove_sample))
}

# Function for PCA outlier detection technique
pca <- function(long_data,metadata){
  
  # Remove rows with missing values
  long_data <- na.omit(long_data)
  
  # Standardize the data
  standardized_data <- scale(long_data$NPX)
  
  # Compute PCA
  pca <- prcomp(standardized_data)
  
  # Extract the scores of the principal components
  scores <- pca$x
  
  # Identify outliers based on the scores
  outliers <- which(apply(scores, 1, function(x) any(abs(x) > 3)))
  
  # Adding outier label
  long_data$outlier <- ifelse(row.names(long_data) %in% outliers, "Outlier", "Normal")
  
  # Adding metadata for plots
  long_data <- merge(long_data, metadata, by = "SampleID", all.x = TRUE)
  
  # Samples as outliers
  remove_sample <- long_data %>% filter(outlier == "Outlier") %>% distinct(SampleID)
  
  # Plot the data, highlighting outliers
  p <- ggplot(long_data, aes(x = Protein, y = NPX, color = outlier, text = apply(long_data[, c(1, 5:ncol(long_data))], 1, function(row) paste(names(long_data)[c(1,5:ncol(long_data))], ": ", row, collapse = "<br>")))) +
    geom_point() +
    theme(axis.text.x = element_blank()) +
    scale_color_manual(values = c("Normal" = "green", "Outlier" = "red")) +
    ggtitle("Outlier using PCA")
  
  return(list(p,remove_sample))
  
}

# Function for Cooks outlier detection technique
cooks <- function(long_data,metadata){
  
  # Remove rows with missing values
  long_data <- na.omit(long_data)
  
  # Standardize the data
  standardized_data <- scale(long_data$NPX)
  
  # Compute Cook's distance
  influence <- influence.measures(lm(standardized_data ~ Protein, data = long_data))
  cooks_distance <- influence$cooks.distance
  
  # Identify outliers based on Cook's distance
  outliers <- which(cooks_distance > 4 / nrow(long_data))
  
  # Create a new column in long_data to indicate outliers
  long_data$outlier <- ifelse(row.names(long_data) %in% outliers, "Outlier", "Normal")
  
  # Adding metadata for plots
  long_data <- merge(long_data, metadata, by = "SampleID", all.x = TRUE)
  
  # Samples as outliers
  remove_sample <- long_data %>% filter(outlier == "Outlier") %>% distinct(SampleID)

  # Plot the data, highlighting outliers
  p <- ggplot(long_data, aes(x = Protein, y = NPX, color = outlier, text = apply(long_data[, c(1, 5:ncol(long_data))], 1, function(row) paste(names(long_data)[c(1,5:ncol(long_data))], ": ", row, collapse = "<br>")))) +
    geom_point() +
    theme(axis.text.x = element_blank()) +
    scale_color_manual(values = c("Normal" = "green", "Outlier" = "red")) +
    ggtitle("Outlier using Cook's Distance")
  
  return(list(p,remove_sample))
}
