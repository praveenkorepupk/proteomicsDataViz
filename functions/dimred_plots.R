#Function of dimension reduction using PCA
drpca <- function(npx_data,metadata,color,shape){
  
  #PCA calculation
  pca_result <- prcomp(npx_data)
  
  # Create a dataframe with PCA dimensions and metadata
  pca_data <- data.frame(
    PCA1 = pca_result$x[, 1],
    PCA2 = pca_result$x[, 2],
    Color = metadata[[color]],
    Shape = metadata[[shape]]
  )
  pca_data$Shape <- as.factor(pca_data$Shape)
  
  # Plot PCA
  pca_plot <- ggplot(pca_data, aes(x = PCA1, y = PCA2, color = Color, shape = Shape)) +
    geom_point() +
    ggtitle("PCA")
  
  # Convert ggplot object to plotly object
  pca_plot <- ggplotly(pca_plot)
  
  return(pca_plot)
}

#Function of dimension reduction using t-SNE
drtsne <- function(npx_data,metadata,color,shape,p){
  
  # Perform t-SNE
  tsne_result <- Rtsne::Rtsne(as.matrix(npx_data), perplexity = p)
  
  # Create a dataframe with t-SNE dimensions and metadata
  tsne_data <- data.frame(
    tSNE1 = tsne_result$Y[, 1],
    tSNE2 = tsne_result$Y[, 2],
    Color = metadata[[color]],
    Shape = metadata[[shape]]
  )
  tsne_data$Shape <- as.factor(tsne_data$Shape)
  
  # Plot t-SNE
  tsne_plot <- ggplot(tsne_data, aes(x = tSNE1, y = tSNE2, color = Color, shape = Shape)) +
    geom_point() + ggtitle("t-SNE")
  
  # Convert ggplot object to plotly object
  tsne_plot <- ggplotly(tsne_plot)
  
  return(tsne_plot)
}

#Function of dimension reduction using UMAP
drumap <- function(npx_data,metadata,color,shape,n){
  
  # Perform UMAP
  umap_result <- umap(npx_data,n_neighbors = n)
  
  # Create a dataframe with UMAP dimensions and metadata
  umap_data <- data.frame(
    UMAP1 = umap_result$layout[, 1],
    UMAP2 = umap_result$layout[, 2],
    Color = metadata[[color]],
    Shape = metadata[[shape]]
  )
  umap_data$Shape <- as.factor(umap_data$Shape)
  
  # Plot UMAP
  umap_plot <- ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = Color, shape = Shape)) +
    geom_point() +
    ggtitle("UMAP")
  
  # Convert to plotly object
  umap_plot <- ggplotly(umap_plot)
  
  return(umap_plot)
}