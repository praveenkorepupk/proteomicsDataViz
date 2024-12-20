## Heirarchical clustering plot

plot_hclust<- function(data,distance, agg){
  data = data[complete.cases(data),]
  
  d_mat = dist(x = data, method = distance)
  hc_mat = hclust(d = d_mat, method = agg)
  d = as.dendrogram(hc_mat)
  par(cex = 0.7)
  plt = ggdendro::ggdendrogram(data = d)
  plt = ggplotly(plt)
  return(plt)
}

## PAM clustering 

plot_pam <- function(data, method){
  
  data = data[complete.cases(data),]
  
  scaleddata = scale(data)

  # find optimal no. of clusters with silhouette analysis 
  nclust = NbClust(data = scaleddata,method = "kmeans",index = "silhouette")
  bestnc = data.frame(nclust$Best.nc)
  numclust = bestnc[1,]
  
  # perform PAM clsutering using the optimal number of clusters 
  pamResult <-pam(scaleddata, k = numclust, metric = method)

  col = list("hotpink","deepskyblue2","maroon", "orange2" , "purple1" ,"seagreen","midnightblue", "red1" )
  pal = sample(col, numclust)
  
  p = fviz_cluster(pamResult,
               palette = pal, ellipse.type = "convex",
               ggtheme =theme_minimal(),ellipse = TRUE)
  p = ggplotly(p)
  return(p)
  
  
}


### PCA plot 

## biplot and screeplot 

plot_pca2 <- function(data, metadata,shape,color,pc1, pc2){
  
  data = t(data)
  print(dim(data))
  data = na.omit(data)
  print(dim(data))
  data = as.data.frame(t(data))
  data = data[metadata$SampleID,]
  print(dim(data))
  
  metadata = metadata[metadata$SampleID %in% rownames(data),]
  all(rownames(data) == metadata$SampleID)
  
  pca1 = prcomp(data, scale=TRUE)
  mat = cbind(data, metadata)
  mat[[shape]] = factor(mat[[shape]])
  mat[[color]] = factor(mat[[color]])
  
  plt1 = autoplot(object = pca1,data=mat, colour=color,shape=shape, x=pc1, y=pc2)
  plt1 = ggplotly(plt1)
  
  plt2 = fviz_screeplot(X = pca1, choice = "variance")
  plt2 = ggplotly(plt2)
  return(list(plt1, plt2))
}

## Box plot 


plot_boxplt <- function(data, metadata, protein, group){
  
  data = data[complete.cases(data),]
  
  mat2 = pivot_wider(data = data, names_from = UniProt,id_cols = SampleID,values_from = NPX,values_fn = mean)
  mat2 = as.data.frame(mat2)
  rownames(mat2) = mat2$SampleID
  mat2 = mat2[,-1]
  
  
  all(rownames(mat2) == metadata$SampleID)
  mat3 = mat2[metadata$SampleID,]
  print(dim(mat3))
  mat3 = cbind(mat3, metadata)
  print("cbind with metadata")
  print(head(mat3))
  print(colnames(mat3)[1:5])

  mat3[[group]] = as.factor(mat3[[group]])

  plt =   ggplot(mat3, aes_string(x = group, y = protein, fill = group)) + geom_boxplot() + ylab(paste0(protein," ","NPX")) + xlab(group) + theme_bw()
  plt = ggplotly(plt)
  return(plt)
}


## Heat map 
plot_heatmap2 <- function(data, meta2, group,clust){
  
  rownames(meta2) = meta2$SampleID
  data = data[rownames(meta2),]
  if (any(is.na(data))){
    datat = t(data)
    datat = as.data.frame(datat)
    datat = na.omit(datat)
    
  } else {
    
    datat = t(data)
  }
  
  colSide = as.factor(meta[["Timepoint"]])
  heatmaply(x = datat ,xlab = "SampleID",ylab = "Protein",col_side_colors = colSide,dendrogram = "none",na.rm=TRUE)
  
  
}

