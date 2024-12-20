correlation_plot1 <- function(olink_data,Metadata,CP1,CP2,CP3,CP4,CP5) {
  
  olink_data <-olink_data
  # Read metadata (Make sure that IDs are not represented as scientific exponential values. eg. ID=4.8E+11 is not allowed. ID should be completely visible)
  # (Replicated samples are manually deleted from metadata file.)
  olink_data_metadata <- Metadata
  
  # Join metadata to NPX data
  # Make SampleID column name same for both dataframes.
  colnames(olink_data_metadata)[1] <- "SampleID"
  olink_data_metadata$SampleID <- as.character(olink_data_metadata$SampleID)
  # Perform Inner Join
  joined_olink_data <<- inner_join(olink_data, olink_data_metadata, by = "SampleID")
  

  xaxis_column<-CP1
  yaxis_column<-CP2
  protein1<-CP3
  protein2<-CP4
  color_feature <- CP5
  
  
  #updated lines for correlation plot1 start here
  `%!in%` <- Negate(`%in%`)
  assays<-joined_olink_data$Assay
  data<-c()
  if((xaxis_column %in% names(olink_data_metadata)) && (yaxis_column %!in% names(olink_data_metadata))) {
    Assay_data <- joined_olink_data %>% filter(Assay == assays[1])
    Assay_data1<-Assay_data[c(xaxis_column)]
    Assay_data2<-joined_olink_data %>% filter(Assay == protein2)
    data<-data.frame(protein1=Assay_data1,protein2=Assay_data2[c(yaxis_column)])
  } else if((xaxis_column %!in% names(olink_data_metadata)) && (yaxis_column %in% names(olink_data_metadata))) {
    Assay_data1<-joined_olink_data %>% filter(Assay == protein1)
    Assay_data <- joined_olink_data %>% filter(Assay == assays[1])
    Assay_data2<-Assay_data[c(yaxis_column)]
    data<-data.frame(protein1=Assay_data1[c(xaxis_column)],protein2=Assay_data2)
  }else if ((xaxis_column %in% names(olink_data_metadata)) && (yaxis_column %in% names(olink_data_metadata))) {
    Assay_data <- joined_olink_data %>% filter(Assay == assays[1])
    Assay_data1 <- Assay_data[c(xaxis_column)]
    Assay_data2 <- Assay_data[c(yaxis_column)]
    data<-data.frame(protein1=Assay_data1,protein2=Assay_data2)
  }else{
    Assay_data1<-joined_olink_data %>% filter(Assay == protein1)
    Assay_data2<-joined_olink_data %>% filter(Assay == protein2)
    data<-data.frame(protein1=Assay_data1[c(xaxis_column)],protein2=Assay_data2[c(yaxis_column)], color_feature2=Assay_data1[c(color_feature)])
    
  }
  
  
  coeff<-round(cor(data[[1]],data[[2]]),2)
  fit <- lm(data[[1]]~data[[2]],data=data)
  p<-summary(fit)
  pval<-round(p$coefficients[2,4],2)
  
  a <- list(
    text = paste0("R=",coeff," p=",pval),
    showarrow = FALSE,
    yshift= 160,
    font=list(size=15)
  )
  
  if(xaxis_column %in% names(Metadata)){
    protein1 <- xaxis_column
  }
  
  if(yaxis_column %in% names(Metadata)){
    protein2 <- yaxis_column
  }
  
  fig <- plot_ly(
    data, x = ~data[[1]], y = ~data[[2]], type="scatter", alpha=1, mode = 'markers', color = ~data[[3]],colors = "Accent")%>%
    layout(xaxis = list(title = protein1),
           yaxis = list(title = protein2),font=list(size=15),annotations = a,
           legend=list(title=list(text='color_feature2'))
    )
  
  fig
  
}
