
correlation_plot2 <- function(olink_data,Metadata,CP1,CP2,CP3,CP4,Panel) {
  olink_data <- olink_data
  
  # (Replicated samples are manually deleted from metadata file.)
  olink_data_metadata <- Metadata
  # Join metadata to NPX data
  # Make SampleID column name same for both dataframes.
  colnames(olink_data_metadata)[1] <- "SampleID"
  
  # Perform Inner Join
  joined_olink_data <<- inner_join(olink_data,olink_data_metadata, by="SampleID")
  
  d<-data.frame(joined_olink_data$Panel,joined_olink_data$Assay)
  #Find common genes from multiple panels
  test<-dcast(d,data.table::rowid(joined_olink_data.Panel) ~ joined_olink_data.Panel, value.var = "joined_olink_data.Assay")
  vals <- lapply(test[,-1], unique)
  # new_vals<-data.frame(vals)
  common_genes<-Reduce(intersect,vals)
  #Filter common genes from multiple panels
  panel_data<- joined_olink_data %>% filter(!Assay %in% c(common_genes))
  
  
  
  #User input
  xaxis_column<-CP1
  yaxis_column<-CP2
  protein1<-CP3
  protein2<-CP4
  
  `%!in%` <- Negate(`%in%`)
  # #Take panel as user input
  # panel <-  Panel
  # panel <- "INFLAMMATION"
  
  protein_corr<-c()
  assays<-unique(panel_data$Assay)
  for (i in 1:length(assays)){
    if((xaxis_column %in% names(olink_data_metadata)) && (yaxis_column %!in% names(olink_data_metadata))) {
      Assay_data1 <- panel_data %>% filter(Assay == assays[1])
      Assay_data2 <- panel_data %>% filter(Assay == protein2)
      p1 <- Assay_data1[c(xaxis_column)]
      p2 <- Assay_data2[c(yaxis_column)]
      d1 <- panel_data %>% filter(Assay == assays[i])
      c1 <- cor(p1, d1[c("NPX")])
      d2 <- panel_data %>% filter(Assay == assays[i])
      c2 <- cor(p2, d2[c(yaxis_column)])
      protein1 = xaxis_column
      corrs <- data.frame(assays[i], c1, c2)
      colnames(corrs) = c("Assay", protein1, protein2)
      protein_corr <- bind_rows(corrs, protein_corr)
    } else if((xaxis_column %!in% names(olink_data_metadata)) && (yaxis_column %in% names(olink_data_metadata))) {
      Assay_data1 <- panel_data %>% filter(Assay == protein1)
      Assay_data2 <- panel_data %>% filter(Assay == assays[1])
      p1 <- Assay_data1[c(xaxis_column)]
      p2 <- Assay_data2[c(yaxis_column)]
      d1 <- panel_data %>% filter(Assay == assays[i])
      c1 <- cor(p1, d1[c(xaxis_column)])
      d2 <- panel_data %>% filter(Assay == assays[i])
      c2 <- cor(p2, d2[c("NPX")])
      protein2 = yaxis_column
      corrs <- data.frame(assays[i], c1, c2)
      colnames(corrs) = c("Assay", protein1, protein2)
      protein_corr <- bind_rows(corrs, protein_corr)
    } else if ((xaxis_column %in% names(olink_data_metadata)) && (yaxis_column %in% names(olink_data_metadata))) {
      Assay_data1 <- panel_data %>% filter(Assay == assays[1])
      Assay_data2 <- panel_data %>% filter(Assay == assays[1])
      p1 <- Assay_data1[c(xaxis_column)]
      p2 <- Assay_data2[c(yaxis_column)]
      d1 <- panel_data %>% filter(Assay == assays[i])
      c1 <- cor(p1, d1[c("NPX")])
      d2 <- panel_data %>% filter(Assay == assays[i])
      c2 <- cor(p2, d2[c("NPX")])
      protein1 = xaxis_column
      protein2 = yaxis_column
      corrs <- data.frame(assays[i], c1, c2)
      colnames(corrs) = c("Assay", protein1, protein2)
      protein_corr <- bind_rows(corrs, protein_corr)
    }else{
      Assay_data1<-panel_data%>% filter(Assay == protein1)
      Assay_data2<-panel_data%>% filter(Assay == protein2)
      p1<-Assay_data1[c(xaxis_column)]
      p2<-Assay_data2[c(yaxis_column)]
      d1<-panel_data%>% filter(Assay == assays[i])
      c1<-cor(p1,d1[c(xaxis_column)])
      d2<-panel_data%>% filter(Assay == assays[i])
      c2<-cor(p2,d2[c(yaxis_column)])
      corrs<-data.frame(assays[i],c1,c2)
      colnames(corrs)=c("Assay",protein1,protein2)
      protein_corr<-bind_rows(corrs,protein_corr)
    }
  }
  
  fig <<- plot_ly(
    protein_corr, x = ~protein_corr[[2]], y = ~protein_corr[[3]], type="scatter", alpha=1, hoverinfo = "text", text=protein_corr$Assay)%>%
    layout(xaxis = list(title = protein1),
           yaxis = list(title = protein2),font=list(size=15))
  
  fig
  
  
  
  
}
