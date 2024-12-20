normalize_meadianwise<-function(olink_data){
  panels<-unique(olink_data$Panel)
  final_df2<-c()
  for (i in 1:length(panels)){
    npx<- olink_data %>% filter(Panel == panels[i])
    median<-median(npx$NPX)
    sample_medians<-npx %>%
      group_by(SampleID)%>% 
      summarise(Median=median(NPX), Std=sd(NPX))
    for(i in 1:dim(sample_medians)[1]){
      npx1<- npx %>% filter(SampleID == sample_medians$SampleID[i])
      # substract sample median from  sample NPX value 
      df3 <- mapply('-',data.frame(npx1$NPX),data.frame(sample_medians$Median[i]))
      colnames(df3)<-c("normalized_NPX")
      column_header<-npx1  
      df<-cbind.data.frame(column_header,df3)
      final_df2<-bind_rows(df,final_df2)
    }
  }
  return(final_df2)
}
