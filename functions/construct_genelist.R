construct_genelist <- function(t_test_result){
  
  
  
  # Extract Genes and LogFC from T Test result
  required_columns <- c("Assay","LogFC")
  genes_with_logFC_value = as.data.frame(t_test_result[,required_columns])
  
  # Detect duplicates and take average for LOGFC
  genes_with_logFC_value_duplicates_removed = aggregate(genes_with_logFC_value$LogFC,by=list(genes_with_logFC_value$Assay),FUN=mean, na.rm=TRUE, data = genes_with_logFC_value)
  colnames(genes_with_logFC_value_duplicates_removed) <- c("Genes", "LogFC")
  
  # Create Gene list
  genelist <- genes_with_logFC_value_duplicates_removed[,2]
  names(genelist) = as.character(genes_with_logFC_value_duplicates_removed[,1])
  genelist = sort(genelist, decreasing = TRUE)
  #print(genelist)
  return(genelist)
  
}

## For timecourse/Lmer
