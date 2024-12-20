t_test <- function(olink_data, olink_data_metadata, type, variable, pair_id){
  # Join metadata to NPX data
  # Make SampleID column name same for both dataframes.
  colnames(olink_data_metadata )[1] <- "SampleID"
  # Perform Inner Join
  joined_olink_data <<- inner_join(olink_data,olink_data_metadata, by="SampleID")
  joined_olink_data <<- data.frame(joined_olink_data)
  joined_olink_data[[variable]] <- as.factor(joined_olink_data[[variable]])
  # Use olink_ttest function from olinkAnalyze package to perform t test. Variable must have only 2 possible values/conditions
  if( type == "Paired"){
    ttest_result_paired <- olink_ttest(df = joined_olink_data, variable = variable,pair_id = pair_id)
    
    #Select only required columns from t test result
    required_columns <- c("Assay", "OlinkID", "UniProt", "Panel", "estimate", "p.value", "method","alternative", "Adjusted_pval")
    ttest_result_paired <- ttest_result_paired[,required_columns]
    ttest_result_paired <- rename(ttest_result_paired, LogFC = estimate)
    return(ttest_result_paired)
  }
  
  if(type == "Unpaired"){
    ttest_result_unpaired <- olink_ttest(df = joined_olink_data, variable = variable)
    
    required_columns <- c("Assay", "OlinkID", "UniProt", "Panel", "estimate", "p.value", "method","alternative", "Adjusted_pval")
    ttest_result_unpaired = ttest_result_unpaired[,required_columns]
    ttest_result_unpaired <- rename(ttest_result_unpaired, LogFC = estimate)
    return(ttest_result_unpaired)
  }
  
}