anova_test <- function(olink_data_test, olink_data_metadata, variable1, variable2, covariate){
  # Join metadata to NPX data
  # Make SampleID column name same for both dataframes.
  colnames(olink_data_metadata )[1] <- "SampleID"
  # Perform Inner Join
  joined_olink_data <<- inner_join(olink_data_test,olink_data_metadata, by="SampleID")
  joined_olink_data <<- data.frame(joined_olink_data)
  if(variable2 == "" && covariate == ""){
    # anova analysis with comparison variable 
    joined_olink_data[[variable1]] <- as.factor(joined_olink_data[[variable1]])
    anova_result <- olink_anova(df = joined_olink_data, variable = variable1, outcome = "NPX")
    if (input$posthoc1 == "Yes") {
      anova_post_hoc <-
        olink_anova_posthoc(df = joined_olink_data,
                            effect = variable1,
                            variable = variable1)
    } else {
      anova_post_hoc <- "Not Selected"
      print("Post hoc is not selected")
    }
    
    
  } else if(variable2 == "" && covariate != ""){
    # anova analysis with comparison variable 
    joined_olink_data[[variable1]] <- as.factor(joined_olink_data[[variable1]])
    joined_olink_data[[covariate]] <- as.factor(joined_olink_data[[covariate]])
    anova_result <- olink_anova(df = joined_olink_data, variable = variable1, outcome = "NPX",covariates=covariate)
    # anova post-hoc analysis 
    if (input$posthoc1 == "Yes") {
      anova_post_hoc <- olink_anova_posthoc(df = joined_olink_data,effect = variable1,variable = variable1, covariates = covariate)
    } else {
      anova_post_hoc <- "Not Selected"
      print("Post hoc is not selected")
    }
    
    
    
  } else if(variable2 != "" && covariate != ""){
    joined_olink_data[[variable1]] <- as.factor(joined_olink_data[[variable1]])  
    joined_olink_data[[variable2]] <- as.factor(joined_olink_data[[variable2]])
    joined_olink_data[[covariate]] <- as.factor(joined_olink_data[[covariate]])
    anova_result <- olink_anova(df = joined_olink_data, variable = paste0(variable1,":",variable2), outcome = "NPX",covariates = covariate)
    # anova post-hoc analysis 
    if (input$posthoc1 == "Yes") {
      anova_post_hoc <- olink_anova_posthoc(df = joined_olink_data,effect = paste0(variable1,":",variable2),variable = paste0(variable1,":",variable2),covariates=covariate)
    } else {
      anova_post_hoc <- "Not Selected"
      print("Post hoc is not selected")
    }
    
    
  } else {
    joined_olink_data[[variable1]] <- as.factor(joined_olink_data[[variable1]])  
    joined_olink_data[[variable2]] <- as.factor(joined_olink_data[[variable2]])
    anova_result <- olink_anova(df = joined_olink_data, variable = paste0(variable1,":",variable2), outcome = "NPX")
    # anova post-hoc analysis 
    if (input$posthoc1 == "Yes") {
      anova_post_hoc <- olink_anova_posthoc(df = joined_olink_data,effect = paste0(variable1,":",variable2),variable = paste0(variable1,":",variable2))
    } else {
      anova_post_hoc <- "Not Selected"
      print("Post hoc is not selected")
    }
  }
  
  return(list(anova_result,anova_post_hoc))
}
