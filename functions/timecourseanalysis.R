## lmer 

run_lmer <- function(data, var, rand, cov = NULL){
  
  if(is.numeric(data[[var]])){
    data[[var]] = paste0("X",data[[var]])
    
  }
  
  if(is.null(cov)){  
  lmer_res = olink_lmer(df = data,variable = var,random = rand,outcome = "NPX")
  
  
  } else {
    
    lmer_res = olink_lmer(df=data, variable = var, random = rand, outcome="NPX", covariates = cov)
  }
  
  return(lmer_res)
  
  
  
}


### LMer plot 

lmer_plot <- function(data, var, rand, cov, olinkid_list, xvar){
  
  if(is.numeric(data[[var]])){
    data[[var]] = as.character(data[[var]])
    data[[var]] = as.factor(data[[var]])
    
  }
  
  
  
  if(is.null(cov)){
  p = olink_lmer_plot(df = data,variable = var,outcome = "NPX",random = rand,olinkid_list = olinkid_list, x_axis_variable = xvar)

  } else {
    
  p = olink_lmer_plot(df = data,variable = var,outcome = "NPX",random = rand,covariates = cov,olinkid_list = olinkid_list, x_axis_variable = xvar)
    
    
  }
  
  return(p)
}


## Lmer posthoc analysis 

run_lmer_ph <- function(data, var, rand, cov = NULL,olinkids,eff){
  
  if(is.numeric(data[[var]])){
    data[[var]] = paste0("X",data[[var]])
    
  }
  
  
  if(is.null(cov)){  
    lmer_ph_res = olink_lmer_posthoc(df = data,variable = var,random = rand,outcome = "NPX",olinkid_list = olinkids,effect = eff)
    
    
  } else {
    
    lmer_ph_res = olink_lmer_posthoc(df=data, variable = var, random = rand, outcome="NPX", covariates = cov,olinkid_list = olinkids,effect = eff)
  }
  
  return(lmer_ph_res)
  
  
  
}

## heatmap posthoc 

plot_tca_heatmap <- function(mat, ph,cont,varcol){
  ph = ph[complete.cases(ph),]
  t = grepl(pattern = ":",x = unique(ph$term),fixed = T)
  
  if(is.numeric(mat[[varcol]])){
    
    mat[[varcol]] = paste0("X", mat[[varcol]])
  }
  
  
  d = ph[ph$Threshold == "Significant",]
  sig_npx = mat[mat$OlinkID %in% d$OlinkID,]
  
  # select contrast
  contrast = d[d$contrast %in% cont,]
  c1 = str_split_fixed(string = cont,pattern = " - ", 2)[1]
  c2 = str_split_fixed(string = cont,pattern = " - ", 2)[2]
  # 
  subcont = sig_npx[sig_npx$OlinkID %in% contrast$OlinkID,]
  subcont_1 = subcont[grep(c1, subcont[[varcol]]),]
  subcont_2 = subcont[grep(c2, subcont[[varcol]]),]
  # 
  subcont1_1 = pivot_wider(data = subcont_1,id_cols = SampleID,names_from = OlinkID,values_from = NPX)
  subcont_2_1 = pivot_wider(data = subcont_2,id_cols = SampleID,names_from = OlinkID,values_from = NPX)
  # # 
  new_df = rbind(subcont1_1,subcont_2_1)
  ann = data.frame("SampleIDs" = new_df$SampleID)
  ann$var = mat[[varcol]][match(ann$SampleIDs, mat$SampleID)]
  rownames(ann) = ann$SampleIDs
  ann2 = as.data.frame(x = ann$var, rownames(ann))
  #
  new_df = as.data.frame(new_df)
  rownames(new_df) = new_df$SampleID
  new_df = new_df[,-1]
  colnames(ann2) = varcol
  
  # ## plot heatmap 
  new_df_t = t(new_df)
  colSide = as.factor(ann2[[varcol]])
  p = heatmaply(x = new_df_t ,xlab = "SampleID",ylab = "Protein",col_side_colors = colSide,dendrogram = F)
  
  return(p)
  

}

