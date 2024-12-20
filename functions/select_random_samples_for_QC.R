select_random_samples_for_QC <- function(olink_data){
  samples <- c(unique(olink_data$SampleID))
  #print(samples)
  random_selection <- sample(x=samples, size=100)
  randomly_selected_samples_for_QC <- olink_data %>% filter(SampleID %in% random_selection)
  return(randomly_selected_samples_for_QC)
}
