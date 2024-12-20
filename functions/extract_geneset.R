
extract_geneset <- function(species, categories) {
  #print(categories)
  
  datalist = list()
  i = 1
  
  for (category in categories) {
    mSigDB_genesets_with_description <-
      msigdbr(species = species, category = category)
    mSigDB_geneset <-
      mSigDB_genesets_with_description %>% select(gs_name, gene_symbol)
    datalist[[i]] <- mSigDB_geneset
    i = i + 1
  }
  
  combined_genesets = do.call(rbind, datalist)
  return(combined_genesets)
}