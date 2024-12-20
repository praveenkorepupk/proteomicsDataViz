gsea <- function(genelist, gene_set, p_adjust_cutoff) {
  # Perform GSEA using universal enrichment analysis
  GSEA_Result <-
    GSEA(
      geneList = genelist,
      TERM2GENE = gene_set,
      pvalueCutoff = p_adjust_cutoff,
      minGSSize = 10,
      maxGSSize = 500
    )
  return(GSEA_Result)
}