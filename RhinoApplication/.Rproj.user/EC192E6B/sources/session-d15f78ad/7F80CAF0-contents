library(ReactomePA)
enrich_pathways <- function(gene_list, all_protein_coding_genes) {
  df <- all_protein_coding_genes %>%
    dplyr::filter(gene_symbol %in% gene_list) %>%
    dplyr::select(entrez_id)
  enriched_pathway <- enrichPathway(gene=df$entrez_id, pvalueCutoff = 0.05, readable=TRUE)
}

list_of_enriched_pathways <- list()
for (i in list_of_dpcs) {
  list_of_enriched_pathways[[i[[2]]]] <- enrich_pathways(i[[1]], genesMetaDataDf_data)
}

# tables of enriched pathways
list_of_enriched_pathways_tables <- list()
for (i in seq_along(list_of_enriched_pathways)) {
  name <- names(list_of_enriched_pathways)[i]
  data <- list_of_enriched_pathways[[i]]
  list_of_enriched_pathways_tables[[name]] <- data.frame(data) %>%
    dplyr::select(ID, Description, qvalue)
}

# plots of enriched pathways
library(enrichplot)
edo <- pairwise_termsim(list_of_enriched_pathways[[1]])
p1 <- emapplot(edo, showCategory = 30)
p1

list_of_enriched_pathways_plots <- list()
for (i in seq_along(list_of_enriched_pathways)) {
  name <- names(list_of_enriched_pathways)[i]
  data <- list_of_enriched_pathways[[i]]
  
  edo <- pairwise_termsim(data)
  p1 <- emapplot(edo, showCategory = 30)
  
  list_of_enriched_pathways_plots[[name]] <- p1
}

# Save tables
saveRDS(list_of_enriched_pathways_tables, "./rda/reactome_enrichment_tables.rda", compress = TRUE)

# Save plots
saveRDS(list_of_enriched_pathways_plots, "./rda/reactome_enrichment_plots.rda", compress = TRUE)


