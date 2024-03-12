box::use(
  fst[read.fst]
)

#' @export
dpc_gene_list_data <- function() {
  #list_of_dpcs <- readRDS("../../../rda/zipped_dpc_names_genes.rda")
  list_of_dpcs <- readRDS("./rda/dpc_gene_lists.rda")
}

#' @export
morphic_gene_list_data <- function() {
  morphic_gene_list_data <- readRDS("./rda/morphic_gene_list_data.rda")
}

#' @export
meta_data_table_data <- function() {
  #genesMetaDataDf_data <- readRDS('./rda/all_genes_with_anot_jan16.rda')
  #write.fst(genesMetaDataDf_data, './rda/all_genes_with_anot_jan16.fst')
  genesMetaDataDf_data <- read.fst('./rda/all_genes_with_anot_jan16.fst')
}

#' @export
visuals_data <- function() {
  #main.annotated.data.frame <- readRDS('./rda/main.annotated.data.frame.rda')
  #write.fst(main.annotated.data.frame, './rda/main.annotated.data.frame.fst')
  main.annotated.data.frame <- read.fst('./rda/main.annotated.data.frame.fst')
}

#' @export
constraint_metrics <- function() {
  #gene.constraint.metrics.num.only <- readRDS('./rda/gene.constraint.metrics.num.only.rda')
  #write.fst(gene.constraint.metrics.num.only, './rda/gene.constraint.metrics.num.only.fst')
  gene.constraint.metrics.num.only <- read.fst('./rda/gene.constraint.metrics.num.only.fst')
}

#' @export
go_scatter_plots <- function() {
  semantic_analysis_plots_bp <- readRDS('./rda/semantic_analysis_plots_bp.rda')
  semantic_analysis_plots_mf <- readRDS('./rda/semantic_analysis_plots_mf.rda')
  semantic_analysis_plots_cc <- readRDS('./rda/semantic_analysis_plots_cc.rda')

  list(
    "BP" = semantic_analysis_plots_bp,
    "MF" = semantic_analysis_plots_mf,
    "CC" = semantic_analysis_plots_cc
  )
}

#' @export
go_top_enriched_terms_tables <- function() {
  semantic_analysis_tables_bp <- readRDS('./rda/semantic_analysis_tables_bp.rda')
  semantic_analysis_tables_mf <- readRDS('./rda/semantic_analysis_tables_mf.rda')
  semantic_analysis_tables_cc <- readRDS('./rda/semantic_analysis_tables_cc.rda')

  list(
    "BP" = semantic_analysis_tables_bp,
    "MF" = semantic_analysis_tables_mf,
    "CC" = semantic_analysis_tables_cc
  )
}

#' @export
reactome_enrichment_tables <- function() {
  reactome_enrichment_tables <- readRDS("./rda/reactome_enrichment_tables.rda")
}

#' @export
reactome_enrichment_plots <- function() {
  reactome_enrichment_plots <- readRDS("./rda/reactome_enrichment_plots.rda")
}


