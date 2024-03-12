box::use(
  DT[...],
  fst[read.fst]
)

#' @export
data_table <- function(table_data, headers, selected_columns) {
  localFileNameDownload <- 'local_file_download'
  datatable(
    table_data,
    class = 'cell-border stripe nowrap',
    fillContainer = TRUE,
    container = headers,
    rownames = FALSE,
    plugins = "ellipsis",
    #filter = 'top',
    selection = 'none',
    extensions = c("Buttons"),
    options = list(
      dom = "Bfrtip",
      # Unsure if this actually speeds up the tooltip
      initComplete = JS(
        "function(settings, json) {",
        "$('[data-toggle=\"tooltip\"]').tooltip({ delay: { show: 0, hide: 0 } });",
        "}"
      ),
      columnDefs = list(
        list(
          targets = "_all",
          render = JS("$.fn.dataTable.render.ellipsis(17, true)")
        )
        ,
        list(
          targets = selected_columns,
          visible = FALSE
        )
      ),
      buttons =  list(
        list(
          extend = 'collection',
          buttons = list(list(extend = "excel", text = "excel", filename = localFileNameDownload,
                              exportOptions = list(
                                modifier = list(page = "all", search = "applied"),
                                orthogonal = "export",
                                columns = ":visible"
                              )
          ),
          list(extend='csv',
               text = "csv",
               filename = localFileNameDownload,
               exportOptions = list(
                 modifier = list(page = "all", search = "applied"),
                 orthogonal = "export",
                 columns = ":visible"
               )
          ),
          list(extend='pdf',
               text = "pdf",
               filename= localFileNameDownload,
               exportOptions = list(
                 modifier = list(page = "all", search = "applied"),
                 orthogonal = "export",
                 columns = ":visible"
               )
          )
          ),
          text = 'Download current view'
        )),
      pageLength = 100,
      scrollX = TRUE
    )
  )
}


#' @export
table_headers <- function() {
  headers <- htmltools::withTags(table(
    class = 'display',
    thead(
      class = 'table_entire_header',
      tr(
        class = 'table_top_header',
        th(colspan = 1, 'DPCs studying Gene'),
        th(colspan = 9, 'Gene Identifiers'),
        th(colspan = 7, 'Mouse data (IMPC & MGI)'),
        th(colspan = 8, 'Disease Data: OMIM'),
        th(colspan = 4, 'Disease Data: DDGene2Phenotype'),
        th(colspan = 7, 'Constraint Metrics: Cell Line'),
        th(colspan = 18, 'Constraint Metrics: Sequencing Data'),
        th(colspan = 5, 'Panther DB'),
        th(colspan = 6, 'Gene Ontology'),
        th(colspan = 2, 'Reactome')
      ),
      tr(
        class = 'table_bottom_header',
        th('DPCs'),

        th('Gene Symbol', title = 'Symbol used to represent a gene'),
        th('Gene Name', title = 'Official name or description of the gene'),
        th('Alias Symbol', title = 'Alternate symbol(s) or name(s) for the gene'),
        th('HGNC ID', title = 'ID assigned by the HUGO Gene Nomenclature Committee'),
        th('UniProt ID', title = 'Unique identifier assigned by UniProt for the gene'),
        th('Entrez ID', title = 'Gene identifier in the NCBI Entrez Gene database'),
        th('Ensembl ID', title = 'Gene identifier in the Ensembl database'),
        th('OMIM Gene ID', title = 'Gene identifier in the Online Mendelian Inheritance in Man (OMIM) database'),
        th('MGI ID', title = 'Gene identifier in the Mouse Genome Informatics (MGI) database'),

        th('MGI Viability', title = 'Mouse Genome Informatics (MGI) Viability'),
        th('IMPC Viability', title = 'International Mouse Phenotyping Consortium (IMPC) Viability'),
        th('Phenotypes Homo', title = 'Abnormal phenotypes observed in the homozygote knockout (IMPC DR 20.0)'),
        th('Phenotypes Hetero', title = 'Abnormal phenotypes observed in the heterozygote knockout (IMPC DR 20.0)'),
        th('Phenotypes Hemiz', title = 'Abnormal phenotypes observed in the hemizygote knockout (IMPC DR 20.0)'),
        th('Ortholog relation', title = 'Relation to Orthologous Genes in Humans'),
        th('Ortholog mapping confidence', title = 'Confidence Level of Ortholog Mapping - refer to the Methods section for more information on how this metric was computed'),

        th('Phenotype ID', title = 'Phenotype identifier in the Online Mendelian Inheritance in Man (OMIM) database'),
        th('Phenotype', title = 'Online Mendelian Inheritance in Man (OMIM) phenotype'),
        th('Strict mendelian disease', title = 'Indicates if the Phenotype is a Strict Mendelian Disease - refer to the Methods section for more information on how this metric was computed'),
        th('Mode of inheritance', title = 'Inheritance Pattern of the Phenotype'),
        th('Gene Phenotype mapping', title = 'Mapping of Genes to Phenotypes - refer to the Methods section for more information on how this value was attained'),
        th('Phenotype flag', title = 'Flag indicating specific characteristics of the Phenotype - refer to the Methods section for more information on how this value was attained'),
        th('Gene lethality', title = 'Lethality Associated with the Gene - refer to the Methods section for more information on how this value was attained'),
        th('Earliest lethality Category', title = 'Category of Earliest Lethality - refer to the Methods section for more information on how this value was attained'),
        th('Phenotype', title = 'Description of the Phenotype (DDG2P)'),
        th('Confidence Category', title = 'Category indicating Confidence Level (DDG2P)'),
        th('Allelic Requirement', title = 'Number of copies (alleles) of a particular gene that are necessary for a specific phenotype to manifest (DDG2P)'),
        th('Organ Specificity List', title = 'List of Organs Specific to the Phenotype (DDG2P)'),

        th('DepMap Mean Gene Effect Score', title = 'Mean Gene Effect Score in DepMap'),
        th('DepMap percentage essential (< -0.5 GES)', title = 'Percentage of Essential Genes with Mean Gene Effect Score < -0.5 in DepMap - refer to the Methods section for more information on how this metric was computed'),
        th('DepMap threshold flag', title = 'Flag indicating Threshold in DepMap - refer to the Methods section for more information on how this value was attained'),
        th('MEF Bayes Factor', title = 'Bayes factor (BF) score, a confidence measure of whether a gene is required for fitness in a pooled CRISPR screen. Derived from loss-of-function screens in an inducible Cas9 H1 hPSC line cultured on MEF feeder cells'),
        th('MEF FDR', title = 'False Discovery Rate'),
        th('Laminin Bayes Factor', title = 'Bayes factor (BF) score, a confidence measure of whether a gene is required for fitness in a pooled CRISPR screen. Derived from loss-of-function screens in an inducible Cas9 H1 hPSC line cultured on laminin'),
        th('Laminin FDR', title = 'False Discovery Rate'),

        th('Transcript', title = 'Mane Gene Transcript Identifier provided by gnomAD v4'),
        th('Transcript type', title = 'Type of Gene Transcript - refer to the Methods section for more information on how this value was attained'),
        th('OE LOF', title = 'Observed over Expected Loss of Function Score'),
        th('OE LOF Lower', title = 'Lower Bound of Observed over Expected LoF Score'),
        th('OE LOF Upper', title = 'Upper Bound of Observed over Expected LoF Score'),
        th('OE Missense', title = 'Observed over Expected Missense Score'),
        th('OE Missense Lower', title = 'Lower Bound of Observed over Expected Missense Score'),
        th('OE Missense Upper', title = 'Upper Bound of Observed over Expected Missense Score'),
        th('constraint flag', title = 'Reason transcript does not have constraint metrics. One of:
	–no_variants: Zero observed synonymous, missense, pLoF variants
	–no_exp_lof: Zero expected pLoF variants
	–outlier_lof: Number of pLoF variants is significantly different than expectation
	–no_exp_mis: Zero expected missense variants
	–outlier_mis: Number of missense variants is significantly different than expectation
	–no_exp_syn: Zero expected synonymous variants
	–outlier_syn: Number of synonymous variants is significantly different than expectation'),
	th('Shet RGCME Mean', title = 'Mean Shet derived RGC-ME dataset'),
	th('Shet RGCME Lower', title = 'Lower Bound Shet derived RGC-ME dataset'),
	th('Shet RGCME Upper', title = 'Upper Bound Shet derived RGC-ME dataset'),
	th('Shet Posterior Mean', title = 'Mean of Shet Posterior Probability'),
	th('Shet Posterior Lower 95', title = 'Lower Bound of Shet Posterior Probability (95%)'),
	th('Shet Posterior Upper 95', title = 'Upper Bound of Shet Posterior Probability (95%)'),
	th('Constraint Metrics: DOMINO', title = 'DOMINO score - likelihood for a gene to harbor dominant changes'),
	th('Constraint Metrics: SCoNeS', title = 'SCoNeS score - likelihood for a gene to underlie an AD or AR disease (score > 0.75 gene predicted AR, score < 0.25 gene predicted AD)'),
	th('Mean AM Pathogenicity', title = 'Mean Pathogenicity Score according to the 2023 AlphaMissense model'),

	th('PANTHER Class ID', title = 'Identifier for PANTHER Class'),
	th('PANTHER Class Term', title = 'Term for PANTHER Class'),
	th('PANTHER Family ID', title = 'Identifier for PANTHER Family'),
	th('PANTHER Family Term', title = 'Term for PANTHER Family'),
	th('PANTHER Subfamily Term', title = 'Term for PANTHER Subfamily'),

	th('GO IDs: Biological Process', title = 'Gene Ontology IDs for Biological Process'),
	th('GO Terms: Biological Process', title = 'Gene Ontology Terms for Biological Process'),
	th('GO IDs: Molecular Function', title = 'Gene Ontology IDs for Molecular Function'),
	th('GO Terms: Molecular Function', title = 'Gene Ontology Terms for Molecular Function'),
	th('GO IDs: Cellular Component', title = 'Gene Ontology IDs for Cellular Component'),
	th('GO Terms: Cellular Component', title = 'Gene Ontology Terms for Cellular Component'),

	th('Reactome Pathway ID', title = 'Identifier for Reactome Pathway'),
	th('Reactome Pathway', title = 'Reactome Pathway')
      )
    )
  )
  )
}




