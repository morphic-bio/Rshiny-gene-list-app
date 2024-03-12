## Imports
# Packages
box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  htmltools[...],
  fst[read.fst]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1/2,
    card(
      full_screen = TRUE,
      card_header(
        "Mouse Models Data"
      ),
      card_body(
        htmltools::h4("Ortholog mapping and confidence scores"),
        htmltools::p("Ortholog mapping was conducted by following the information presented in the Orthologue mapping sub-section from the methods section of Cacheiro et al. 2020, found here: https://www.nature.com/articles/s41467-020-14284-2#Sec10")
      ),
      card_footer(
        htmltools::span("IMPC ",
                        htmltools::a(href = "https://www.mousephenotype.org/",
                                     "website")
        ),
        tags$br(),
        htmltools::a(href = "http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-20.0/results/viability.csv.gz",
                     "IMPC viability/lethality data"),
        htmltools::a(href = "http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-20.0/results/genotype-phenotype-assertions-ALL.csv.gz",
                     "IMPC phenotypes data"),
        tags$br(),
        htmltools::span("MGI ",
                        htmltools::a(href = "https://www.informatics.jax.org/",
                                     "website")
        ),
        tags$br(),
        htmltools::a(href = " https://www.informatics.jax.org/downloads/reports/MRK_List2.rpt",
                     "MGI viability data")
      )
    ),

    card(
      full_screen = TRUE,
      card_header(
        "Disease Data"
      ),
      card_body(
        htmltools::h5("Gene-Phenotype mapping and Phenotype flags"),
        htmltools::p("These values relate to the symbols and numbers used by OMIM. Their definitions are found here: https://www.omim.org/help/faq#1_6 and are as follows:"),
        tags$p('Brackets, "[ ]", indicate "nondiseases," mainly genetic variations that lead to apparently abnormal laboratory test values (e.g., dysalbuminemic euthyroidal hyperthyroxinemia).

Braces, "{ }", indicate mutations that contribute to susceptibility to multifactorial disorders (e.g., diabetes, asthma) or to susceptibility to infection (e.g., malaria).

A question mark, "?", before the phenotype name indicates that the relationship between the phenotype and gene is provisional. More details about this relationship are provided in the comment field of the map and in the gene and phenotype OMIM entries.

The number in parentheses after the name of each disorder indicates the following: (1) the disorder was positioned by mapping of the wildtype gene; (2) the disease phenotype itself was mapped; (3) the molecular basis of the disorder is known; (4) the disorder is a chromosome deletion or duplication syndrome. Move the cursor over the number to display this information.'
        ),
htmltools::h5("Gene lethality and Earliest lethality category"),
htmltools::p("These values were manually curated from OMIM. They represent OMIM reports of lethality for disorders with molecular basis known and the Earliest age of death reported according to OMIM clinical records, respectivley."),
      ),
card_footer(
  htmltools::span("OMIM ",
                  htmltools::a(href = "https://www.omim.org/",
                               "website")
  ),
  tags$br(),
  htmltools::span("Files used: ",
                  htmltools::a(href = "https://www.omim.org/downloads", "mimTitles.txt"),
                  ", ",
                  htmltools::a(href = "https://www.omim.org/downloads", "genemap2.txt"),
                  ", ",
                  htmltools::a(href = "https://www.omim.org/downloads", "morbidmap.txt")
  ),
  tags$br(),
  htmltools::span("Gene2Phenotype ",
                  htmltools::a(href = "https://www.ebi.ac.uk/gene2phenotype",
                               "website")
  ),
  tags$br(),
  htmltools::p("Files used: ",
               htmltools::a(href = "https://www.ebi.ac.uk/gene2phenotype/downloads", "DDG2P.csv.gz")
  ),
)
    ),

card(
  full_screen = TRUE,
  card_header(
    "Constraint metrics - Human Cell Lines"
  ),
  card_body(
    htmltools::h5("DepMap (Cancer Dependency Map project) thesholds"),
    htmltools::p("For gene effect, a score less than -0.5 represents depletion in most cell lines, while less than -1 represents strong killing, outlined here: https://forum.depmap.org/t/depmap-genetic-dependencies-faq/131"),
    tags$a('The gene effect score otherwise referred to as the corrected CERES scores, measures the effect size of knocking out a gene, normalized against the distributions of non-essential and pan-essential genes.'),
    htmltools::h5("DepMap percentage essential"),
    tags$p('This was computed by calculating the number of different cancer cell lines depleted (using the -0.5 gene effect threshold) amongst all 1100 cancer cell lines.'),
    htmltools::h5("MEF and Laminin Bayes Factor and FDR"),
    tags$p('MEF and Laminin refer to the Mouse Embryonic Feeder cells and Laminin substrate used to grow H1-iCas9 human pluripotent stem cells (hPSCs). Bayes Factor (BF) scores represent a confidence measure as to whether a gene is essential, whereby a threshold of BF > 5 can be used to distinguish essential genes.
               Data is from Mair et al. 2019 Supplemental Information Tables S1 & S3.'),
  ),
  card_footer(
    htmltools::span("DepMap ",
                    htmltools::a(href = "https://depmap.org/portal/",
                                 "website")
    ),
    tags$br(),
    htmltools::span("Files used: ",
                    htmltools::a(href = "https://depmap.org/portal/download/all/", "Model.csv"),
                    ", ",
                    htmltools::a(href = "https://depmap.org/portal/download/all/", "CRISPRGeneEffect.csv")
    ),
    tags$br(),
    tags$a(href = "https://www.sciencedirect.com/science/article/pii/S2211124719302128#app2", "Mair et al. 2019")
  )
),

card(
  full_screen = TRUE,
  card_header(
    "Sequencing Constraint metrics"
  ),
  card_body(
    htmltools::h3("gnomAD transcripts"),
    htmltools::p("Gene transcripts were chosen based on the mane select transcript provided by gnomAD from the data release version 4 constraint downloads. For genes where no mane transcript was provided, the R package biomaRt
                     (v2.58.0) was used to obtain Ensembl canonical transcipts."),
    htmltools::h3("gnomAD constraint flags"),
    tags$p("Explanations as to why a transcript is missing constraint metrics, outlined here: https://storage.googleapis.com/gcp-public-data--gnomad/release/v4.0/constraint/README.txt"),
    htmltools::h3("Shet RGC-ME metrics"),
    tags$p("These metrics are derived from the RGC-ME dataset, published by Sun et al. 2023, comprising 985,830 exomes from individuals of diverse ancestry. Here, Shet RGC-ME represents the selectin coefficient estimated per gene from heterozygous probability Loss of Function
               (pLoF) variation amungst the RGC-ME dataset."),
    htmltools::h3("Shet posterior metrics"),
    tags$p("These metrics are derived from the machine learning model GeneBayes, created by Zeng et al. 2023. Here, Shet posterior represents the Bayesian estimation of the selectin coefficient from heterzygous LoF carriers from the gnomAD (v2.1.1) dataset."),
    htmltools::h3("DOMINO score"),
    tags$p("This metric was developed by Quinodoz et al. 2017 and assesses the likelihood of a gene to harbor dominant changes using the DOMINO machine learning model."),
    tags$h5("SCoNeS score"),
    tags$p("This metric was developed by Rapaport et al. 2021 and predicts the likelihood for a gene to underlie an Autosomal Dominant (AD) or Autosomal Recessive (AD) disorder. Thresholds of SCoNeS score > 0.75 and SCoNeS score < 0.25 are used
               to determine genes are underlying AR and AD disorders respectivley."),
    tags$h5("Mean AM Pathogenicity"),
    tags$p("This metric represents the probability of a gene being pathogenic, according to the 2023 AlphaMissense deep learning model developed by Cheng et al. 2023.")
  ),
  card_footer(
    htmltools::span("gnomAD ",
                    htmltools::a(href = "https://gnomad.broadinstitute.org/",
                                 "website")
    ),
    htmltools::a(href = "https://gnomad.broadinstitute.org/downloads#v4-constraint", "gnomAD v4 constraint downloads"),
    tags$br(),
    tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/37214792/", "Sun et al. 2023 (Supplement 1/media-1.txt under the Supplementary Materials section)"),
    tags$br(),
    tags$a(href = "https://www.biorxiv.org/content/10.1101/2023.05.19.541520v1", "Zeng et al 2023 (Supplemental Table 2/supplements/541520_file02.tsv)"),
    tags$br(),
    tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5630195/", "Quinodoz et al. 2017 (Supplementary File. pnas.2001248118.sd01.xlsx)"),
    tags$br(),
    tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7826345/", "Rapaport et al. 2021 (Supplementary File.pnas.2001248118.sd01.xlsx)"),
    tags$br(),
    tags$a(href = "https://www.science.org/doi/10.1126/science.adg7492", "Cheng et al. 2023 (https://console.cloud.google.com/storage/browser/dm_alphamissense: AlphaMissense_gene_hg38.tsv.gz)")
  )
),

card(
  full_screen = TRUE,
  card_header(
    "Panther DB"
  ),
  card_body(
    htmltools::p("Panther protein data was obtained using the PANTHER.db R package (v1.0.12)")
  ),
  card_footer(
    htmltools::span("Pantherdb ",
                    htmltools::a(href = "https://www.pantherdb.org/downloads/index.jsp",
                                 "website")
    )
  )
),

card(
  full_screen = TRUE,
  card_header(
    "Gene Ontology"
  ),
  card_body(
    htmltools::p("Gene Ontology data was obtained using the GO.db R package (v3.18.0)")
  ),
  card_footer(
    htmltools::span("Gene Ontology ",
                    htmltools::a(href = "https://reactome.org/",
                                 "website")
    )
  )
),

card(
  full_screen = TRUE,
  card_header(
    "Reactome"
  ),
  card_body(
    htmltools::p("Reactome data was obtained using the reactome.db R pacakge (v1.86.2)")
  ),
  card_footer(
    htmltools::span("Reactome ",
                    htmltools::a(href = "https://reactome.org/",
                                 "website")
    )
  )
)
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
