# dependencies.R
# Installs the R packages required to run the MorPhiC Gene List app.
# Run once from the repo root:  source("dependencies.R")

cran_packages <- c(
  "shiny",
  "bslib",
  "duckdb",
  "DBI",
  "DT",
  "dplyr",
  "tidyr",
  "ggplot2",
  "plotly",
  "UpSetR",
  "shinyWidgets"
)

to_install <- setdiff(cran_packages, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install)
}

# Bioconductor package (used to render the enrichment bar plots)
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
if (!requireNamespace("enrichplot", quietly = TRUE)) {
  BiocManager::install("enrichplot", ask = FALSE)
}
