# MorPhiC Gene List App

A Shiny app for browsing MorPhiC gene lists, viewing
summary charts, and exploring GO / Reactome / odds-ratio enrichment results.

**Live app:** https://whri-phenogenomics.shinyapps.io/morphic_gene_list/

## Run locally

**1. Clone** (the ~164 MB DuckDB database is stored with [Git LFS](https://git-lfs.com/)):

```bash
git lfs install
git clone https://github.com/morphic-bio/Rshiny-gene-list-app.git
cd Rshiny-gene-list-app
```

**2. Install dependencies** — in R:

```r
source("dependencies.R")
```

**3. Run** — in an IDE, `shiny::runApp("app")`, or from a terminal:

```bash
Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'
```

## Project layout

```
.
├── dependencies.R              # package installation script
└── app/
    ├── app.R                   # entry point (UI + server)
    ├── EGEx-db.duckdb          # database (Git LFS)
    ├── modules/                # Shiny modules
    │   ├── gene_list_module.R  #   gene list browser
    │   ├── charts.R            #   summary charts
    │   └── enrichment-analysis.R  # GO / Reactome / odds-ratio views
    ├── enirchment-plots-tables/   # precomputed enrichment results (.rds)
    └── www/                    # static assets (favicon, title image)
```
