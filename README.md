# MorPhiC Gene List App

A [Shiny](https://shiny.posit.co/) app for browsing MorPhiC gene lists, viewing
summary charts, and exploring GO / Reactome / odds-ratio enrichment results. All
data is served from a bundled [DuckDB](https://duckdb.org/) database, so the app
runs fully locally with no external services.

## 1. Clone (with the database)

The database `app/EGEx-db.duckdb` (~164 MB) is stored with **[Git LFS](https://git-lfs.com/)**.
Install Git LFS first, otherwise you'll only get a small pointer file instead of the
real database.

```bash
git lfs install
git clone https://github.com/morphic-bio/Rshiny-gene-list-app.git
cd Rshiny-gene-list-app
```

If you cloned before installing LFS, fetch the database with:

```bash
git lfs pull
```

## 2. Install R dependencies

From the repo root, in R:

```r
source("dependencies.R")
```

This installs the required CRAN packages and the Bioconductor `enrichplot` package.

## 3. Run the app

The app uses paths relative to the `app/` directory, so launch it by pointing
Shiny at that folder:

```r
shiny::runApp("app", launch.browser = TRUE)
```

or from a terminal:

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
