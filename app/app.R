# app.R
library(shiny)
library(duckdb)
library(bslib)
library(DT)
library(UpSetR)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(enrichplot)
library(graphics)


source("./modules/gene_list_module.R")
source("./modules/charts.R")
source("./modules/enrichment-analysis.R")

con <- dbConnect(duckdb(), "EGEx-db.duckdb")
all_tables <- dbListTables(con)
individual_tables <- setdiff(all_tables, "db_metadata")
filter_metadata <- dbReadTable(con, "db_metadata")
gene_annotations <- dbReadTable(con, "genes_flat")

ui <- page_navbar(
  tags$head(
    tags$style(HTML("
      .navbar { padding: 12px !important; }
      .navbar-brand img { height: 30px; margin-right: 8px; }
      .navbar-dark .navbar-brand, .navbar-dark .nav-link { color: #fff !important; }
    "))
  ),
  title = tagList(
    tags$img(src = "title_img.png")
  ),
  bg = "black",
  theme = bs_theme(bootswatch = "lux"),
  nav_panel(
    "Gene List",
    geneListModuleUI("gene_list")
  ),
  nav_panel(
    "Charts",
    chartsModuleUI("charts")
  ),
  nav_panel(
    "Enrichment Analysis",
    enrichmentModuleUI("enrichment")
  )
)

server <- function(input, output, session) {
  geneListModuleServer("gene_list", con, gene_annotations)
  chartsModuleServer("charts", con)
  enrichmentModuleServer("enrichment", con)
}

shinyApp(ui, server)