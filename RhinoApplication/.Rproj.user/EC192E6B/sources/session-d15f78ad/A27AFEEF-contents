# visuals_violinplots_sidebar.R

## Imports
# Packages
box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue],
  fst[read.fst]
)

# Modules
box::use(
)

# Rda data
box::use(
  app/logic/import_rda_data
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    # GENE SEARCH
    searchInput(
      ns("search_gene"),
      label = "Search Gene Symbol",
      placeholder = "DLX1",
      btnSearch = icon("search"),
      btnReset = icon("remove"),
      width = "100%",
      value = "",
      resetValue = ""
    ),
    pickerInput(
      ns("select_dpcs_vis"),
      "Select Data Gene List",
      c("JAX", "MSK", "NWU", "UCSF", "All protein coding genes"),
      multiple = TRUE,
      selected = "JAX",
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "100%"
      ),
      inline = FALSE
    ),
    checkboxInput(
      ns("toggle_all_data_points"),
      label = "Show all data points",
      width = "100%"
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    list(
      selected_dpc = reactive(input$select_dpcs_vis),
      gene_search_input = reactive(input$search_gene),
      show_all_data_points = reactive(input$toggle_all_data_points)
    )

  })
}
