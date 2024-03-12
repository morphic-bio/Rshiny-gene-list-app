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
    pickerInput(
      ns("select_dpcs_vis_single"),
      "Select Data Production Center",
      c("JAX", "MSK", "NWU", "UCSF"),
      multiple = FALSE,
      selected = "JAX",
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "300px"
      ),
      inline = FALSE
    ),
    pickerInput(
      ns("select_ontology"),
      "Select GO Ontology",
      c("BP", "MF", "CC"),
      multiple = FALSE,
      selected = "BP",
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "300px"
      ),
      inline = FALSE
    ),
    awesomeCheckbox(
      ns("show_legend"),
      "Show plot legend",
      value = TRUE
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    list(
      dpc_selected = reactive(input$select_dpcs_vis_single),
      ontology = reactive(input$select_ontology),
      show_legend = reactive(input$show_legend)
    )

  })
}
