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
      ns("select_dpcs_vis"),
      "Select Data Production Center",
      c("JAX", "MSK", "NWU", "UCSF", "All protein coding genes"),
      multiple = TRUE,
      selected = "JAX",
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "300px"
      ),
      inline = FALSE
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    visuals_sidebar_data <- reactive(input$select_dpcs_vis)
  })
}
