box::use(
  shiny[...],
  rhino,
  DT,
  htmltools[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue],
  fst[read.fst]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    # uiOutput(
    #   ns("current_table_view")
    # ),
    # hr(),
    # textOutput(
    #   ns("select_data_table")
    # ),
    pickerInput(
      ns("select_dpcs"),
      "Select Data Production Center",
      c("JAX", "MSK", "NWU", "UCSF"),
      multiple = TRUE,
      selected = "JAX",
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "300px"
      ),
      inline = FALSE
    ),
    pickerInput(
      ns("show_cols"),
      "Select meta data to display:",
      c('DPCs studying Gene', 'Gene IDs', 'Mouse data', 'Disease data', 'Cell line data',
        'Sequencing data', 'Pantherdb protein data', 'Gene Ontology data', 'Pathway data'),
      selected = c('DPCs studying Gene', 'Gene IDs', 'Mouse data', 'Disease data'),
      multiple = TRUE,
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

    output$current_table_view <- renderUI({
      header <- 'Current view: '
      dpc_selected <- input$select_dpcs
      body <- dpc_selected
      body <- paste(body, collapse = ", ")

      HTML(glue("<p>{header} <br> {body}</p>"))
    })

    output$select_data_table <- renderText('Select Gene list:')

    list(
      selected_columns = reactive(input$show_cols),
      selected_dpc_gene_list = reactive(input$select_dpcs)
    )
  })
}


