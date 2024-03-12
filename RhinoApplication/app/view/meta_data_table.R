box::use(
  shiny[...],
  rhino,
  DT[...],
  shinycssloaders[...],
  htmltools[...],
  fst[read.fst]
)

box::use(
  app/view/table_sidebar,
  app/logic/table_manage,
  app/logic/import_rda_data,
  app/logic/create_table
)


#' @export
ui <- function(id) {
  ns <- NS(id)
    DTOutput(
      ns("genesMetaDataDf")
      )
}

#' @export
server <- function(id, table_data, sidebar_data) {
  moduleServer(id, function(input, output, session) {

    output$genesMetaDataDf <- renderDataTable({
      headers <- create_table$table_headers()
      selected_columns <- table_manage$getHiddenColumns(sidebar_data$selected_columns())
      user_chosen_gene_lists <- table_manage$getGeneListsFromSelect(sidebar_data$selected_dpc_gene_list())

      req(length(sidebar_data$selected_dpc_gene_list()) > 0)
      user_chosen_table_data <- table_manage$subsetGenesMetaDataDf_rowsOnly(user_chosen_gene_lists)

      create_table$data_table(user_chosen_table_data, headers, selected_columns)
    }, server = TRUE)


  })
}

