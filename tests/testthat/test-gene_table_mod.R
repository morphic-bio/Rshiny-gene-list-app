library(shiny)
source("../../gene_table_mod.R")

# test the search filtering of the gene list data table
test_that("reactives and data table output test", {
  testServer(geneListPageServer, {
    # gene list data table
    app_data_table <- readRDS("../../rda_data/app_data_table.rda")
    data_subset <- app_data_table
    
    # user search
    session$setInputs(gene_search = "DLX3")
    search_term <- isolate(input$gene_search)    
    #search_term <- gene_search
    # test data
    test_subset <- data_subset[data_subset$gene_symbol == "DLX3", ]
    
    # Check if the search term is an HGNC ID (e.g., HGNC:1234)
    if (grepl("^HGNC:\\d+$", search_term)) {
      data_subset <- data_subset[data_subset$hgnc_id == search_term, ]
    } else {
      # Check if the search term is an Entrez ID (e.g., ENSG12345)
      if (grepl("^ENSG\\d+$", search_term)) {
        data_subset <- data_subset[data_subset$ensembl_id == search_term, ]
      } else {
        # Check if the search term is an MGI ID (e.g., MGI:12345)
        if (grepl("^MGI:\\d+$", search_term)) {
          data_subset <- data_subset[data_subset$mgi_id == search_term, ]
        } else {
          # Check if the search term is a gene symbol (e.g., Symbol_ABC)
          data_subset <- data_subset[data_subset$gene_symbol == search_term, ]
        }
      }
    }
    
    expect_equal(data_subset, test_subset)
  
  })
})