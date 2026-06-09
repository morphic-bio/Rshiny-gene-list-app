# modules/enrichment-analysis.R

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(ggplot2)
library(enrichplot)

enrichmentModuleUI <- function(id) {
  ns <- NS(id)
  
  navset_tab(
    id = ns("enrichment_nav"),
    nav_panel("GO",         uiOutput(ns("go_tab"))),
    nav_panel("Reactome",   uiOutput(ns("reactome_tab"))),
    nav_panel("Odds Ratio", uiOutput(ns("odds_tab")))
  )
}

enrichmentModuleServer <- function(id, con, load_trigger = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    go_path       <- "./enirchment-plots-tables/go_results_v3.rds"
    reactome_path <- "./enirchment-plots-tables/reactome_results_v3.rds"
    odds_path     <- "./enirchment-plots-tables/oddsratio_results_v3.rds"
    
    sanitize_id <- function(x) gsub("[^a-zA-Z0-9]+", "_", x)
    
    safe_readRDS <- function(path) {
      if (!file.exists(path)) {
        warning("File not found: ", path)
        return(NULL)
      }
      readRDS(path)
    }
    
    go_results_rv       <- reactiveVal(NULL)
    reactome_results_rv <- reactiveVal(NULL)
    odds_results_rv     <- reactiveVal(NULL)
    
    go_choices_rv       <- reactiveVal(character(0))
    reactome_choices_rv <- reactiveVal(character(0))
    
    observeEvent(load_trigger(), {
      if (!isTRUE(load_trigger())) return()
      
      if (is.null(go_results_rv())) {
        gr <- safe_readRDS(go_path)
        go_results_rv(gr)
        if (!is.null(gr)) {
          glists <- sort(unique(unlist(lapply(gr, names))))
          go_choices_rv(glists)
        }
      }
      
      if (is.null(reactome_results_rv())) {
        rr <- safe_readRDS(reactome_path)
        reactome_results_rv(rr)
        if (!is.null(rr)) {
          reactome_choices_rv(sort(names(rr)))
        }
      }
      
      if (is.null(odds_results_rv())) {
        or <- safe_readRDS(odds_path)
        odds_results_rv(or)
      }
    }, ignoreNULL = FALSE)
    
    #---------------- GO ----------------#
    output$go_tab <- renderUI({
      go_results <- go_results_rv()
      if (is.null(go_results)) {
        return(div("Loading GO enrichment results…"))
      }
      
      go_gene_list_choices <- go_choices_rv()
      if (length(go_gene_list_choices) == 0) {
        go_gene_list_choices <- sort(unique(unlist(lapply(go_results, names))))
        go_choices_rv(go_gene_list_choices)
      }
      if (length(go_gene_list_choices) == 0) {
        return(div("No GO gene set choices found in results."))
      }
      
      selected_gl <- input$go_gene_list
      if (is.null(selected_gl) || !(selected_gl %in% go_gene_list_choices)) {
        selected_gl <- go_gene_list_choices[[1]]
      }
      
      show_cat <- input$go_showCategory
      if (is.null(show_cat) || !is.finite(show_cat) || show_cat < 1) show_cat <- 10L
      
      cards <- list()
      
      for (ont in names(go_results)) {
        ont_label <- switch(
          ont,
          BP = "Biological Process",
          MF = "Molecular Function",
          CC = "Cellular Component",
          ont
        )
        
        if (!(selected_gl %in% names(go_results[[ont]]))) next
        
        res <- go_results[[ont]][[selected_gl]]
        
        base_id    <- paste0("go_", ont, "_", sanitize_id(selected_gl))
        plot_id    <- paste0(base_id, "_plot")
        table_id   <- paste0(base_id, "_table")
        card_title <- paste("GO", ont_label, "-", selected_gl)
        
        has_enrich <- !is.null(res$enrich)
        has_tbl    <- !is.null(res$table)
        
        local({
          res_local    <- res
          plot_id_loc  <- plot_id
          table_id_loc <- table_id
          
          if (!is.null(res_local$enrich)) {
            output[[plot_id_loc]] <- renderPlot({
              sc <- input$go_showCategory
              if (is.null(sc) || !is.finite(sc) || sc < 1) sc <- 10L
              
              if (nrow(as.data.frame(res_local$enrich)) == 0) {
                plot.new()
                text(0.5, 0.5, "no enriched terms for this gene set")
              } else {
                graphics::barplot(res_local$enrich, showCategory = sc)
              }
            })
          }
          
          if (!is.null(res_local$table)) {
            output[[table_id_loc]] <- renderDT({
              datatable(
                res_local$table,
                options = list(
                  pageLength   = 10,
                  lengthChange = FALSE
                )
              )
            })
          }
        })
        
        cards[[length(cards) + 1L]] <-
          navset_card_tab(
            title       = card_title,
            full_screen = TRUE,
            nav_panel(
              "Plot",
              if (has_enrich) plotOutput(ns(plot_id), height = "350px") else div("no enriched terms for this gene set")
            ),
            nav_panel(
              "Table",
              if (has_tbl) {
                div(style = "max-height:400px; overflow-y:auto;", DTOutput(ns(table_id)))
              } else {
                div("no table available for this gene set")
              }
            )
          )
      }
      
      tagList(
        fluidRow(
          column(
            6,
            selectInput(
              ns("go_gene_list"),
              "Select gene set:",
              choices  = go_gene_list_choices,
              selected = selected_gl,
              multiple = FALSE
            )
          ),
          column(
            6,
            numericInput(
              ns("go_showCategory"),
              "Show categories (barplot):",
              value = if (!is.null(input$go_showCategory)) input$go_showCategory else 10,
              min   = 1,
              step  = 1
            )
          )
        ),
        layout_column_wrap(width = 1/2, !!!cards)
      )
    })
    
    #---------------- Reactome ----------------#
    output$reactome_tab <- renderUI({
      reactome_results <- reactome_results_rv()
      if (is.null(reactome_results)) {
        return(div("Loading Reactome enrichment results…"))
      }
      
      reactome_gene_list_choices <- reactome_choices_rv()
      if (length(reactome_gene_list_choices) == 0) {
        reactome_gene_list_choices <- sort(names(reactome_results))
        reactome_choices_rv(reactome_gene_list_choices)
      }
      if (length(reactome_gene_list_choices) == 0) {
        return(div("No Reactome gene set choices found in results."))
      }
      
      selected_gl <- input$reactome_gene_list
      if (is.null(selected_gl) || !(selected_gl %in% reactome_gene_list_choices)) {
        selected_gl <- reactome_gene_list_choices[[1]]
      }
      
      show_cat <- input$reactome_showCategory
      if (is.null(show_cat) || !is.finite(show_cat) || show_cat < 1) show_cat <- 10L
      
      cards <- list()
      
      if (selected_gl %in% names(reactome_results)) {
        res <- reactome_results[[selected_gl]]
        
        base_id    <- paste0("reactome_", sanitize_id(selected_gl))
        plot_id    <- paste0(base_id, "_plot")
        table_id   <- paste0(base_id, "_table")
        card_title <- paste("Reactome -", selected_gl)
        
        has_enrich <- !is.null(res$enrich)
        has_tbl    <- !is.null(res$table)
        
        local({
          res_local    <- res
          plot_id_loc  <- plot_id
          table_id_loc <- table_id
          
          if (!is.null(res_local$enrich)) {
            output[[plot_id_loc]] <- renderPlot({
              sc <- input$reactome_showCategory
              if (is.null(sc) || !is.finite(sc) || sc < 1) sc <- 10L
              
              if (nrow(as.data.frame(res_local$enrich)) == 0) {
                plot.new()
                text(0.5, 0.5, "no enriched terms for this gene set")
              } else {
                graphics::barplot(res_local$enrich, showCategory = sc)
              }
            })
          }
          
          if (!is.null(res_local$table)) {
            output[[table_id_loc]] <- renderDT({
              datatable(
                res_local$table,
                options = list(
                  pageLength   = 10,
                  lengthChange = FALSE
                )
              )
            })
          }
        })
        
        cards[[length(cards) + 1L]] <-
          navset_card_tab(
            title       = card_title,
            full_screen = TRUE,
            nav_panel(
              "Plot",
              if (has_enrich) plotOutput(ns(plot_id), height = "350px") else div("no enriched terms for this gene set")
            ),
            nav_panel(
              "Table",
              if (has_tbl) {
                div(style = "max-height:400px; overflow-y:auto;", DTOutput(ns(table_id)))
              } else {
                div("no table available for this gene set")
              }
            )
          )
      }
      
      tagList(
        fluidRow(
          column(
            6,
            selectInput(
              ns("reactome_gene_list"),
              "Select gene set:",
              choices  = reactome_gene_list_choices,
              selected = selected_gl,
              multiple = FALSE
            )
          ),
          column(
            6,
            numericInput(
              ns("reactome_showCategory"),
              "Show categories (barplot):",
              value = if (!is.null(input$reactome_showCategory)) input$reactome_showCategory else 10,
              min   = 1,
              step  = 1
            )
          )
        ),
        layout_column_wrap(width = 1/2, !!!cards)
      )
    })
    
    #---------------- Odds ratio ----------------#
    output$odds_tab <- renderUI({
      odds_results <- odds_results_rv()
      if (is.null(odds_results)) {
        return(div("Loading odds ratio results…"))
      }
      
      pretty_label <- function(name) {
        label_map <- c(
          OMIM_all_disease = "OMIM All Disease Genes",
          IMPC_lethal      = "IMPC Lethal Genes",
          DepMap_essential = "DepMap Essential Genes",
          LOEUF_0_6        = "gnomAD constrained genes"
        )
        if (!is.null(label_map[[name]])) label_map[[name]] else gsub("_", " ", name)
      }
      
      cards <- list()
      
      # Each odds_results[[target_name]] is list(plot = ggplot, table = df)
      for (target_name in names(odds_results)) {
        res <- odds_results[[target_name]]
        
        base_id    <- paste0("odds_", sanitize_id(target_name))
        plot_id    <- paste0(base_id, "_plot")
        table_id   <- paste0(base_id, "_table")
        
        # Title shown on the CARD (header), not inside the plot
        card_title <- pretty_label(target_name)
        
        has_plot <- !is.null(res$plot)
        has_tbl  <- !is.null(res$table)
        
        local({
          res_local    <- res
          plot_id_loc  <- plot_id
          table_id_loc <- table_id
          
          if (!is.null(res_local$plot)) {
            output[[plot_id_loc]] <- renderPlot({
              # ensure plot itself has no title; card header carries it
              res_local$plot + ggplot2::labs(title = NULL)
            })
          }
          
          if (!is.null(res_local$table)) {
            output[[table_id_loc]] <- renderDT({
              datatable(
                res_local$table,
                options = list(
                  pageLength   = 10,
                  lengthChange = FALSE
                )
              )
            })
          }
        })
        
        cards[[length(cards) + 1L]] <-
          navset_card_tab(
            title       = card_title,   # <- title in the card header
            full_screen = TRUE,
            nav_panel(
              "Plot",
              if (has_plot) {
                plotOutput(ns(plot_id), height = "350px")
              } else {
                div("no plot available for this target")
              }
            ),
            nav_panel(
              "Table",
              if (has_tbl) {
                div(
                  style = "max-height:400px; overflow-y:auto;",
                  DTOutput(ns(table_id))
                )
              } else {
                div("no table available for this target")
              }
            )
          )
      }
      
      layout_column_wrap(width = 1/2, !!!cards)
    })
  })
}