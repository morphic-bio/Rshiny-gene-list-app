# gene annotations page
geneListModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Global CSS (not namespaced) to stop auto-filling heights/padding
    tags$style(HTML("
      .bslib-grid { grid-auto-rows: auto !important; }
      .bslib-grid-item { height: auto !important; }
      .html-fill-container { height: auto !important; min-height: 0 !important; }
      .bslib-navs-content { padding-top: 0 !important; padding-bottom: 0 !important; }
      .bslib-sidebar-layout > .main { padding: 0 !important; }
      .bslib-sidebar-layout > .sidebar { padding-top: .5rem !important; padding-bottom: .5rem !important; }
    ")),
    
    navset_tab(
      id = ns("table_tabs"),
      nav_panel(
        "Table",
        layout_sidebar(
          fillable = FALSE,
          sidebar = sidebar(
            # accordion(
            #   accordion_panel(
            # "Display options",
            selectInput(
              ns("which_lists"),
              "Select Gene Lists",
              choices  = c("JAX","UCSF","MSK","NWU","All MorPhiC Genes","All protein coding genes"),
              selected = c("JAX","UCSF","MSK","NWU"),
              multiple = TRUE
            ),
            radioButtons(
              ns("set_mode"),
              "Show union or intersection of genes",
              choices  = c("Union" = "union", "Intersection" = "intersect"),
              selected = "union",
              inline   = TRUE
            ),
            actionButton(ns("show_info"), "Table info"),
            downloadButton(ns("download_table"), "Download table")
            #   )
            # )
          ),
          DTOutput(ns("gene_list_table"))
        )
      ),
      nav_panel(
        "Upset Plot",
        # Keep control outside the grid so it doesn't inherit tall row height
        div(
          selectInput(
            ns("upset_lists"),
            "Select gene lists for UpSet plot",
            choices  = c("JAX","UCSF","MSK","NWU","All MorPhiC Genes","All protein coding genes"),
            selected = c("JAX","UCSF","MSK","NWU"),
            multiple = TRUE
          )
        ),
        layout_column_wrap(
          width = 1,
          plotOutput(ns("upset_plot"), height = "520px"),
          p("The UpSet plot visualizes the intersections of gene sets selected by each Data Production Center (JAX, NWU, MSK, UCSF). It shows how many genes are unique to each center and how many are shared across multiple centers."),
          p("Four of the twenty-three genes currently planned for targeted null allele generation will share the same cell line across all four DPCs. This is referred to as the Exchange Experiment, these genes are ISL1, EOMES, GCM1, and NKX2-1.")
        )
      )
    )
  )
}

geneListModuleServer <- function(id, con, gene_annotations) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    colnames(gene_annotations) <- gsub("\\.", " ", colnames(gene_annotations))
    symbol_col <- if ("HGNC Gene Symbol" %in% colnames(gene_annotations)) "HGNC Gene Symbol" else "symbol"
    
    dpc_lists <- reactive({
      tbl <- tryCatch(DBI::dbReadTable(con, "dpc_gene_lists"), error = function(e) NULL)
      if (is.null(tbl)) {
        list(JAX=character(),UCSF=character(),MSK=character(),NWU=character(),
             MORPHIC_ALL=character(),
             PROTEIN_ALL=tryCatch(DBI::dbGetQuery(con, 'SELECT DISTINCT "symbol" FROM "genes";')[,1], error=function(e) character()))
      } else {
        list(
          JAX  = stats::na.omit(tbl$JAX),
          UCSF = stats::na.omit(tbl$UCSF),
          MSK  = stats::na.omit(tbl$MSK),
          NWU  = stats::na.omit(tbl$NWU),
          MORPHIC_ALL = unique(c(stats::na.omit(tbl$JAX), stats::na.omit(tbl$UCSF),
                                 stats::na.omit(tbl$MSK), stats::na.omit(tbl$NWU))),
          PROTEIN_ALL = tryCatch(DBI::dbGetQuery(con, 'SELECT DISTINCT "symbol" FROM "genes";')[,1], error=function(e) character())
        )
      }
    })
    
    filtered_data <- reactive({
      sel  <- input$which_lists
      mode <- input$set_mode
      if (is.null(sel) || length(sel) == 0) return(gene_annotations)
      key_map <- c(
        "JAX" = "JAX", "UCSF" = "UCSF", "MSK" = "MSK", "NWU" = "NWU",
        "All MorPhiC Genes" = "MORPHIC_ALL",
        "All protein coding genes" = "PROTEIN_ALL"
      )
      sets <- lapply(sel, function(k) dpc_lists()[[ key_map[[k]] ]])
      sets <- Filter(function(x) length(x) > 0, sets)
      if (!length(sets)) return(gene_annotations)
      genes <- if (identical(mode, "intersect")) Reduce(intersect, sets) else Reduce(union, sets)
      if (!length(genes)) return(gene_annotations[0, , drop = FALSE])
      gene_annotations[gene_annotations[[symbol_col]] %in% genes, , drop = FALSE]
    })
    
    output$gene_list_table <- renderDT({
      x <- filtered_data()
      datatable(
        x,
        rownames = FALSE,
        filter = 'top',
        plugins = "ellipsis",
        options = list(
          dom = "Bfrtip",
          searching = TRUE,
          columnDefs = list(list(targets = "_all", render = JS("$.fn.dataTable.render.ellipsis(17, true)"))),
          pageLength = 100,
          scrollX = TRUE
        )
      )
    })
    
    output$download_table <- downloadHandler(
      filename = function() paste0("genes_flat_filtered_", Sys.Date(), ".csv"),
      content = function(file) {
        x <- filtered_data()
        write.csv(x, file, row.names = FALSE, na = "")
      }
    )
    
    meta_tbl <- reactiveVal(NULL)
    
    observeEvent(input$show_info, {
      meta <- tryCatch(DBI::dbReadTable(con, "db_metadata"), error = function(e) NULL)
      if (is.null(meta)) {
        showModal(modalDialog(title = "Table info", "Metadata table not found.", easyClose = TRUE))
        return()
      }
      present_cols <- intersect(meta$column_nice_name, colnames(gene_annotations))
      meta_sub <- meta[meta$column_nice_name %in% present_cols,
                       c("column_nice_name", "description", "data_source", "version")]
      meta_sub <- meta_sub[match(colnames(gene_annotations), meta_sub$column_nice_name, nomatch = 0), , drop = FALSE]
      colnames(meta_sub) <- c("Column", "Description", "Source", "Version")
      meta_tbl(meta_sub)
      
      output$meta_dt <- DT::renderDT({
        DT::datatable(
          meta_tbl(),
          rownames = FALSE,
          filter = 'top',
          options = list(pageLength = 25, scrollX = TRUE, dom = "frtip")
        )
      })
      
      showModal(
        modalDialog(
          title = "Table info",
          tagList(
            DTOutput(ns("meta_dt")),
            br(),
            downloadButton(ns("download_meta"), "Download metadata")
          ),
          size = "xl",
          easyClose = TRUE
        )
      )
    })
    
    output$download_meta <- downloadHandler(
      filename = function() paste0("genes_flat_metadata_", Sys.Date(), ".csv"),
      content = function(file) {
        m <- meta_tbl()
        if (is.null(m)) m <- data.frame()
        write.csv(m, file, row.names = FALSE, na = "")
      }
    )
    
    generateUpsetR <- function(gene_lists, set_size_max = NULL) {
      if (!length(gene_lists)) return(invisible(NULL))
      input_list <- lapply(gene_lists, function(x) x$gene_list)
      if (!length(input_list)) return(invisible(NULL))
      if (is.null(set_size_max)) {
        set_size_max <- max(vapply(input_list, length, integer(1L), USE.NAMES = FALSE), 0) + 50
      }
      if (length(input_list) == 1) {
        # UpSetR::fromList works with 1 set, but guard to avoid internal 1:n issues
        names(input_list) <- if (is.null(names(input_list))) "Set1" else names(input_list)
      }
      upset(
        UpSetR::fromList(input_list),
        order.by = "freq",
        matrix.color = "black",
        main.bar.color = "black",
        sets.bar.color = "black",
        shade.color = "gray88",
        set_size.show = TRUE,
        show.numbers = "yes",
        text.scale = c(2, 2, 2, 2, 2, 2),
        point.size = 4,
        line.size = 1.5,
        set_size.scale_max = set_size_max
      )
    }
    
    upset_gene_lists <- reactive({
      sel <- input$upset_lists
      if (is.null(sel) || !length(sel)) return(list())
      key_map <- c(
        "JAX" = "JAX", "UCSF" = "UCSF", "MSK" = "MSK", "NWU" = "NWU",
        "All MorPhiC Genes" = "MORPHIC_ALL",
        "All protein coding genes" = "PROTEIN_ALL"
      )
      base <- dpc_lists()
      sets <- lapply(sel, function(k) base[[ key_map[[k]] ]])
      names(sets) <- sel
      lapply(sets, function(v) list(gene_list = unique(stats::na.omit(v))))
    })
    
    output$upset_plot <- renderPlot({
      gl <- upset_gene_lists()
      if (!length(gl)) return(invisible(NULL))  # no warning/error when 0 selected
      generateUpsetR(gl)
    })
  })
}