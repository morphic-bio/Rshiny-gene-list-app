# ========================= UI ========================= #
chartsModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # minimal helpers
    tags$style(HTML("
      .egex-options-row { display:flex; align-items:end; gap:1rem; flex-wrap:wrap; }
    ")),
    
    bslib::navset_card_pill(
      id = ns("tabs"),
      full_screen = FALSE,
      
      # ---- Human Disease ----
      bslib::nav_panel(
        "Human Disease",
        div(
          class = "egex-options-row",
          div(
            shinyWidgets::pickerInput(
              inputId = ns("pick_disease"),
              label   = "Select gene lists",
              choices = character(0),
              multiple = TRUE
            )
          ),
          bslib::tooltip(
            trigger   = checkboxInput(ns("pct_disease"), "Show %", TRUE),
            "Express counts as a percentage of genes within each selected list.",
            placement = "right"
          ),
          bslib::tooltip(
            trigger   = checkboxInput(ns("na_disease"), "Include NA", FALSE),
            "Include genes without this annotation in denominators and show as (NA) where applicable.",
            placement = "right"
          )
        ),
        uiOutput(ns("ui_disease"))
      ),
      
      # ---- Mouse Knockouts ----
      bslib::nav_panel(
        "Mouse Knockouts",
        div(
          class = "egex-options-row",
          div(
            shinyWidgets::pickerInput(
              inputId = ns("pick_mouse"),
              label   = "Select gene lists",
              choices = character(0),
              multiple = TRUE
            )
          ),
          bslib::tooltip(
            trigger   = checkboxInput(ns("pct_mouse"), "Show %", TRUE),
            "Express counts as a percentage of genes within each selected list.",
            placement = "right"
          ),
          bslib::tooltip(
            trigger   = checkboxInput(ns("na_mouse"), "Include NA", FALSE),
            "Include genes without this annotation in denominators and show as (NA) where applicable.",
            placement = "right"
          )
        ),
        uiOutput(ns("ui_mouse"))
      ),
      
      # ---- Human Cellular Knockouts ----
      bslib::nav_panel(
        "Human Cellular Knockouts",
        div(
          class = "egex-options-row",
          div(
            shinyWidgets::pickerInput(
              inputId = ns("pick_cell"),
              label   = "Select gene lists",
              choices = character(0),
              multiple = TRUE
            )
          ),
          div(
            textInput(
              inputId = ns("cell_highlight"),
              label   = "Highlight gene symbols (comma/space/newline separated)",
              placeholder = "e.g. TP53, BRCA1 BRCA2\nSMAD4"
            )
          )
        ),
        uiOutput(ns("ui_cell"))
      ),
      
      # ---- Constraint Metrics ----
      bslib::nav_panel(
        "Constraint Metrics",
        div(
          class = "egex-options-row",
          div(
            shinyWidgets::pickerInput(
              inputId = ns("pick_constraint"),
              label   = "Select gene lists",
              choices = character(0),
              multiple = TRUE
            )
          ),
          div(
            textInput(
              inputId = ns("constraint_highlight"),
              label   = "Highlight gene symbols (comma/space/newline separated)",
              placeholder = "e.g. PCSK9 LDLR APOB"
            )
          )
        ),
        uiOutput(ns("ui_constraint"))
      )
    )
  )
}

# ========================= Plotting helper ========================= #
auto_plot_one_var <- function(con, df, gene_lists, as_percent = TRUE,
                              show_na = FALSE, col_nice_name = NULL) {
  col <- setdiff(names(df), "GeneID")
  stopifnot(length(col) == 1)
  if (is.null(col_nice_name)) col_nice_name <- col

  col_is_num <- is.numeric(df[[col]])

  # Build long data: one row per gene per list
  long <- do.call(rbind, lapply(names(gene_lists), function(lst) {
    ids <- gene_lists[[lst]]
    sub <- df[df$GeneID %in% ids, , drop = FALSE]
    if (!nrow(sub)) return(NULL)
    data.frame(List = lst, Value = sub[[col]], stringsAsFactors = FALSE)
  }))

  if (is.null(long) || !nrow(long)) {
    return(plotly::plot_ly() %>%
             plotly::layout(title = "No data available"))
  }

  if (col_is_num) {
    # --- Numeric: box plot ---
    if (!show_na) long <- long[!is.na(long$Value), ]
    if (!nrow(long)) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No non-NA data"))
    }
    p <- plotly::plot_ly(long, x = ~List, y = ~Value, type = "box",
                         color = ~List) %>%
      plotly::layout(showlegend = FALSE)
  } else {
    # --- Categorical ---
    n_unique <- length(unique(stats::na.omit(long$Value)))

    if (n_unique >= 10) {
      # High-cardinality: collapse to binary "Has / Does not have" annotations
      has_label     <- paste0("Has ", col_nice_name, " annotations")
      has_not_label <- paste0("Does not have ", col_nice_name, " annotations")
      long$Value <- ifelse(is.na(long$Value), has_not_label, has_label)

      counts <- as.data.frame(table(List = long$List, Value = long$Value),
                              stringsAsFactors = FALSE)
      names(counts)[3] <- "N"

      totals <- tapply(counts$N, counts$List, sum)
      if (as_percent) {
        counts$Y <- counts$N / totals[counts$List]
      } else {
        counts$Y <- counts$N
      }

      # Order so "Has" comes first
      counts$Value <- factor(counts$Value, levels = c(has_label, has_not_label))

      p <- plotly::plot_ly(counts, x = ~Value, y = ~Y, color = ~List,
                           type = "bar",
                           hovertemplate = if (as_percent)
                             paste0("%{y:.1%}<br>n=%{customdata}<extra>%{fullData.name}</extra>")
                           else
                             paste0("%{y}<extra>%{fullData.name}</extra>"),
                           customdata = ~N) %>%
        plotly::layout(barmode = "group",
                       yaxis = list(tickformat = if (as_percent) ".0%" else NULL))
    } else {
      # Low-cardinality: standard grouped bar chart
      if (!show_na) {
        long <- long[!is.na(long$Value), ]
      } else {
        long$Value[is.na(long$Value)] <- "(NA)"
      }
      if (!nrow(long)) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      counts <- as.data.frame(table(List = long$List, Value = long$Value),
                              stringsAsFactors = FALSE)
      names(counts)[3] <- "N"

      if (as_percent) {
        totals <- tapply(counts$N, counts$List, sum)
        counts$Y <- counts$N / totals[counts$List]
      } else {
        counts$Y <- counts$N
      }

      p <- plotly::plot_ly(counts, x = ~Value, y = ~Y, color = ~List,
                           type = "bar",
                           hovertemplate = if (as_percent)
                             paste0("%{y:.1%}<br>n=%{customdata}<extra>%{fullData.name}</extra>")
                           else
                             paste0("%{y}<extra>%{fullData.name}</extra>"),
                           customdata = ~N) %>%
        plotly::layout(barmode = "group",
                       yaxis = list(tickformat = if (as_percent) ".0%" else NULL))
    }
  }

  p
}

# ========================= SERVER ========================= #
chartsModuleServer <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Column groups ----
    cols_disease    <- c("phenotypes", "panel_disease_group", "earliest_lethality_category")
    cols_mouse      <- c("wol", "impc_phenotypes", "impc_viability", "viability_mgi")
    cols_cell       <- c("mean_score_all", "bf_mef", "bf_lam", "NE_pluripotency_score",
                         "DE_pluripotency_score", "E8_self_renewal_score", "E6_self_renewal_score")
    cols_constraint <- c("LOEUF", "mean_am_pathogenicity", "shet_post_mean")
    
    # ---- Helpers ----
    safe_id <- function(x) gsub("[^A-Za-z0-9_]+", "_", x)
    
    meta <- reactive({ DBI::dbReadTable(con, "db_metadata") })
    gene_map <- reactive({ DBI::dbGetQuery(con, 'SELECT "GeneID","symbol" FROM "genes";') })
    
    get_tbl <- function(col) {
      m <- meta()
      tcol <- if ("table" %in% names(m)) "table" else if ("table_name" %in% names(m)) "table_name" else NULL
      validate(need(!is.null(tcol), "db_metadata needs 'table' or 'table_name'"))
      hit <- unique(m[[tcol]][m$column == col])
      validate(need(length(hit) && !is.na(hit[1]), sprintf("No table for column '%s'", col)))
      hit[1]
    }
    nice_name <- function(col) {
      m <- meta()
      nn <- if ("column_nice_name" %in% names(m)) unique(m$column_nice_name[m$column == col]) else NULL
      if (length(nn) && !is.na(nn[1]) && nzchar(nn[1])) nn[1] else col
    }
    desc_text <- function(col) {
      m <- meta()
      dcol <- intersect(c("description","column_description","desc"), names(m))
      if (!length(dcol)) return("No description available.")
      d <- unique(m[[dcol[1]]][m$column == col])
      if (length(d) && nzchar(d[1])) d[1] else "No description available."
    }
    source_text <- function(col) {
      m <- meta()
      if (!"data_source" %in% names(m)) return("metadata")
      s <- unique(m$data_source[m$column == col])
      if (length(s) && !is.na(s[1]) && nzchar(s[1])) s[1] else "metadata"
    }
    
    # ---- Read DPC lists (supports WIDE and LONG) ----
    raw_dpc <- reactive({
      out <- try(DBI::dbReadTable(con, "dpc_gene_lists"), silent = TRUE)
      if (inherits(out, "try-error")) data.frame() else out
    })
    
    dpc_lists_by_name <- reactive({
      raw <- raw_dpc()
      if (!nrow(raw)) return(list())
      
      if (any(names(raw) %in% c("list_name","list"))) {
        list_col <- if ("list_name" %in% names(raw)) "list_name" else "list"
        has_gid  <- "GeneID" %in% names(raw)
        sym_col  <- if ("symbol" %in% names(raw)) "symbol" else if ("hgnc_symbol" %in% names(raw)) "hgnc_symbol" else NULL
        if (!has_gid && is.null(sym_col)) return(list())
        if (!has_gid) {
          gm  <- gene_map()
          tmp <- merge(raw[, c(list_col, sym_col)], gm, by.x = sym_col, by.y = "symbol", all.x = TRUE)
          tmp <- tmp[!is.na(tmp$GeneID), c(list_col, "GeneID")]
        } else {
          tmp <- raw[, c(list_col, "GeneID")]
        }
        return(split(tmp$GeneID, tmp[[list_col]]))
      }
      
      gm <- gene_map()
      out <- list()
      for (cn in names(raw)) {
        vals <- unique(na.omit(raw[[cn]]))
        if (!length(vals)) next
        tmp <- merge(data.frame(symbol = vals, stringsAsFactors = FALSE), gm, by = "symbol", all.x = TRUE)
        gids <- unique(na.omit(tmp$GeneID))
        if (length(gids)) out[[cn]] <- gids
      }
      out
    })
    
    available_lists <- reactive({
      base <- dpc_lists_by_name()
      out  <- list()
      
      union_by_tag <- function(tag_regex) {
        nm <- names(base); if (is.null(nm) || !length(nm)) return(integer(0))
        idx <- grep(tag_regex, nm, ignore.case = TRUE); if (!length(idx)) return(integer(0))
        unique(unlist(base[idx], use.names = FALSE))
      }
      
      jax <- union_by_tag("\\bJAX\\b")
      ucs <- union_by_tag("\\bUCSF\\b")
      msk <- union_by_tag("\\bMSK\\b")
      nwu <- union_by_tag("\\bNWU\\b")
      
      if (length(jax)) out[["JAX"]] <- jax
      if (length(ucs)) out[["UCSF"]] <- ucs
      if (length(msk)) out[["MSK"]]  <- msk
      if (length(nwu)) out[["NWU"]]  <- nwu
      
      union_all <- unique(c(jax, ucs, msk, nwu))
      if (length(union_all)) out[["All MorPhiC genes"]] <- union_all
      
      out[["All protein coding genes"]] <- unique(gene_map()$GeneID)
      
      wanted <- c("JAX","UCSF","MSK","NWU","All MorPhiC genes","All protein coding genes")
      out[intersect(wanted, names(out))]
    })
    
    # ---- Defaults (select DPCs once when empty) ----
    observeEvent(available_lists(), ignoreInit = FALSE, {
      ch   <- names(available_lists()); req(length(ch) > 0)
      dpcs <- intersect(c("JAX","UCSF","MSK","NWU"), ch)
      for (id in c("pick_disease","pick_mouse","pick_cell","pick_constraint")) {
        cur <- input[[id]]
        if (is.null(cur) || !length(cur)) {
          shinyWidgets::updatePickerInput(session, id, choices = ch, selected = dpcs)
        } else {
          shinyWidgets::updatePickerInput(session, id, choices = ch)
        }
      }
    })
    
    # ---- Cards (4 per row) with centered Info footer ----
    make_grid <- function(cols, key) {
      cards <- lapply(cols, function(cn) {
        oid     <- paste0("plot_", key, "_", safe_id(cn))
        info_id <- paste0("info_", key, "_", safe_id(cn))
        
        column(
          width = 3,
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(nice_name(cn)),
            plotly::plotlyOutput(ns(oid), height = "420px"),
            bslib::card_footer(
              div(
                class = "d-flex justify-content-center",
                actionButton(ns(info_id), label = "Info", class = "btn-outline-secondary btn-sm")
              )
            )
          )
        )
      })
      rows <- list()
      if (length(cards)) {
        idx <- split(seq_along(cards), ceiling(seq_along(cards)/4))
        for (ii in seq_along(idx)) rows[[ii]] <- fluidRow(cards[idx[[ii]]])
      }
      do.call(tagList, rows)
    }
    output$ui_disease    <- renderUI(make_grid(cols_disease,    "disease"))
    output$ui_mouse      <- renderUI(make_grid(cols_mouse,      "mouse"))
    output$ui_cell       <- renderUI(make_grid(cols_cell,       "cell"))
    output$ui_constraint <- renderUI(make_grid(cols_constraint, "constraint"))
    
    # ---- Info modals (XL size; use data_source) ----
    bind_info_modals <- function(cols, key) {
      lapply(cols, function(cn) {
        info_id <- paste0("info_", key, "_", safe_id(cn))
        observeEvent(input[[info_id]], {
          showModal(
            modalDialog(
              title = paste("About:", nice_name(cn)),
              tagList(
                tags$p(tags$b("Description")),
                tags$p(desc_text(cn)),
                tags$hr(),
                tags$p(tags$b("Source")),
                tags$p(source_text(cn))
              ),
              easyClose = TRUE,
              size = "xl",
              footer = modalButton("Close")
            )
          )
        }, ignoreInit = TRUE)
      })
    }
    bind_info_modals(cols_disease,    "disease")
    bind_info_modals(cols_mouse,      "mouse")
    bind_info_modals(cols_cell,       "cell")
    bind_info_modals(cols_constraint, "constraint")
    
    # ---- Parse highlight symbols ----
    parse_symbols <- function(txt) {
      if (is.null(txt) || !nzchar(txt)) return(character(0))
      toks <- unlist(strsplit(txt, "[,\\s\\n\\r;]+", perl = TRUE))
      unique(toupper(toks[nzchar(toks)]))
    }
    
    # ---- Picker selection -> named list ----
    lists_from <- function(sel) {
      all <- available_lists()
      if (is.null(sel) || !length(sel)) return(list())
      sel <- sel[sel %in% names(all)]
      if (!length(sel)) return(list())
      all[sel]
    }
    
    # ---- Plot renderer ----
    render_one <- function(out_id, col, gene_lists, as_percent, show_na, highlight_syms = character(0)) {
      output[[out_id]] <- plotly::renderPlotly({
        validate(need(length(gene_lists) > 0, "Select at least one gene list to display the plot"))
        
        tbl <- get_tbl(col)
        df  <- DBI::dbGetQuery(con, sprintf('SELECT "GeneID","%s" FROM "%s";', col, tbl))
        validate(need(nrow(df) > 0, sprintf("No data for %s.%s", tbl, col)))
        
        col_is_num  <- is.numeric(df[[col]])
        eff_show_na <- show_na
        y_label     <- if (col_is_num) nice_name(col)
                       else if (isTRUE(as_percent)) "Proportion of Genes"
                       else "Counts of Genes"
        
        p <- auto_plot_one_var(
          con           = con,
          df            = df,
          gene_lists    = gene_lists,
          as_percent    = as_percent,
          show_na       = eff_show_na,
          col_nice_name = nice_name(col)
        )

        if (col_is_num && length(highlight_syms)) {
          gm <- gene_map(); gm$SYMB_UP <- toupper(gm$symbol)
          wanted <- gm$GeneID[gm$SYMB_UP %in% highlight_syms]
          if (length(wanted)) {
            overlay <- lapply(names(gene_lists), function(lst) {
              ids  <- gene_lists[[lst]]
              keep <- intersect(ids, wanted)
              if (!length(keep)) return(NULL)
              sub <- df[df$GeneID %in% keep, c("GeneID", col), drop = FALSE]
              sub <- sub[!is.na(sub[[col]]), , drop = FALSE]
              if (!nrow(sub)) return(NULL)
              data.frame(List = lst, Value = sub[[col]], GeneID = sub$GeneID, stringsAsFactors = FALSE)
            })
            overlay <- do.call(rbind, overlay)
            if (!is.null(overlay) && nrow(overlay)) {
              sym_map <- gene_map()[, c("GeneID","symbol")]
              overlay <- merge(overlay, sym_map, by = "GeneID", all.x = TRUE)
              p <- p %>%
                plotly::add_trace(
                  data = overlay,
                  x = ~List, y = ~Value,
                  type = "scatter", mode = "markers",
                  marker = list(color = "black", size = 6),
                  name = "Highlighted",
                  hovertemplate = paste0("%{text}<br>", nice_name(col), ": %{y}<extra>Highlighted</extra>"),
                  text = ~symbol,
                  inherit = FALSE
                )
            }
          }
        }
        
        p %>% plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = y_label)
        )
      })
    }
    
    # ---- Wire plots ----
    observe({
      gl <- lists_from(input$pick_disease)
      ap <- isTRUE(input$pct_disease)
      na <- isTRUE(input$na_disease)
      lapply(cols_disease, function(cn) {
        render_one(paste0("plot_disease_", safe_id(cn)), cn, gl, ap, na)
      })
    })
    
    observe({
      gl <- lists_from(input$pick_mouse)
      ap <- isTRUE(input$pct_mouse)
      na <- isTRUE(input$na_mouse)
      lapply(cols_mouse, function(cn) {
        render_one(paste0("plot_mouse_", safe_id(cn)), cn, gl, ap, na)
      })
    })
    
    observe({
      gl <- lists_from(input$pick_cell)
      hi <- parse_symbols(input$cell_highlight)
      lapply(cols_cell, function(cn) {
        render_one(paste0("plot_cell_", safe_id(cn)), cn, gl, TRUE, FALSE, highlight_syms = hi)
      })
    })
    
    observe({
      gl <- lists_from(input$pick_constraint)
      hi <- parse_symbols(input$constraint_highlight)
      lapply(cols_constraint, function(cn) {
        render_one(paste0("plot_constraint_", safe_id(cn)), cn, gl, TRUE, FALSE, highlight_syms = hi)
      })
    })
  })
}