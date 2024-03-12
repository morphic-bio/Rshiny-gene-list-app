box::use(
  tidyverse[...],
  dplyr[...],
  #plotly[plot_ly, layout, add_trace],
  plotly[...],
  stats[...],
  upsetjs[...],
  rrvgo[...],
  ggplot2[...],
  fst[read.fst]
)

# Rda data
box::use(
  app/logic/import_rda_data[...]
)

# Plots ----
# General ----
# Takes as input selected data, utilises list of lists with [[1]] data + [[2]] name of data
#' @export
getDataFromUserSelect <- function(selected_data, data) {
  gene_lists <- list()
  for (i in data) {
    if (i[[2]] %in% selected_data) {
      gene_lists <- append(gene_lists, list(i))
    }
  }

  return(gene_lists)
}

# Mouse model data functions ----
# Takes gene list, returns dataframe for plot
#' @export
generateImpcBarchart <- function(gene_lists, data) {

  main.annotated.data.frame <- data

  mouse_data_list <- list()
  for (i in gene_lists) {

    impc_data <- main.annotated.data.frame[main.annotated.data.frame$gene_symbol %in% i[[1]], c('mgi_id', 'impc_viability')]
    impc_plot_data <- impc_data %>%
      dplyr::filter(mgi_id != "NA") %>%
      dplyr::filter(impc_viability != "NA") %>%
      dplyr::mutate(impc_viability_2 = ifelse(!impc_viability %in% c("lethal","subviable","viable"),
                                              "conflicting", impc_viability)) %>%
      dplyr::group_by(impc_viability_2) %>%
      dplyr::tally() %>%
      dplyr::mutate(impc_viability_3 = factor(impc_viability_2,
                                              levels = c("lethal","subviable","viable"))) %>%
      dplyr::mutate(percentage = (n/sum(n)*100)) %>%
      dplyr::mutate(list_name = i[[2]])

    # Remove conflicting rows
    impc_plot_data <- impc_plot_data[impc_plot_data$impc_viability_2 != "conflicting", ]

    # Round the numeric columns to 3 decimal places
    impc_plot_data <- impc_plot_data %>%
      dplyr::mutate_at(vars('percentage'), list(~ round(., 3)))

    if (dim(impc_plot_data)[1] != 3) {
      # if true then one category has 0 genes and needs to be filled
      levels <- c('viable', 'subviable', 'lethal')
      current_rows <- impc_plot_data$impc_viability_3
      missing_rows <- levels[!levels %in% current_rows]

      # Add missing rows with a value of 0 for both 'n' and 'percentage'
      missing_data <- data.frame(impc_viability_3 = missing_rows, n = 0, percentage = 0)

      # Update impc_plot_data with the missing rows
      impc_plot_data <- bind_rows(impc_plot_data, missing_data)
    }

    mouse_data_list <- c(mouse_data_list, list(impc_plot_data))
  }

  # Combine data frames vertically
  combined_df <- bind_rows(mouse_data_list)

  plot <- plot_ly(combined_df, x = ~impc_viability_3, y = ~percentage, color = ~list_name,
                  textposition = 'outside', text = ~percentage) %>%
    layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'IMPC preweaning viability assessment'))

  return(plot)
}

# Takes impc data frame, create plotly plot
#' @export
#' generateImpcPlot <- function(impc_plot_data) {
#'   # conflicted::conflicts_prefer(plotly::layout)
#'   x_axis <- c("lethal", "subviable", "viable")
#'   data <- impc_plot_data[, c('impc_viability_3', 'percentage')]
#'   new_col_names <- c("x_axis", "percentage")
#'   colnames(data) <- new_col_names
#'
#'   impc_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
#'                        name = "EXAMPLE", textposition = 'outside', text = ~percentage) %>%
#'     plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'IMPC preweaning viability assessment'))
#'
#'   return(impc_plot)
#' }
#'
#' # Takes gene lists & impc data frames, creates plotly plot with multiple traces
#' #' @export
#' generateMultipleTracesImpcPlot <- function(plots, gene_lists_for_plots) {
#'   percentage_cols <- lapply(plots, function(plot) plot$percentage)
#'
#'   # Bind the percentage column
#'   df <- data.frame(x_axis = c("lethal", "subviable", "viable"))
#'   df <- bind_cols(df, !!!percentage_cols)
#'   # Rename the columns
#'   list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
#'
#'   # Create column names for the dataframe
#'   col_names <- c("x_axis", list_names)
#'
#'   # Assign column names to your dataframe (replace df with your actual dataframe)
#'   colnames(df) <- col_names
#'   # set y_col as first value name for initial plotly obj
#'   y_col <- names(df)[2] # first value after xaxis column
#'   y_col
#'   p <- plot_ly(df, x = ~x_axis, y = as.formula(paste0("~", y_col)),
#'                type = 'bar', name = y_col, textposition = 'outside', text = ~get(y_col)) %>%
#'     plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'IMPC preweaning viability assessment'))
#'   p
#'   # set y_cols2 for rest of value names for traces
#'   y_cols1<- names(df)[-1]
#'   y_cols2 <- y_cols1[-1]
#'   # Add traces
#'   for (i in y_cols2) {
#'     text_col <- paste0("text_", i)  # New variable for dynamic text
#'     df[[text_col]] <- df[[i]]
#'
#'     p <- p %>%
#'       add_trace(data = df, y = as.formula(paste0("~", i)), name = i, text = as.formula(paste0("~", text_col)))
#'   }
#'   # Print the resulting plot
#'   p
#' }

#' @export
generateMgiBarchart <- function(gene_lists, data) {

  main.annotated.data.frame <- data

  mouse_data_list <- list()
  for (i in gene_lists) {

    mgi_data <- main.annotated.data.frame[main.annotated.data.frame$gene_symbol %in% i[[1]], c('mgi_id', 'mgi_viability')]
    mgi_plot_data <- mgi_data %>%
      dplyr::filter(mgi_id != "NA") %>%
      dplyr::filter(mgi_viability != "NA") %>%
      dplyr::mutate(mgi_viability_2 = ifelse(!mgi_viability %in% c("lethal", "viable"),
                                              "conflicting", mgi_viability)) %>%
      dplyr::group_by(mgi_viability_2) %>%
      dplyr::tally() %>%
      dplyr::mutate(mgi_viability_3 = factor(mgi_viability_2,
                                              levels = c("lethal", "viable"))) %>%
      dplyr::mutate(percentage = (n/sum(n)*100)) %>%
      dplyr::mutate(list_name = i[[2]])

    # Remove conflicting rows
    mgi_plot_data <- mgi_plot_data[mgi_plot_data$mgi_viability_2 != "conflicting", ]

    # Round the numeric columns to 3 decimal places
    mgi_plot_data <- mgi_plot_data %>%
      dplyr::mutate_at(vars('percentage'), list(~ round(., 3)))

    if (dim(mgi_plot_data)[1] != 2) {
      # if true then one category has 0 genes and needs to be filled
      levels <- c('viable', 'lethal')
      current_rows <- mgi_plot_data$mgi_viability_3
      missing_rows <- levels[!levels %in% current_rows]

      # Add missing rows with a value of 0 for both 'n' and 'percentage'
      missing_data <- data.frame(mgi_viability_3 = missing_rows, n = 0, percentage = 0)

      # Update mgi_plot_data with the missing rows
      mgi_plot_data <- bind_rows(mgi_plot_data, missing_data)
    }

    mouse_data_list <- c(mouse_data_list, list(mgi_plot_data))
  }

  # Combine data frames vertically
  combined_df <- bind_rows(mouse_data_list)

  plot <- plot_ly(combined_df, x = ~mgi_viability_3, y = ~percentage, color = ~list_name,
                  textposition = 'outside', text = ~percentage) %>%
    layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'MGI preweaning viability assessment'))

  return(plot)
}

#' # Takes impc data frame, create plotly plot
#' #' @export
#' generateMgiPlot <- function(mgi_plot_data) {
#'   # conflicted::conflicts_prefer(plotly::layout)
#'   x_axis <- c("lethal", "viable")
#'   data <- mgi_plot_data[, c('mgi_viability_3', 'percentage')]
#'   new_col_names <- c("x_axis", "percentage")
#'   colnames(data) <- new_col_names
#'
#'   mgi_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
#'                        name = "EXAMPLE", textposition = 'outside', text = ~percentage) %>%
#'     plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'mgi preweaning viability assessment'))
#'
#'   return(mgi_plot)
#' }
#'
#' # Takes gene lists & mgi data frames, creates plotly plot with multiple traces
#' #' @export
#' generateMultipleTracesMgiPlot <- function(plots, gene_lists_for_plots) {
#'   percentage_cols <- lapply(plots, function(plot) plot$percentage)
#'
#'   # Bind the percentage column
#'   df <- data.frame(x_axis = c("lethal", "viable"))
#'   df <- bind_cols(df, !!!percentage_cols)
#'   # Rename the columns
#'   list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
#'
#'   # Create column names for the dataframe
#'   col_names <- c("x_axis", list_names)
#'
#'   # Assign column names to your dataframe (replace df with your actual dataframe)
#'   colnames(df) <- col_names
#'   # set y_col as first value name for initial plotly obj
#'   y_col <- names(df)[2] # first value after xaxis column
#'   y_col
#'   p <- plot_ly(df, x = ~x_axis, y = as.formula(paste0("~", y_col)),
#'                type = 'bar', name = y_col, textposition = 'outside', text = ~get(y_col)) %>%
#'     plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'MGI preweaning viability assessment'))
#'   p
#'   # set y_cols2 for rest of value names for traces
#'   y_cols1<- names(df)[-1]
#'   y_cols2 <- y_cols1[-1]
#'   # Add traces
#'   for (i in y_cols2) {
#'     text_col <- paste0("text_", i)  # New variable for dynamic text
#'     df[[text_col]] <- df[[i]]
#'
#'     p <- p %>%
#'       add_trace(data = df, y = as.formula(paste0("~", i)), name = i, text = as.formula(paste0("~", text_col)))
#'   }
#'   # Print the resulting plot
#'   p
#' }


# disease plots ----
#' @export
generateHasOmimPlot <- function(gene_lists, data) {

  main.annotated.data.frame <- data

  omim_data_list <- list()
  for (i in gene_lists) {

    omim_data <- main.annotated.data.frame[main.annotated.data.frame$gene_symbol %in% i[[1]], c('hgnc_id', 'omim_phenotype')]

    omim_plot_data <- omim_data %>%
      mutate(has_omim_phenotype = if_else(!is.na(omim_phenotype), "yes", "no"))

    omim_summary_data <- omim_plot_data %>%
      group_by(has_omim_phenotype) %>%
      summarize(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100) %>%
      dplyr::mutate(list_name = i[[2]])

    # Round the numeric columns to 3 decimal places
    omim_summary_data <- omim_summary_data %>%
      mutate_at(vars('percentage'), list(~ round(., 3)))

    if (dim(omim_summary_data)[1] != 2) {
      # if true then one category has 0 genes and needs to be filled
      levels <- c('yes', 'no')
      current_rows <- omim_summary_data$has_omim_phenotype
      missing_rows <- levels[!levels %in% current_rows]

      # Add missing rows with a value of 0 for both 'n' and 'percentage'
      missing_data <- data.frame(has_omim_phenotype = missing_rows, n = 0, percentage = 0)

      # Update impc_plot_data with the missing rows
      omim_summary_data <- bind_rows(omim_summary_data, missing_data)
    }

    omim_data_list <- c(omim_data_list, list(omim_summary_data))
  }

  # Combine data frames vertically
  combined_df <- bind_rows(omim_data_list)

  plot <- plot_ly(combined_df, x = ~has_omim_phenotype, y = ~percentage, color = ~list_name,
                  textposition = 'outside', text = ~percentage) %>%
    layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Mendelian disease association (OMIM)'))

  return(plot)
}

#' @export
#' generateHasOmimPlot <- function(omim_summary_data) {
#'   # conflicted::conflicts_prefer(plotly::layout)
#'   x_axis <- c("yes", "no")
#'   data <- omim_summary_data[, c('has_omim_phenotype', 'percentage')]
#'   new_col_names <- c("x_axis", "percentage")
#'   colnames(data) <- new_col_names
#'
#'   omim_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
#'                        name = "EXAMPLE", textposition = 'outside', text = ~percentage) %>%
#'     plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Mendelian disease association (OMIM)'))
#'
#'   return(omim_plot)
#' }
#'
#' #' @export
#' generateMultipleTracesHasOmimPlot <- function(plots, gene_lists_for_plots) {
#'   percentage_cols <- lapply(plots, function(plot) plot$percentage)
#'
#'   # Bind the percentage column
#'   df <- data.frame(x_axis = c("yes", "no"))
#'   df <- bind_cols(df, !!!percentage_cols)
#'   # Rename the columns
#'   list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
#'
#'   # Create column names for the dataframe
#'   col_names <- c("x_axis", list_names)
#'
#'   # Assign column names to your dataframe (replace df with your actual dataframe)
#'   colnames(df) <- col_names
#'   # set y_col as first value name for initial plotly obj
#'   y_col <- names(df)[2] # first value after xaxis column
#'   y_col
#'   p <- plot_ly(df, x = ~x_axis, y = as.formula(paste0("~", y_col)),
#'                type = 'bar', name = y_col, textposition = 'outside', text = ~get(y_col)) %>%
#'     plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Mendelian disease association (OMIM)'))
#'   p
#'   # set y_cols2 for rest of value names for traces
#'   y_cols1<- names(df)[-1]
#'   y_cols2 <- y_cols1[-1]
#'   # Add traces
#'   for (i in y_cols2) {
#'     text_col <- paste0("text_", i)  # New variable for dynamic text
#'     df[[text_col]] <- df[[i]]
#'
#'     p <- p %>%
#'       add_trace(data = df, y = as.formula(paste0("~", i)), name = i, text = as.formula(paste0("~", text_col)))
#'   }
#'   # Print the resulting plot
#'   p
#' }

#' @export
generateOmimLethalityPlot <- function(gene_lists, data) {

  main.annotated.data.frame <- data

  omim_data_list <- list()
  for (i in gene_lists) {

    omim_lethality_data <- main.annotated.data.frame[main.annotated.data.frame$gene_symbol %in% i[[1]], c('hgnc_id', 'omim_gene_lethality')]

    omim_summary_data <- omim_lethality_data %>%
      dplyr::filter(!is.na(omim_gene_lethality)) %>%
      dplyr::filter(omim_gene_lethality %in% c("lethal", "nonlethal")) %>%
      dplyr::group_by(omim_gene_lethality) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::mutate(percentage = (count / sum(count)) * 100) %>%
      dplyr::mutate(list_name = i[[2]])

    # Round the numeric columns to 3 decimal places
    omim_lethality_summary_data <- omim_summary_data %>%
      dplyr::mutate_at(vars('percentage'), list(~ round(., 3)))

    if (dim(omim_lethality_summary_data)[1] != 2) {
      # if true then one category has 0 genes and needs to be filled
      levels <- c("lethal", "nonlethal")
      current_rows <- omim_lethality_summary_data$omim_gene_lethality
      missing_rows <- levels[!levels %in% current_rows]

      # Add missing rows with a value of 0 for both 'n' and 'percentage'
      missing_data <- data.frame(omim_gene_lethality = missing_rows, n = 0, percentage = 0)

      # Update impc_plot_data with the missing rows
      omim_lethality_summary_data <- bind_rows(omim_lethality_summary_data, missing_data)
    }

    omim_data_list <- c(omim_data_list, list(omim_lethality_summary_data))
  }
  # Combine data frames vertically
  combined_df <- bind_rows(omim_data_list)

  plot <- plot_ly(combined_df, x = ~omim_gene_lethality, y = ~percentage, color = ~list_name,
                  textposition = 'outside', text = ~percentage) %>%
    layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Lethal Phenotypes (OMIM)'))

  return(plot)
}

#' #' @export
#' generateOmimLethalityPlot <- function(omim_lethality_summary_data) {
#'   # conflicted::conflicts_prefer(plotly::layout)
#'   x_axis <- c("lethal", "nonlethal")
#'   data <- omim_lethality_summary_data[, c('omim_gene_lethality', 'percentage')]
#'   new_col_names <- c("x_axis", "percentage")
#'   colnames(data) <- new_col_names
#'
#'   omim_lethality_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
#'                                  name = "Example", textposition = 'outside', text = ~percentage) %>%
#'     plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Lethal Phenotypes (OMIM)'))
#'
#'   return(omim_lethality_plot)
#' }
#'
#' #' @export
#' generateMultipleTracesOmimLethalityPlot <- function(plots, gene_lists_for_plots) {
#'   percentage_cols <- lapply(plots, function(plot) plot$percentage)
#'
#'   # Bind the percentage column
#'   df <- data.frame(x_axis = c("lethal", "nonlethal"))
#'   df <- bind_cols(df, !!!percentage_cols)
#'   # Rename the columns
#'   list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
#'
#'   # Create column names for the dataframe
#'   col_names <- c("x_axis", list_names)
#'
#'   # Assign column names to your dataframe (replace df with your actual dataframe)
#'   colnames(df) <- col_names
#'   # set y_col as first value name for initial plotly obj
#'   y_col <- names(df)[2] # first value after xaxis column
#'   y_col
#'   p <- plot_ly(df, x = ~x_axis, y = as.formula(paste0("~", y_col)),
#'                type = 'bar', name = y_col, textposition = 'outside', text = ~get(y_col)) %>%
#'     plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Lethal Phenotypes (OMIM)'))
#'   p
#'   # set y_cols2 for rest of value names for traces
#'   y_cols1<- names(df)[-1]
#'   y_cols2 <- y_cols1[-1]
#'   # Add traces
#'   for (i in y_cols2) {
#'     text_col <- paste0("text_", i)  # New variable for dynamic text
#'     df[[text_col]] <- df[[i]]
#'
#'     p <- p %>%
#'       add_trace(data = df, y = as.formula(paste0("~", i)), name = i, text = as.formula(paste0("~", text_col)))
#'   }
#'   # Print the resulting plot
#'   p
#' }

#' #' @export
#' constraintMetricsPlots <- function(gene_lists_for_plots, meta_data, metric_col_name, x_axis_text) {
#'
#'   main.annotated.data.frame <- meta_data
#'   # Get list of tibbles with gene_symbol & corresponding metric value
#'   metrics_data_list <- list()
#'   for (i in gene_lists_for_plots) {
#'     metrics_data <- main.annotated.data.frame[main.annotated.data.frame$gene_symbol %in% (i[[1]]), c('gene_symbol', metric_col_name)]
#'     metrics_data_list <- c(metrics_data_list, list(metrics_data))
#'   }
#'   # Combine into one df with one column for each gene list
#'   df <- purrr::reduce(metrics_data_list, full_join, by = "gene_symbol")
#'
#'   # Assign new column names
#'   # Extract the list names from gene_lists_for_plots
#'   list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
#'   col_names <- c("x_axis", list_names)
#'   colnames(df) <- col_names
#'
#'   # set y_col as first value name for initial plotly obj
#'   y_col <- names(df)[2] # first value after xaxis column
#'   # Generate plot
#'   p <- plot_ly(df,
#'                y = as.formula(paste0("~", y_col)),
#'                x = y_col,
#'                name = y_col,
#'                type = "violin",
#'                box = list(visible = T),
#'                hoverinfo = "text",
#'                hovertext = paste("Gene Symbol: ", df$x_axis, "<br>", x_axis_text, df[[y_col]])
#'                ) %>%
#'     plotly::layout(yaxis = list(title = x_axis_text))
#'
#'   # set y_cols2 for rest of value names for traces
#'   y_cols1<- names(df)[-1]
#'   y_cols2 <- y_cols1[-1]
#'   # Add traces
#'   for (i in y_cols2) {
#'     text_col <- paste0("text_", i)  # New variable for dynamic text
#'     df[[text_col]] <- df[[i]]
#'
#'     p <- p %>%
#'       add_trace(data = df, y = as.formula(paste0("~", i)), x = i, name = i, text = as.formula(paste0("~", text_col)),
#'                 hoverinfo = "text", hovertext = paste("Gene Symbol: ", df$x_axis, "<br>", x_axis_text, df[[i]]))
#'   }
#'   # Print the resulting plot
#'   return(p)
#' }
#'
#' # Gene search highlight data point
#' #' @export
#' addGeneTrace <- function(meta_data, plot, gene, col, x_axis_text) {
#'   print("received gene input:")
#'   print(gene)
#'   for (i in plot$x$attrs) {
#'     plot <- plot %>%
#'       add_trace(
#'         y = meta_data[[col]][meta_data$gene_symbol == gene],
#'         x = i$x,
#'         name = gene,
#'         hoverinfo = "text",
#'         hovertext = paste("Gene Symbol: ", gene, "<br>", x_axis_text,
#'                           meta_data[[col]][meta_data$gene_symbol == gene]),
#'         type = "scatter",
#'         mode = 'markers'
#'       )
#'   }
#'   return(plot)
#' }

# Threshold line
#' @export
hline <- function(y = 0, color = "grey") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(dash = "dash", color = color)
  )
}

#' @export
getViolinPlotData <- function(data, column, gene_lists) {

  # Get list of tibbles with gene_symbol & corresponding metric value
  all_data_df <- data.frame()

  for (i in gene_lists) {
    gene_list_name <- i[[2]]
    metrics_data <- data[data$gene_symbol %in% (i[[1]]), c('gene_symbol', column)] # gene symbol + corresponding metric value

    # Add gene_list_name column
    metrics_data$gene_list_name <- gene_list_name

    # Bind rows to the existing dataframe
    all_data_df <- rbind(all_data_df, metrics_data)
  }

  return(all_data_df)
}

#' @export
renderViolinPlot <- function(data, column, genes_to_highlight, threshold_value, toggle_option, custom_y_axis_title) {
  # print(option)
  if (toggle_option == TRUE) {
    points_setting <- "all"
  } else {
    points_setting <- "none"
  }

  # Your existing code for creating the violin plots
  violin_plot <- plot_ly(
    data,
    y = as.formula(paste("~", column)),
    x = ~gene_list_name,
    type = "violin",
    box = list(visible = T),
    points = points_setting,
    name = ~gene_list_name,
    text = ~paste("Gene: ", gene_symbol, "<br>", column, ": ", get(column)),
    hoverinfo = "text"  # Include gene symbol and metric value in hover text
  )

  # Add highlight points for individual genes
  if (length(genes_to_highlight > 0)) {
    violin_plot <- violin_plot %>%
      highlight("plotly_selecting") %>%
      # Add points for highlighting
      add_trace(
        data = data[data$gene_symbol %in% genes_to_highlight, ],  # Only include specified genes
        type = "scatter",
        mode = "markers",
        x = ~gene_list_name,
        y = as.formula(paste("~", column)),
        text = ~paste("Gene: ", gene_symbol, "<br>", column, ": ", get(column)),
        marker = list(color = "black", size = 10),
        hoverinfo = "text",
        name = "Searched Genes"  # Legend entry for the added trace
      )
  }

  # Remove x-axis title
  violin_plot <- violin_plot %>%
    layout(
    xaxis = list(title = ""),
    yaxis = list(title = custom_y_axis_title),
    showlegend = FALSE
    )

  if (!is.null(threshold_value)) {
    violin_plot <- violin_plot %>%
      layout(
        shapes = list(hline(threshold_value))
      )
  }

  violin_plot %>% toWebGL()

  return(violin_plot)
}

# Panther plots
# Issue: can't use column in the ~reorder
#' @export
getPantherPlots <- function(data, gene_lists) {

  all_data_list <- list()

  for (i in gene_lists) {
    gene_list_name <- i[[2]]
    all_data_list <- append(
      all_data_list,
      list(
        list(
          data %>%
            dplyr::filter(!is.na(class_term)) %>%
            dplyr::filter(gene_symbol %in% i[[1]]) %>%
            dplyr::select(class_term) %>%
            dplyr::group_by(class_term) %>%
            dplyr::tally() %>%
            dplyr::arrange(desc(n)) %>%
            dplyr::slice_head(n = 10),
          gene_list_name
          )
        )
      )
  }

  plot_list <- list()
  for (i in all_data_list) {
    p <- i[[1]] %>%
      plot_ly(
        type = 'bar',
        x = ~reorder(class_term, n),
        y = ~n,
        name = i[[2]]
      )

    plot_list <- append(plot_list, list(p))
  }

  subplots <- subplot(plot_list) %>%
    layout(
      title = 'Top 10 Protein Class Terms'
    )

  return(subplots)
}

#' @export
generateUpsetR <- function(gene_lists) {

  input <- list()

  for (i in gene_lists) {
    input[[i[[2]]]] <- i[[1]]
  }

  upsetjs() %>%
    fromList(input) %>%
    interactiveChart()
}

# Gene Ontology Enriched terms + plots
# Semantic similarity analysis
#' @export
getEnrichedGoTermsMultipleInput <- function(gene_lists, background) {

  list_of_enrichGO_objs <- list()
  for (i in gene_lists) {
    go_analysis <- clusterProfiler::enrichGO(gene          = i[[1]],
                            universe      = background,
                            keyType = "SYMBOL",
                            OrgDb         = "org.Hs.eg.db",
                            ont           = "BP",
                            pAdjustMethod = "BH",
                            pvalueCutoff  = 0.01,
                            qvalueCutoff  = 0.05,
                            readable      = TRUE)

    #list_of_enrichGO_objs[[i[[2]]]] <- go_analysis
    list_of_enrichGO_objs <- append(list_of_enrichGO_objs, list(
      list(go_analysis, i[[2]])
    ))
  }

  list_of_enrichGO_objs
}

#' @export
getEnrichedGoTerms <- function(gene_list, background) {

  go_analysis <- clusterProfiler::enrichGO(gene          = gene_list[[1]],
                          universe      = background,
                          keyType = "SYMBOL",
                          OrgDb         = "org.Hs.eg.db",
                          ont           = "BP",
                          pAdjustMethod = "BH",
                          pvalueCutoff  = 0.01,
                          qvalueCutoff  = 0.05,
                          readable      = TRUE)
}

#' @export
generateGoSemanticSimilarityPlot <- function(go_analysis) {
  simMatrix <- rrvgo::calculateSimMatrix(go_analysis$ID,
                                  orgdb="org.Hs.eg.db",
                                  ont="BP",
                                  method="Rel")

  scores <- setNames(-log10(go_analysis$qvalue), go_analysis$ID)
  reducedTerms <- rrvgo::reduceSimMatrix(simMatrix,
                                  scores,
                                  threshold=0.7,
                                  orgdb="org.Hs.eg.db")

  scat_p <- scatterPlot(simMatrix, reducedTerms)

  # Interactive plots
  ggplotly(scat_p)
}

#' @export
renderGoScatterPlot <- function(plots_list, ontology, selected_dpc, show_legend) {

  p <- plots_list[[ontology]][[selected_dpc]]

  if (show_legend == FALSE) {
    p <- p + theme(legend.position='none')
  }

  return(p)
}

#' @export
renderGoTable <- function(tables_list, ontology, selected_dpc) {
  table <- tables_list[[ontology]][[selected_dpc]]
  top_enriched_terms <- table %>%
    top_n(-10)
}

#' @export
renderReactomeEnrichmentPlot <- function(plots_list, selected_dpc) {

  p <- plots_list[[selected_dpc]]

  return(p)
}

#' @export
renderReactomeTable <- function(table_list, selected_dpc) {

  table <- table_list[[selected_dpc]]

  return(table)
}
