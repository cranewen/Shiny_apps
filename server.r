library(DT)
library(shiny)
library(dplyr)
library(clipr)
library(plotly)
library(heatmaply)
library(rclipboard)
library(dendextend)
library(shinyalert)
library(htmlwidgets)
library(colourpicker)
library(RColorBrewer)
library(shinydashboard)
# options(shiny.host = '10.251.99.213')
# options(shiny.host = '172.24.65.161')
# options(shiny.port = 3030)

shinyServer(function(input, output, session){
   
  `%ucin%` <- function(a, b) {toupper(a) %in% toupper(b)}
  
  # Set Scatter plot variables
  df <- NULL
  column_names <- NULL
  highlight_list <- NULL
  observeEvent(input$select_dataset_scatter, {
    scatterfile <- input$scatter_file
    scatter_df <<- read.csv(scatterfile$datapath, header = TRUE, sep = ",")
    View(scatter_df)
    
    sample_names <<- colnames(scatter_df)[colnames(scatter_df) != 'Gene']
    updateSelectInput(session, "dataset_col1", label = "Sample 1 (X axis)", choices = sample_names)
    updateSelectInput(session, "dataset_col2", label = "Sample 2 (Y axis)", choices = sample_names)
  })
  
  
  colorpick <- eventReactive(input$clickscatter_highlight, {
    c(input$color1, input$color2)
  })

  colorpick_gene <- eventReactive(input$clickscatter_highlight_gene, {
    c(input$color1, input$color2)
  })

  observeEvent(input$clickscatter, {
    df_col1 <- scatter_df[[input$dataset_col1]]
    df_col2 <- scatter_df[[input$dataset_col2]]
    hovertext <- scatter_df[["Gene"]]

    # r squared value of two columns of data
    r2 <- cor(df_col1, df_col2) ^ 2
    r2 = format(round(r2, 2), nsmall = 2)

    output$scatterplot <- renderPlotly({
      updateCheckboxInput(session, inputId = "scatter_r2") 
      if(input$scatter_r2 == TRUE){
        p <- plot_ly(data = scatter_df, type = "scatter", mode = "markers", x = df_col1, y = df_col2, customdata = scatter_df$Gene,
                     text = hovertext, hoverinfo = text,
                     color = df_col1, source = "scatter_sub") %>%
            layout(
                   xaxis = list(title = input$dataset_col1),
                   yaxis = list(title = input$dataset_col2)) %>%
            config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height),
                   edits = list(
                     annotationPosition = TRUE,
                     annotationTail = TRUE,
                     annotationText = TRUE
                   )) %>% 
            add_annotations(x = max(df_col1) / 10, y = max(df_col2), text = TeX(sprintf(r'($R^2 = %s)', r2)), 
                            showarrow = FALSE, font=list(size=input$r2_size, color=input$r2_color))
      }else{
         p <- plot_ly(data = scatter_df, type = "scatter", mode = "markers", x = df_col1, y = df_col2, customdata = scatter_df$Gene,
                     text = hovertext, hoverinfo = text,
                     color = df_col1, source = "scatter_sub") %>%
            layout(
                   xaxis = list(title = input$dataset_col1),
                   yaxis = list(title = input$dataset_col2)) %>%
            config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height),
                   edits = list(
                     annotationPosition = TRUE,
                     annotationTail = TRUE,
                     annotationText = TRUE
                   ))       
      }

      p
    })
    
    output$lasso_gene_dt_table <- renderUI({ DTOutput("lasso_gene")})

    output$lasso_gene <- renderDataTable({
      lasso_data <- event_data("plotly_selected", source = "scatter_sub")
      lasso_df <- lasso_data["customdata"]
      if (!is.null(lasso_df)) {
        names(lasso_df)[names(lasso_df) == "customdata"] <- "Gene"
      }
      datatable(lasso_df,
                extensions = c("Buttons", "Scroller", "FixedColumns"),
                options = list(dom = 'frtipB', autoWidth = TRUE,
                               scrollX = TRUE, scrollY = 500,
                               scroller = TRUE,
                               fixedColumns = list(leftColumns = 2),
                               buttons = c('copy', 'csv', 'excel')),
                )

          }, server = FALSE)

    output$copybutton <- renderUI({
      if (is.null(event_data("plotly_selected", source = "scatter_sub"))) {
        actionButton("geneclipbutton", label = "Add Highlighted Genes", icon = icon("clipboard"))
        } 
      else {
        actionButton("geneclipbutton", label = "Add Highlighted Genes", icon = icon("clipboard"), class = "btn-warning")
        }
      })

    })
  
  lasso_history <- c()
  
                           
  observeEvent(input$geneclipbutton, {
    scatter_sub_data <- event_data("plotly_selected", source = "scatter_sub")
    gene_list <- scatter_sub_data$customdata
    # convert gene_list into a string, so the textArea can use it
    lasso_gene_str <- paste(paste0(gene_list, collapse = "\n"), "\n", sep = "")
    if (!is.null(gene_list)) {
      lasso_history <<- c(lasso_history, lasso_gene_str)
      lasso_history <<- unique(lasso_history)
    }
    if (!is.null(lasso_history)) {
      output$copy_history_slider <- renderUI({
        sliderInput("history_slider", label = "View Highlight Batches", min = 1, max = length(lasso_history), 
                    value = c(1, length(lasso_history)), step = 1)
      })
    }
    updateTextAreaInput(session, "scatter_gene_input", value = paste(input$scatter_gene_input, lasso_gene_str, sep = ""))
  })
  
  observeEvent(input$history_slider, {
    lasso_history_range <- c(input$history_slider[1], input$history_slider[2])
    lasso_range_list <- lasso_history[lasso_history_range[1]:lasso_history_range[2]]
    updateTextAreaInput(session, "scatter_gene_input", value = paste0(lasso_range_list, collapse = ""))
  })

  observeEvent(input$clickscatter_highlight_gene, {
    gene_input_list <- unique(strsplit(input$scatter_gene_input, "\n")[[1]])
    gene_input_list <- gene_input_list[gene_input_list != ""]
    gene_selectizeinput_list <- input$highlight_scatter
    combined_gene <<- unique(c(gene_input_list, gene_selectizeinput_list))
    combined_gene_len <<- length(combined_gene)


    df_color <<- scatter_df %>% mutate(color_col = ifelse(scatter_df$Gene %in% combined_gene, "interesting", "uninteresting"))

    df_x <- df_color[[input$dataset_col1]]
    df_y <- df_color[[input$dataset_col2]]
    color_list <- colorpick_gene()

    output$scatterplot <- renderPlotly({
      p <- plot_ly(data = df_color, type = "scatter", mode = "markers",
                   x = df_x, y = df_y, customdata = df_color$Gene,
                   text = df_color$Gene, hoverinfo = text,
                   color = df_color$color_col, colors = c(color_list), source = "scatter_sub_gene") %>%
        layout(
               xaxis = list(title = input$dataset_col1),
               yaxis = list(title = input$dataset_col2),
               annotations =
                 list(x = df_color[which(df_color$color_col == "interesting"),][[input$dataset_col1]],
                      y = df_color[which(df_color$color_col == "interesting"),][[input$dataset_col2]],
                      text = df_color[which(df_color$color_col == "interesting"),][["Gene"]],
                      # set the data point on the right of the text
                      font = list(size = 16),
                      xanchor = 'right',
                      showarrow = input$scatter_arrow, arrowhead = 2, arrowsize = 1)
        ) %>% config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height),
                     edits = list(
                       annotationPosition = TRUE,
                       annotationTail = TRUE,
                       annotationText = TRUE
                     ))
      p

    })

    output$lasso_gene_dt_table <- renderUI({ DTOutput("lasso_gene_highlight") })
    

    output$lasso_gene_highlight <- renderDataTable({
      lasso_data_gene <- event_data("plotly_selected", source = "scatter_sub_gene")
      lasso_df_gene <<- df_color[which(df_color$Gene %in% lasso_data_gene$customdata), ]["Gene"]
      datatable(lasso_df_gene,
                extensions = c("Buttons", "Scroller", "FixedColumns"),
                options = list(dom = 'frtipB', autoWidth = TRUE,
                               scrollX = TRUE, scrollY = 500,
                               scroller = TRUE,
                               fixedColumns = list(leftColumns = 2),
                               buttons = c('copy', 'csv', 'excel')),
                )

          }, server = FALSE)
    
    output$gene_table <- renderDataTable({
      lasso_data_gene <- event_data("plotly_selected", source = "scatter_sub_gene")
      lasso_df_gene <<- df_color[which(df_color$Gene %in% lasso_data_gene$customdata), ]["Gene"]
      datatable(lasso_df_gene,
                extensions = c("Buttons", "Scroller", "FixedColumns"),
                options = list(dom = 'frtipB', autoWidth = TRUE,
                               scrollX = TRUE, scrollY = 500,
                               scroller = TRUE,
                               fixedColumns = list(leftColumns = 2),
                               buttons = c('copy', 'csv', 'excel')),
                )

          }, server = FALSE)

    
    
    output$copybutton <- renderUI({
      if (is.null(event_data("plotly_selected", source = "scatter_sub_gene"))) {
        actionButton("geneclipbutton_gene", label = "Add Highlighted Genes", icon = icon("clipboard"))
      } else {
        actionButton("geneclipbutton_gene", label = "Add Highlighted Genes", icon = icon("clipboard"), class = "btn-success")
      }
    })

    output$copybutton_all <- renderUI({
      actionButton("geneclipbutton_gene_all", label = "Copy Highlighted Genes", icon = icon("clipboard"), 
                   style = "color: #fff; background-color: #337ab7;")
    })
    

    output$selected_dt <- renderDataTable({
      combined_df <<- df_color[which(df_color$Gene %in% combined_gene),]
      datatable(combined_df,
                extensions = c("Buttons", "Scroller", "FixedColumns"),
                options = list(dom = 'frtipB', autoWidth = TRUE,
                               scrollX = TRUE, scrollY = 500,
                               scroller = TRUE,
                               fixedColumns = list(leftColumns = 2),
                               buttons = c('copy', 'csv', 'excel')),
      )
    }, server = FALSE)
    
    output$gene_table <- renderDataTable({
      combined_df <<- df_color[which(df_color$Gene %in% combined_gene),]
      datatable(combined_df,
                extensions = c("Buttons", "Scroller", "FixedColumns"),
                options = list(dom = 'frtipB', autoWidth = TRUE,
                               scrollX = TRUE, scrollY = 500,
                               scroller = TRUE,
                               fixedColumns = list(leftColumns = 2),
                               buttons = c('copy', 'csv', 'excel')),
      )
    }, server = FALSE)

    # clipbutton for all highlighted genes from two input areas
    # it works after drawing the scatter plot while all the highlights are showing on the plot
    clip_all_str <- paste(df_color[which(df_color$color_col == "interesting"),][["Gene"]], collapse = "\n") 
    output$clipbutton_all <- renderUI({
      rclipButton("clip_all", "Copy Current Genes List", clip_all_str, icon = icon("clipboard"))
    })

  })

  
  # copy lasso gene list from scatter highlight plot -- copybutton
  observeEvent(input$geneclipbutton_gene, {
    # convert lasso gene list into a string with \n as separator 
    lasso_gene_str <- paste(paste0(lasso_df_gene$Gene, collapse = "\n"), "\n", sep = "")
    output$clipbutton <- renderUI(rclipButton("clip", "Copy Genes", lasso_gene_str, icon = icon("clipboard")))
    if (!is.null(lasso_gene_str) && lasso_gene_str != "\n") {
      lasso_history <<- c(lasso_history, lasso_gene_str)
      lasso_history <<- unique(lasso_history)
    }
    if (!is.null(lasso_history)) {
      output$copy_history_slider <- renderUI({
        sliderInput("history_slider", label = "View Highlight Batches", min = 1, max = length(lasso_history),
                    value = c(1, length(lasso_history)), step = 1)
      })
    }
    updateTextAreaInput(session, "scatter_gene_input", value = paste(input$scatter_gene_input, lasso_gene_str, sep = ""))
  })
  
  # copy lasso gene list from scatter highlight plot -- copybutton_all
  observeEvent(input$geneclipbutton_gene_all, {
    #write_clip(df_color[which(df_color$color_col == "interesting"),][["Gene"]], object_type = c("auto"), breaks = "\n")
  })
  
  
  # Heatmap
  
  # Set dataframe for heatmap
  dfheatmap <- NULL
  
  observeEvent(input$select_dataset_heatmap, {
    heatmapfile <- input$heatmap_file
    
    dfheatmap <<- read.csv(heatmapfile$datapath, header = TRUE, sep = ",")
    sample_names <<- colnames(dfheatmap)[colnames(dfheatmap) != 'Gene']
    updateSelectInput(session, "heatmap_col_input", label = "Select Samples", choices = sample_names)
  })
 
  
  observeEvent(input$clickheatmap, {
    req(input$heatmap_gene_input)
    heatmap_gene_list <- unique(strsplit(input$heatmap_gene_input, "\n")[[1]])
    heatmap_gene_list <- heatmap_gene_list[heatmap_gene_list != ""]
    df_heatmap_matrix <- NULL
    df_heatmap_matrix_sample <- NULL
    df_heatmap <- dfheatmap[which(heatmap_gene_list %ucin% dfheatmap$Gene),]
    # sort heatmap by the input order
    df_heatmap <- dfheatmap[match(tolower(heatmap_gene_list), tolower(dfheatmap$Gene)),]
    df_heatmap <- df_heatmap[which(!is.na(df_heatmap$Gene)),]
    # sort heatmap by alphabetic(ascii)
    df_heatmap_matrix <- df_heatmap[, -1]
    gene_error_list <- heatmap_gene_list[which(!heatmap_gene_list %ucin% dfheatmap$Gene)]
    gene_error_list <- gene_error_list[gene_error_list != ""]
    if (length(gene_error_list) > 0) {
      shinyalert(title = "You have entered some genes are not in the data!", text = paste0("There are ", length(gene_error_list)," unmatched genes"), type = "error")
      updateTextAreaInput(session, "heatmap_gene_error_list", value = paste0(gene_error_list, collapse = "\n" ))
    }
    
    rownames(df_heatmap_matrix) <- df_heatmap[, 1]
    df_heatmap_matrix <- as.matrix(df_heatmap_matrix)
    
    # Cosmetic variables
    title_style <- list(family = "sans serif", size = 35)
    
    if (is.null(input$heatmap_col_input)) {
      df_heatmap <- dfheatmap[match(tolower(heatmap_gene_list), tolower(dfheatmap$Gene)),]
      df_heatmap <- df_heatmap[which(!is.na(df_heatmap$Gene)),]
      df_heatmap_matrix <- df_heatmap[, -1]
      rownames(df_heatmap_matrix) <- df_heatmap[, 1]
      df_heatmap_matrix <- as.matrix(df_heatmap_matrix)
      
    } else {
      df_heatmap <- dfheatmap[match(tolower(heatmap_gene_list), tolower(dfheatmap$Gene)),][c("Gene", input$heatmap_col_input)]
      df_heatmap <- df_heatmap[which(!is.na(df_heatmap$Gene)),]
      df_heatmap_matrix_sample <- df_heatmap[, -1]
      rownames(df_heatmap_matrix_sample) <- df_heatmap$Gene
      df_heatmap_matrix_sample <- as.matrix(df_heatmap_matrix_sample)
    }
    
    heatmap_title <- "Heatmap - Selected Samples"
    if (!is.null(input$heatmap_title)) {
      heatmap_title <- input$heatmap_title
    }
    
    
    if(input$heatmap_cluster_checkbox == TRUE) {
      # set clustering heatmap, so we can use it for download
      p_heatmap_cluster <- ""
      output$heatmap_sample <- renderPlotly({
        if (input$heatmap_text) {
          p_heatmap_cluster <<- heatmaply(df_heatmap_matrix_sample, main = heatmap_title, dendrogram = input$show_cluster, 
                    colors = colorRampPalette(brewer.pal(3, input$heatmap_color))(256), cellnote = df_heatmap_matrix_sample)
          p_heatmap_cluster %>% config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
        } else {
            if (input$heatmap_color == "viridis") {
              p_heatmap_cluster <<- heatmaply(df_heatmap_matrix_sample, main = heatmap_title, dendrogram = input$show_cluster)
              p_heatmap_cluster %>% config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
            } else {
              p_heatmap_cluster <<- heatmaply(df_heatmap_matrix_sample, main = heatmap_title, dendrogram = input$show_cluster, 
                        colors = colorRampPalette(brewer.pal(3, input$heatmap_color))(256))
              p_heatmap_cluster %>% config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
            }
        }
      })
      
    }else {
      if (input$heatmap_color_max == 0 && input$heatmap_color_min == 0) {
        p <- ""
        output$heatmap_sample <- renderPlotly({
          row_dim <- dim(df_heatmap_matrix_sample)[1]
          col_dim <- dim(df_heatmap_matrix_sample)[2]
          p <<- plot_ly(x = colnames(df_heatmap_matrix_sample), y = rownames(df_heatmap_matrix_sample), z = df_heatmap_matrix_sample, type = "heatmap", 
                       colors = input$heatmap_color, zmin = NULL, zmax = NULL) %>% 
            config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))       
          p
          if (input$heatmap_text == TRUE) {
            # x and y are paird as c(x,y) for determining the location in the x-y coordinator, and taking single value from "text = "
            p <<- p %>% add_annotations(x = rep(c(0:(col_dim-1)), each=row_dim),
                                       y = rep(c(0:(row_dim-1)), col_dim),
                                       text = df_heatmap_matrix_sample, xref = "x", yref = "y", showarrow = FALSE, font=list(color="white")) %>% 
              config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
            p
          }
          p <<- p %>% layout(title = heatmap_title, yaxis = list(autorange="reversed"), margin = 10, titleFont = title_style) %>% 
            config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
          p
        })
        
        
        p_all_sample_heatmap <- ""
        output$heatmap <- renderPlotly({
          p_all_sample_heatmap <<- plot_ly(x = colnames(df_heatmap_matrix), y = rownames(df_heatmap_matrix), z = df_heatmap_matrix, type = "heatmap", 
                  colors = input$heatmap_color, zmin = NULL, zmax = NULL) %>%
            layout(title = "Heatmap - All the Samples", yaxis = list(autorange="reversed")) %>% 
            config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
          p_all_sample_heatmap
        })
        
      }
      if (input$heatmap_color_max != 0 && input$heatmap_color_min != 0) {
        p <- ""
        output$heatmap_sample <- renderPlotly({
          row_dim <- dim(df_heatmap_matrix_sample)[1]
          col_dim <- dim(df_heatmap_matrix_sample)[2]
          p <<- plot_ly(x = colnames(df_heatmap_matrix_sample), y = rownames(df_heatmap_matrix_sample), z = df_heatmap_matrix_sample, type = "heatmap", 
                       colors = input$heatmap_color, zmin = input$heatmap_color_min, zmax = input$heatmap_color_max) %>%
               layout(title = heatmap_title, yaxis = list(autorange="reversed"), margin = 10, titleFont = title_style) %>% 
            config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
          
          if (input$heatmap_text == TRUE) {
            p <<- p %>% add_annotations(x = rep(c(0:(col_dim-1)), each=row_dim),
                                       y= rep(c(0:(row_dim-1)), col_dim),
                                       text = df_heatmap_matrix_sample, xref = "x", yref = "y", showarrow = FALSE, font=list(color="white")) %>% 
              config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
          }
          p
        })
        
        
        # because in all samples' heatmap, the scale is different, so we need to check if the input min and max exceed the range
        # make sure the setting numbers won't cut off the original scale
        color_min <- 0
        color_max <- 0
        if (input$heatmap_color_max < max(df_heatmap_matrix, na.rm = TRUE)) {
          color_max <- max(df_heatmap_matrix, na.rm = TRUE)
        } else {
          color_max <- input$heatmap_color_max
        }
        if (input$heatmap_color_min > min(df_heatmap_matrix, na.rm = TRUE)) {
          color_min <- min(df_heatmap_matrix, na.rm = TRUE )
        } else {
          color_min <- input$heatmap_color_min
        }
        p_all_sample_heatmap <- ""
        output$heatmap <- renderPlotly({
          p_all_sample_heatmap <<- plot_ly(x = colnames(df_heatmap_matrix), y = rownames(df_heatmap_matrix), z = df_heatmap_matrix, type = "heatmap", 
                  colors = input$heatmap_color, zmin = color_min, zmax = color_max) %>%
            layout(title = "Heatmap - All the Samples", yaxis = list(autorange="reversed")) %>% 
            config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
          p_all_sample_heatmap
        })
      }
    }
    
  })
  
  # Histogram
  df_hist <- NULL
  hist_color_list <- NULL
  
  observeEvent(input$select_dataset_histogram, {
    
    histogramfile <- input$histogram_file
    
    df_hist <<- read.csv(histogramfile$datapath, header = TRUE, sep = ",")
    hist_columns <<- colnames(df_hist)[colnames(df_hist) != 'Gene']
    updateSelectizeInput(session, "hist_col_input", choices = hist_columns, server = TRUE)
  })
  
  observeEvent(input$hist_color_panel, {
    updateColourInput(session, "hist_color_select", label = "pick a color", value = "white", 
                      palette = input$hist_color_panel, returnName = TRUE)
  })
  
  observeEvent(input$add_hist_color, {
    if (is.null(hist_color_list)){
      hist_color_list <<- c(input$hist_color_select)
      updateSelectizeInput(session, "hist_color", label = "Histogram Colors", 
                           choices = hist_color_list, server = TRUE, 
                           options = list(create = TRUE))
    }
    else {
      hist_color_list <<- c(hist_color_list, input$hist_color_select)
      updateSelectizeInput(session, "hist_color", label = "Histogram Colors", 
                           choices = hist_color_list, server = TRUE, 
                           options = list(create = TRUE))
    }
  })
  
  observeEvent(input$clickhist, {
  
    hist_gene_list <- unique(strsplit(input$hist_gene_input, "\n")[[1]])
    output$hist <- renderPlotly({
      p <- plot_ly(alpha = 0.9)
      for (col_name in input$hist_col_input){
        p <- p %>% add_histogram(x = df_hist[[col_name]], name = col_name)
      }
      p <- p %>% config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d"),
                        toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height)) %>% 
        layout(title = "Histogram - All Genes")
      p
    })
    
    output$hist_gene <- renderPlotly({
      p <- plot_ly(alpha = 0.9, source = "hist_lasso") 
      df_hist_gene = df_hist[which(df_hist$Gene %in% hist_gene_list),]
      if (is.null(input$hist_color) == FALSE) {
        hist_colors <- input$hist_color
        color_i <- 1
        for (col_name in input$hist_col_input) {
          p <- p %>% add_histogram(x = df_hist_gene[[col_name]], name = col_name, 
                                   marker = list(color = hist_colors[color_i])) 
          color_i <- color_i + 1
        }
        event_register(p, "plotly_selected")
        p <- p %>% config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d"),
                          toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height)) %>%
          layout(title = input$histogram_title)
        p
      }
      else {
        for (col_name in input$hist_col_input) {
          p <- p %>% add_histogram(x = df_hist_gene[[col_name]], name = col_name) 
        }
        event_register(p, "plotly_selected")
        p <- p %>% config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d"),
                          toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height)) %>%
          layout(title = "Histogram - Current Genes List")
        p
      }
        
    })
  })
  
  
  # Box Plot
  df_box <- NULL
   
  observeEvent(input$select_dataset_boxplot, {
    
    boxfile <- input$box_file
    
    df_box <<- read.csv(boxfile$datapath, header = TRUE, sep = ",")
    box_columns <<- colnames(df_box)[colnames(df_box) != 'Gene']
    updateSelectizeInput(session, "box_column_input", choices = box_columns, server = TRUE)
  })
  
  
  observeEvent(input$clickbox, {
    gene_list <- c()
    if (is.null(input$box_input) == FALSE) {
      gene_list <- unique(strsplit(input$box_input, "\n")[[1]])
    }
    selected_columns <<- c()
    df_selected <<- df_box[which(df_box$Gene %in% gene_list),]
    if (is.null(input$box_column_input)) {
      selected_columns <<- box_columns
    }else {
      selected_columns <<- input$box_column_input
    }
    
    output$boxplot <- renderPlotly({
      p <- plot_ly(y = df_selected[, selected_columns[1]], type = "box", quartilemethod = "linear", name = selected_columns[[1]])
      for (col_n in selected_columns[-1]) {
        p <- p %>% add_trace(y = df_selected[, col_n], quartilemethod = "linear", name = col_n)
        
      }
      p <- p %>% config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
      p
    })
    
    output$boxplot_df <- renderDataTable({
      datatable(df_selected[, c("Gene",selected_columns)], 
                extensions = c("Buttons", "Scroller", "FixedColumns"),
                options = list(searching = TRUE, 
                               dom = "firtBPS", 
                               scrollX = TRUE, scrollY = 500, 
                               scroller = TRUE, 
                               fixedColumns = list(leftColumns = 2), 
                               buttons = c("copy", "csv", "excel")))}, 
      server = FALSE)
    
    ########## t.test part ##########
    t_len <- length(selected_columns)
    t_len_1 <- t_len - 1
    t_pvalue <- c()
    t_conf_int <- c()
    t_statistic <- c()
    x <- c()
    y <- c()
    t_comp <- c() # recording sample names from comparison
    for (i in c(1:t_len_1)) {
      z <- i + 1
      for (j in c(z:t_len)) {
        # t_comp <- c(t_comp, paste0(selected_columns[i], " __VS__ ", selected_columns[j]))
        x <- c(x, selected_columns[i])
        y <- c(y, selected_columns[j])
        t_test_res <- t.test(df_selected[, selected_columns[i]], df_selected[, selected_columns[j]])
        t_pvalue <- c(t_pvalue, t_test_res$p.value)
        t_conf_int <- c(t_conf_int, paste0(t_test_res$conf.int[1], " , ", t_test_res$conf.int[2]))
        t_statistic <- c(t_statistic, t_test_res$statistic)
      }
    }
    
    t_test_df <- data.frame(X = x, Y = y, p_value = t_pvalue, 
                            conf_interval = t_conf_int, t_statistic = t_statistic)
    
    output$boxplot_t_test <- renderDataTable({
      datatable(t_test_df, 
                extensions = c("Buttons", "Scroller", "FixedColumns"),
                options = list(searching = TRUE, 
                               dom = "firtBPS", 
                               scrollX = TRUE, scrollY = 500, 
                               scroller = TRUE, 
                               fixedColumns = list(leftColumns = 2), 
                               buttons = c("copy", "csv", "excel")))}, 
      server = FALSE)
    
    
    
  })
  
  observeEvent(input$clickbox_gene, {
    gene_list <<- c()
    if (is.null(input$box_input) == FALSE) {
      gene_list <<- unique(strsplit(input$box_input, "\n")[[1]])
    }
    selected_columns <- c()
    df_selected <- df_box[which(df_box$Gene %in% gene_list),]
    if (is.null(input$box_column_input)) {
      selected_columns <- box_columns
      df_selected <- df_selected
    }else {
      selected_columns <- input$box_column_input
      print(selected_columns[1:2])
      df_selected <- df_selected[, c("Gene", selected_columns)]
    }
    
    # re-create a transposed dataframe, first columns is sample name, first row is gene name
    gene_data_list <- lapply(gene_list, function(x) { x = as.vector(unlist(
      lapply(df_selected[which(df_selected$Gene == x),][-1], function(y) {y[1]})
    ))})
    df_t <- data.frame(gene_data_list)
    names(df_t) <- gene_list
    # check.names = FALSE is very IMPORTANT!!!, if it's true, it will check the names
    # if it's like abc-def, it turns it into abc.def
    df_t <- data.frame(Sample = selected_columns, df_t, check.names = FALSE)
    df_t_selected <<- df_t[which(df_t$Sample %in% selected_columns),]
    group_list_range <- c()
    group_list <- NULL
    group_n <- 0
    if(input$box_column_group != ""){
      group_list <- as.vector(unlist(lapply(strsplit(input$box_column_group, ",")[[1]], function(x){as.numeric(x)})))
      right_max <- group_list[1]
      l_start <- 1
      for (g in group_list) {
        r_current <- l_start + g - 1
        current_range <- c(l_start, r_current)
        group_list_range <- c(group_list_range, c(current_range))
        l_start <- r_current + 1
      }
      group_list_range <- array(group_list_range, dim = c(2, length(group_list_range)/2))
      group_n <- length(group_list_range)/2
    }
    
    hovertext <- df_t_selected$Sample
    
    # tranposed dataframe method
      
    output$boxplot_gene <- renderPlotly({
      if(input$box_column_group == ""){
        p <- plot_ly(y = df_t_selected[, gene_list[1]],
                   type = "box", quartilemethod = "linear", name = gene_list[1])
        for (gene_n in gene_list[-1]) {
          p <- p %>% add_trace(y = df_t_selected[, gene_n], quartilemethod = "linear", name = gene_n)
        }
        p <- p %>% config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
        p   
      }else{
        # The first boxplot needs to be seperated and then keep adding trace
        samples_1 <- df_t_selected$Sample[group_list_range[,1][1]: group_list_range[,1][2]]
        s_n_1 <- as.vector(unlist(lapply(samples_1, function(x){tail(strsplit(x, "_")[[1]], n=1)})))
        b_name_1 <- paste0(gene_list[1], "_", paste(s_n_1, collapse = "_"))
        p <- plot_ly(y = df_t_selected[, gene_list[1]][group_list_range[,1][1]: group_list_range[,1][2]],
                   type = "box", quartilemethod = "linear", name = b_name_1)
        for (i in c(2:group_n)) {
          samples <- df_t_selected$Sample[group_list_range[,i][1]: group_list_range[,i][2]]
          s_n <- as.vector(unlist(lapply(samples, function(x){tail(strsplit(x, "_")[[1]], n=1)})))
          b_name <- paste0(gene_list[1], "_", paste(s_n, collapse = "_"))
          p <- p %>% add_trace(y = df_t_selected[, gene_list[1]][group_list_range[,i][1]: group_list_range[,i][2]], quartilemethod = "linear", name = b_name)
        }
        for (gene_n in gene_list[-1]) {
          for (i in c(1:group_n)) {
            samples <- df_t_selected$Sample[group_list_range[,i][1]: group_list_range[,i][2]]
            s_n <- as.vector(unlist(lapply(samples, function(x){tail(strsplit(x, "_")[[1]], n=1)})))
            b_name <- paste0(gene_n, "_", paste(s_n, collapse = "_"))
            p <- p %>% add_trace(y = df_t_selected[, gene_n][group_list_range[,i][1]: group_list_range[,i][2]], quartilemethod = "linear", name = b_name)
          }
        }
        p <- p %>% config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height))
        p
      }
      
    })
    
    output$boxplot_gene_df <- renderDataTable({
      datatable(df_t_selected, 
                extensions = c("Buttons", "Scroller", "FixedColumns"),
                options = list(searching = TRUE, 
                               dom = "firtBPS", 
                               scrollX = TRUE, scrollY = 500, 
                               scroller = TRUE, 
                               fixedColumns = list(leftColumns = 2), 
                               buttons = c("copy", "csv", "excel")))}, 
      server = FALSE)
    
    
  })
  
  # Hockey Stick Plot
  
  # Set dataframe for heatmap
  df_hockey <- NULL
  # This dataframe is used for "paste gene" action button, because it needs to be globel variable
  df_hockey_gene <- NULL
  
  observeEvent(input$select_dataset_hockeystickplot, {
    hockeystickfile <- input$hockeystick_file
    
    df_hockey <<- read.csv(hockeystickfile$datapath, header = TRUE, sep = ",")
    hockey_samples <<- colnames(df_hockey)[colnames(df_hockey) != "Gene"]
    print(hockey_samples)
    updateSelectizeInput(session, "hockeystick_col_input", choices = hockey_samples, server = TRUE)
  })
  
  observeEvent(input$clickhockeystick, {
    if (!is.null(input$hockeystick_col_input)) {
      df_hockey <- df_hockey[order(df_hockey[[input$hockeystick_col_input]]),]
      df_hockey$rank <- c(1:length(rownames(df_hockey)))
      
      # Set x and y minimum threshold to show the filtered gene in a table
      x_top <- 0
      x_top_exclude <- 0
      y_min <- 0
      y_max <- 0
      
      if (input$hockeystick_x_top == '') {
        input$hockeystick_x_top <- 0
      }
      
      if (input$hockeystick_x_top != 0) {
        x_top <- input$hockeystick_x_top
        num_rows <- length(df_hockey$rank)
        if (input$hockeystick_x_top_exclude != 0) {
          x_top_exclude <- input$hockeystick_x_top_exclude
          df_hockey <- df_hockey[which(df_hockey$rank %in% c((num_rows - input$hockeystick_x_top + 1) : (num_rows - x_top_exclude))),]
        } 
        df_hockey <- df_hockey[which(df_hockey$rank %in% c((num_rows - input$hockeystick_x_top + 1) : num_rows)),]
      }
      
      if (input$hockeystick_y_min != 0 && input$hockeystick_y_max != 0) {
        y_min <- input$hockeystick_y_min
        y_max <- input$hockeystick_y_max
          df_hockey <- df_hockey[which(df_hockey[[input$hockeystick_col_input]] %in% c(y_min : y_max)), ]
      } 
      else if (input$hockeystick_y_min != 0) {
        df_hockey <- df_hockey[which(df_hockey[[input$hockeystick_col_input]] >= input$hockeystick_y_min), ]
      }
      else if (input$hockeystick_y_max != 0) {
        df_hockey <- df_hockey[which(df_hockey[[input$hockeystick_col_input]] <= input$hockeystick_y_max), ]
      }
      
      df_hockey_gene <<- df_hockey
      
      # Set a threshold for showing data table, if it exceeds 1000 rows, it doesn't show the table
      if (length(df_hockey$rank) <= 1000) {
        output$hockeystick_table <- renderDataTable({
          datatable(df_hockey, 
                extensions = c("Buttons", "Scroller", "FixedColumns"),
                options = list(searching = TRUE, 
                               dom = "firtBPS", 
                               scrollX = TRUE, scrollY = 500, 
                               scroller = TRUE, 
                               fixedColumns = list(leftColumns = 2), 
                               buttons = c("copy", "csv", "excel")))}, 
          server = FALSE)
        }
      
      if (input$hockeystick_gene_input != "") {
        hockeystickplot_gene_list <- unique(strsplit(input$hockeystick_gene_input, "\n")[[1]])
        gene_error_list <- get_gene_error_list(input$hockeystick_gene_input, df_hockey)
        selected_gene_index <- match(toupper(hockeystickplot_gene_list[!hockeystickplot_gene_list %in% gene_error_list]), toupper(df_hockey$Gene))
        selected_gene_index <- selected_gene_index[!is.na(selected_gene_index)]
        if (length(gene_error_list) > 0) {
          shinyalert(title = "You have entered some genes are not in the data!", text = paste0("There are ", length(gene_error_list)," unmatched genes"), type = "error")
          updateTextAreaInput(session, "hockeystick_gene_error_list", value = paste0(gene_error_list, collapse = "\n" ))
        }
          
        output$hockeystick_plot <- renderPlotly({
          p <- plot_ly(data = df_hockey, type = "scatter", mode = "markers", x = df_hockey$rank, y = df_hockey[[input$hockeystick_col_input]],
                       text = df_hockey[["Gene"]], hoverinfo = text) %>%
          
            layout(title = input$hockeystick_title,
                   xaxis = list(title = input$hockystick_x_title),
                   yaxis = list(title = input$hockystick_y_title)
                  )

          p <- p %>% add_annotations(x = df_hockey$rank[selected_gene_index], y = df_hockey[[input$hockeystick_col_input]][selected_gene_index],
                                     text = df_hockey[selected_gene_index,][["Gene"]],
                                     xref = "x", yref = "y", showarrow = input$hockystick_arrow, arrowhead = 3, arrowsize = 0.5, ax = -200, ay = -100,
                                     font = list(size = 16))
          
          p <- p %>% add_markers(x = df_hockey$rank[selected_gene_index], y = df_hockey[[input$hockeystick_col_input]][selected_gene_index],
                                 text = df_hockey[selected_gene_index,][["Gene"]],
                                 showlegend = FALSE) %>% 
               config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height),
                   edits = list(
                     annotationPosition = TRUE,
                     annotationTail = TRUE,
                     annotationText = TRUE
                   ))
          p
        })
      } else {
        output$hockeystick_plot <- renderPlotly({
          p <- plot_ly(data = df_hockey, type = "scatter", mode = "markers", x = df_hockey$rank, y = df_hockey[[input$hockeystick_col_input]],
                       text = df_hockey[["Gene"]], hoverinfo = text) %>%
            layout(title = input$hockeystick_title,
                   xaxis = list(title = input$hockystick_x_title),
                   yaxis = list(title = input$hockystick_y_title),
                   margin = 10
                  ) %>% 
            config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height),
                 edits = list(
                   annotationPosition = TRUE,
                   annotationTail = TRUE,
                   annotationText = TRUE
                 ))
          p  
        })  
      }
      
      
    }
  })
  
  observeEvent(input$paste_hockeystick_genes, {
    updateTextAreaInput(session, "hockeystick_gene_input", value = paste(paste0(df_hockey_gene$Gene, collapse = "\n"), "\n", sep = ""))
  })
  
  # Volcano Plot
  observeEvent(input$clickvolcano, {
    
    volcanoplotfile <- input$volcanoplot_file
    df_volcano <<- read.csv(volcanoplotfile$datapath, header = TRUE, sep = ",")
    
    df_volcano$color_col <- rep("", nrow(df_volcano))
    volcano_blue <- paste0("-", input$volcano_log2fc_thred, "<Log2FC<", input$volcano_log2fc_thred)
    volcano_red <- paste0("Log2FC>=", input$volcano_log2fc_thred, " & Log2FC<=-", input$volcano_log2fc_thred, " & padj<=", input$volcano_padj_thred)
    df_volcano$color_col[which(df_volcano$log2FoldChange < input$volcano_log2fc_thred & df_volcano$log2FoldChange > -input$volcano_log2fc_thred)] <- volcano_blue
    df_volcano$color_col[which(df_volcano$log2FoldChange >= input$volcano_log2fc_thred | df_volcano$log2FoldChange <= -input$volcano_log2fc_thred)] <- volcano_red
    df_volcano$color_col[which(-log10(df_volcano$padj) < -log10(input$volcano_padj_thred) )] <- "NS"
    
    p <- ""
    
    if (input$volcano_gene_input != "") {
      volcano_gene_list <- unique(strsplit(input$volcano_gene_input, "\n")[[1]])
      gene_error_list <- get_gene_error_list(input$volcano_gene_input, df_volcano)
      # after removing the incorrect genes 
      volcano_filtered_gene_list <<- volcano_gene_list[!volcano_gene_list %in% gene_error_list]
      volcano_filtered_gene_list <<- volcano_filtered_gene_list[!volcano_filtered_gene_list == ""]
      print(volcano_filtered_gene_list)
      if (length(gene_error_list) > 0) {
        shinyalert(title = "You have entered some genes are not in the data!", text = paste0("There are ", length(gene_error_list)," unmatched genes"), type = "error")
        updateTextAreaInput(session, "volcano_gene_error_list", value = paste0(gene_error_list, collapse = "\n" ))
      }
      
      output$volcano_plot <- renderPlotly({
          p <<- plot_ly(data = df_volcano, type = "scatter", mode = "markers", x = df_volcano$log2FoldChange, y = -log10(df_volcano$padj),
                       text = df_volcano$Gene, hoverinfo = text, color = df_volcano$color_col, colors = c("blue", "red", "grey")
                       ) %>%
                  layout(shapes = list(vline(input$volcano_log2fc_thred), vline(-input$volcano_log2fc_thred), hline(-log10(input$volcano_padj_thred))),
                         title = input$volcano_title,
                         xaxis = list(title = paste0("Log2 Fold Change (", nrow(df_volcano), "variables)")),
                         yaxis = list(title = "-Log10(p)"),
                         annotations =
                           list(
                              x = df_volcano[match(tolower(volcano_filtered_gene_list), tolower(df_volcano$Gene)), ][["log2FoldChange"]],
                              y = -log10(df_volcano[match(tolower(volcano_filtered_gene_list), tolower(df_volcano$Gene)), ][["padj"]]),
                             text = df_volcano[match(tolower(volcano_filtered_gene_list), tolower(df_volcano$Gene)), ][["Gene"]],
                             font = list(size = 14),
                             xanchor = 'right',
                             showarrow = FALSE, arrowhead = 6)
                         ) %>%
                  config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height),
                     edits = list(
                       annotationPosition = TRUE,
                       annotationTail = TRUE,
                       annotationText = TRUE
                     ))
          })

    } else {
      if (nrow(df_volcano[which(df_volcano$color_col == volcano_red), ]) <= 20) {
        output$volcano_plot <- renderPlotly({
          p <<- plot_ly(data = df_volcano, type = "scatter", mode = "markers", x = df_volcano$log2FoldChange, y = -log10(df_volcano$padj),
                       text = df_volcano$Gene, hoverinfo = text, color = df_volcano$color_col, colors = c("blue", "red", "grey")
                       ) %>% 
                  layout(shapes = list(vline(input$volcano_log2fc_thred), vline(-input$volcano_log2fc_thred), hline(-log10(input$volcano_padj_thred))),
                         title = input$volcano_title,
                         xaxis = list(title = paste0("Log2 Fold Change (", nrow(df_volcano), "variables)")),
                         yaxis = list(title = "-Log10(p)"),
                         annotations =
                           list(
                              x = df_volcano[which(df_volcano$color_col == volcano_red),][["log2FoldChange"]],
                              y = -log10(df_volcano[which(df_volcano$color_col == volcano_red),][["padj"]]),
                             text = df_volcano[which(df_volcano$color_col == volcano_red),][["Gene"]],
                             font = list(size = 14),
                             xanchor = 'right',
                             showarrow = FALSE, arrowhead = 6)
                         ) %>%
                  config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height),
                     edits = list(
                       annotationPosition = TRUE,
                       annotationTail = TRUE,
                       annotationText = TRUE
                     ))
          })
      
        
      } else {
        output$volcano_plot <- renderPlotly({
          p <<- plot_ly(data = df_volcano, type = "scatter", mode = "markers", x = df_volcano$log2FoldChange, y = -log10(df_volcano$padj),
                       text = df_volcano$Gene, hoverinfo = text, color = df_volcano$color_col, colors = c("blue", "red", "grey")
                       ) %>% 
                  layout(shapes = list(vline(input$volcano_log2fc_thred), vline(-input$volcano_log2fc_thred), hline(-log10(input$volcano_padj_thred))),
                         title = input$volcano_title,
                         xaxis = list(title = paste0("Log2 Fold Change (", nrow(df_volcano), "variables)")),
                         yaxis = list(title = "-Log10(p)")
                         ) %>%
                  config(toImageButtonOptions = list(format = input$img_type, filename = "", width = input$img_width, height = input$img_height),
                     edits = list(
                       annotationPosition = TRUE,
                       annotationTail = TRUE,
                       annotationText = TRUE
                     ))
          })
      
      }
      
      output$volcano_html_download <- html_download(p)
        
    }
    
    
  })
  
  # Sample info table
  # observeEvent(input$info_samples, {
  #   sample_df <- de_jsonify(get_samples(input$info_datasets))
  #   output$sample_table <- renderDataTable({
  #     datatable(sample_df,
  #               extensions = c("Buttons", "Scroller", "FixedColumns"),
  #               options = list(searching = TRUE, 
  #                              dom = "firtBPS", 
  #                              pageLength = 1000,
  #                              fixedColumns = list(leftColumns = 2), 
  #                              buttons = c("copy", "csv", "excel"))
  #     )
  #               
  #   })
  # })
})
  