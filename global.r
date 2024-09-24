library(digest)
library(httr)


de_jsonify <- function(parsed_json_list) {
  return(do.call(rbind.data.frame, parsed_json_list))
}

html_download <- function(plot_obj) {
  downloadHandler(filename = ".html", content = function(file){ htmlwidgets::saveWidget(plot_obj, file = file)})
}

plotly_config <- function() {
  plotly::config(
    toImageButtonOptions = list(format = input$img_type, filename = "", width = input$svg_width, height = input$svg_heigh),
    edits = list(
      annotationPosition = TRUE,
      annotationTail = TRUE,
      annotationText = TRUE
    )
  )
}

# gene_input_list should be like input$xxx, df is the original dataframe
get_gene_error_list <- function(gene_input_list, df) {
  input_list <- unique(strsplit(gene_input_list, "\n")[[1]])
  gene_error_list <- input_list[which(!input_list %ucin% df$Gene)]
  gene_error_list <- gene_error_list[gene_error_list != ""]
  return(gene_error_list)
}

get_tss_list <- function(gene_input_list, df) {
  input_list <- unique(strsplit(gene_input_list, "\n")[[1]])
  gene_error_list <- input_list[which(!input_list %ucin% df$Gene)]
  gene_error_list <- gene_error_list[gene_error_list != ""]
  input_list <- input_list[gene_error_list %in% input_list == FALSE]
  return(input_list)
  
}

# vertical and horizontal dash lines in volcano plot
vline <- function(x = 0, color = "red") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, dash = "dash", showlegend = FALSE)
  )
}

hline <- function(y = 0, color = "blue") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, dash = "dash", showlegend = FALSE)
  )
}

