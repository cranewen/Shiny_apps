library(DT)
library(shiny)
library(plotly)
library(heatmaply)
library(rclipboard)
library(shinyalert)
library(colourpicker)
library(shinydashboard)



shinyUI(
  dashboardPage(
    dashboardHeader(title = "Apps"),
    dashboardSidebar(tags$style(type = 'text/css',
                                ".selectize-input { word-wrap : break-word;}
                                 .selectize-dropdown {word-wrap : break-word;} "),
      sidebarMenu(id = "sidebar", 
                  menuItem("Scatter Plot", tabName = "scatter_tab", icon = icon("image", lib = "font-awesome"),
                           fileInput("scatter_file", "upload file"),
                           actionButton("select_dataset_scatter", label = "Submit"),
                           tags$hr(style = "border-color: green;"),
                           selectInput("dataset_col1", label = "Sample 1", choices = NULL, selectize = FALSE),
                           selectInput("dataset_col2", label = "Sample 2", choices = NULL, selectize = FALSE),
                           checkboxInput("scatter_r2", label = "R Squared", value = FALSE),
                           checkboxInput("scatter_arrow", label = "Scatter Arrow", value = TRUE),
                           sliderInput("r2_size", label = "R Squared Font size", min = 10, max = 35, value = 10),
                           radioButtons("r2_color", label = "R Squared color", choices = c("Blue", "Green", "Black"), selected = "Blue"),
                           actionButton("clickscatter", label = "Draw Scatter Plot"),
                           tags$hr(style = "border-color: green;"),
                           uiOutput("copybutton"),
                           rclipboardSetup(),
                           # uiOutput("clipbutton"),
                           selectizeInput("highlight_scatter", label = "Specified Genes", choices = NULL,
                                          multiple = TRUE, options = list(create = FALSE)),
                           textAreaInput("scatter_gene_input", label = "Selected genes", value = "", height = '200px'),
                           uiOutput("copy_history_slider"),
                           actionButton("clickscatter_highlight_gene", label = "Update Current Gene List/Plot"),
                           tags$hr(style = "border-color: green;"),
                           uiOutput("clipbutton_all"),
                           # uiOutput("copybutton_all"),
                           colourInput("color1", label = "interesting gene color", value = "blue", returnName = TRUE),
                           colourInput("color2", label = "uninteresting gene color", value = "green", returnName = TRUE)
                           # actionButton("clickscatter_highlight", label = "Highlight Scatter"),
                           ),
                  menuItem("Heatmap", tabName = "heatmap_tab", icon = icon("barcode", lib = "font-awesome"),
                           fileInput("heatmap_file", "upload file"),
                           actionButton("select_dataset_heatmap", label = "Submit"),
                           tags$hr(style = "border-color: green;"),
                           selectizeInput("heatmap_col_input", label = "Select Samples", choices = NULL,
                                          multiple = TRUE, options = list(create = FALSE)),
                           textAreaInput("heatmap_gene_input", label = "Selected Genes", value = "", 
                                         height = '200px'),
                           textInput("heatmap_title", label = "Enter Heatmap Title", value = NULL),
                           actionButton("clickheatmap", label = "Draw Heatmap"),
                           checkboxInput("heatmap_text", label = "Show Text", value = FALSE),
                           checkboxInput("heatmap_cluster_checkbox", label = "Clustering Heatmap", value = FALSE),
                           selectInput("show_cluster", label = "Choose Cluster", choices = c("both", "row", "column")),
                           selectInput("heatmap_color", label = "Choose Colors", 
                                       choices = c("viridis", "Blues", "Greens", "Reds", "BuGn", "GnBu", "PuBuGn", 
                                                   "RdYlBu", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")),
                           numericInput("heatmap_color_max", label = "Color Max", value = 0, min = -10, max = 10, step = 1, width = '100px'),
                           numericInput("heatmap_color_min", label = "Color Min", value = 0, min = -10, max = 10, step = 1, width = '100px'),
                           useShinyalert()
                           ),
                  menuItem("Histogram", tabName = "histogram_tab", icon = icon("area-chart", lib = "font-awesome"),
                           fileInput("histogram_file", "upload file"),
                           actionButton("select_dataset_histogram", label = "Submit"),
                           tags$hr(style = "border-color: green;"),
                           selectizeInput("hist_col_input", label = "Select Samples", choices = NULL,
                                          multiple = TRUE, options = list(create = TRUE)),
                           textAreaInput("hist_gene_input", label = "Selected Genes", value = "", 
                                         height = '200px'),
                           colourInput("hist_color_select", label = "pick a color", value = "white", 
                                       palette = c("limited"), returnName = TRUE),
                           radioButtons("hist_color_panel", label = "color panel", selected = "limited",
                                        choiceNames = c("customize", "preset"), 
                                        choiceValues = c("square", "limited")),
                           actionButton("add_hist_color", label = "Add Color"),
                           selectizeInput("hist_color", label = "Histogram Colors", choices = NULL,
                                          multiple = TRUE, options = list(create = TRUE)),
                           textInput("histogram_title", label = "Enter Histogram Title", value = NULL),
                           actionButton("clickhist", label = "Draw Histogram")
                           ),
                  menuItem("Box Plot", tabName = "boxplot_tab", icon = icon("bar-chart"), 
                           fileInput("box_file", "upload file"),
                           actionButton("select_dataset_boxplot", label = "Submit"),
                           tags$hr(style = "border-color: green;"),
                           selectizeInput("box_column_input", label = "Select Samples", choices = NULL, 
                                          multiple = TRUE, options = list(create = FALSE)),
                           textInput("box_column_group", label = "Numbers of group samples", value = NULL),
                           textAreaInput("box_input", label = "Selected Genes", value = "", 
                                         height = '200px'),
                           # radioButtons("box_algorithm", label = "Select a algorithm",
                           #              choices = c("linear", "exclusive", "inclusive"),
                           #              selected = "linear"),
                           actionButton("clickbox", label = "Draw Box Plot"),
                           actionButton("clickbox_gene", label = "Draw Box Plot (gene)")
                           ),
                  menuItem("Hockey Stick Plot", tabName = "Hockey_stick_tab", icon = icon("bar-chart"),
                           fileInput("hockeystick_file", "upload file"),
                           actionButton("select_dataset_hockeystickplot", label = "Submit"),
                           selectInput("hockeystick_col_input", label = "Select Sample", choices = NULL, selectize = TRUE),
                           tags$hr(style = "border-color: green;"),
                           textAreaInput("hockeystick_gene_input", label = "Enter Genes", value = "", 
                                         height = '200px'),
                           checkboxInput("hockystick_arrow", label = "Show Arrow?", value = TRUE),
                           actionButton("paste_hockeystick_genes", label = "Paste Genes"),
                           numericInput("hockeystick_x_top", label = "Top Genes (X)", value = 0, min = 0, width = '200px'),
                           numericInput("hockeystick_x_top_exclude", label = "Exclude Extreme Genes (X)", value = 0, min = 0, width = '200px'),
                           numericInput("hockeystick_y_min", label = "Counts Above (Y)", value = 0, step = 100000, width = '200px'),
                           numericInput("hockeystick_y_max", label = "Counts Below (Y)", value = 0, step = 100000, width = '200px'),
                           actionButton("clickhockeystick", label = "Draw Hockey Stick Plot"),
                           textInput("hockeystick_title", label = "Enter the title", value = NULL),
                           textInput("hockystick_x_title", label = "Enter X-axis title", value = NULL),
                           textInput("hockystick_y_title", label = "Enter Y-axis title", value = NULL)
                           ),
                  menuItem("Volcano Plot", tabName = "Volcano_Plot_tab", icon = icon("align-center"),
                           fileInput("volcanoplot_file", "upload file"),
                           textInput("volcano_title", label = "Enter the title", value = NULL),
                           numericInput("volcano_log2fc_thred", label = "Enter Log2FC Cutoff(only + values)", value = 1, min = 0, width = '200px'),
                           numericInput("volcano_padj_thred", label = "Enter P adjusted value Cutoff(only + values)", value = 0.05, min = 0, width = '200px'),
                           textAreaInput("volcano_gene_input", label = "Enter Genes", value = "", height = "200px"),
                           actionButton("clickvolcano", label = "Draw Volcano Plot"),
                           downloadButton("volcano_html_download", label = "Download HTML")
                           ),
                  tags$h4("Image Settings", align = "center"),
                  numericInput("img_width", label = "Image Width", value = 1000, width = '150px'),
                  numericInput("img_height", label = "Image Height", value = 1000, width = '150px'),
                  selectInput("img_type", label = "Image Format", choices = c("svg", "png", "jpeg"))
                          )
    ),
    dashboardBody(
        tabsetPanel(
          tabPanel("Scatter Plot", 
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   ),
                  fluidRow(
                    withMathJax(),
                    column(10, plotlyOutput("scatterplot", width = 1000, height = 800)),
                    column(2, uiOutput("lasso_gene_dt_table"))
                    ),
                  
                  fluidRow(
                    DTOutput("selected_dt", width = 1000, height = 600)
                    )
      
                  ),
          tabPanel("Heatmap", 
                   fluidRow(
                     plotlyOutput("heatmap_sample", width = 1000, height = 800)
                     ), 
                   fluidRow(
                     plotlyOutput("heatmap", width = 1000, height = 800)
                     ), 
                   fluidRow(
                     plotlyOutput("heatmap_clust", width = 1000, height = 600)
                     ),
                   fluidRow(
                     textAreaInput("heatmap_gene_error_list", label = "Unmatched Genes", value = "", height = "200px")
                     )
                   ),
          tabPanel("Histogram",
                   fluidRow(
                     column(10, plotlyOutput("hist_gene", width = 1000, height = 500))
                     ),
                   fluidRow(
                     column(10, plotlyOutput("hist", width = 1000, height = 500))
                     )
                   ),
          tabPanel("Box Plot", 
                   fluidRow(
                     plotlyOutput("boxplot", width = 1000, height = 600) 
                     ),
                   fluidRow(
                     DTOutput("boxplot_t_test", width = 1000, height = 700)
                     ),
                   fluidRow(
                     DTOutput("boxplot_df", width = 1000, height = 700)
                     ),
                   fluidRow(
                     plotlyOutput("boxplot_gene", width = 1000, height = 600) 
                     ),
                   fluidRow(
                     DTOutput("boxplot_gene_df", width = 1000, height = 700)
                     )
                   ),
          tabPanel("Hockey Stick Plot",
                   fluidRow(
                     plotlyOutput("hockeystick_plot", width = 1000, height = 600)
                     ),
                   fluidRow(
                     DTOutput("hockeystick_table", width = 1000, height = 600)
                     ),
                   fluidRow(
                     textAreaInput("hockeystick_gene_error_list", label = "Unmatched Genes", value = "", height = "200px")
                     )
                   ),
          tabPanel("Volcano Plot", 
                   fluidRow(
                     plotlyOutput("volcano_plot", width = 1000, height = 700)
                     ),
                   fluidRow(
                     textAreaInput("volcano_gene_error_list", label = "Unmatched Genes", value = "", height = "200px")
                     )
                   ),
          tabPanel("Sample Info Table",
                   DTOutput("sample_table", width = 1000, height = 1000)
                   )

        )
        
    )
  ) 
  
)