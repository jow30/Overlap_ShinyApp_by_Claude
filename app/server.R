# ============================================================
# server.R - Server logic
# ============================================================

server <- function(input, output, session) {

  # --- Data Loading Module ---
  gene_lists <- dataLoaderServer("data_loader")

  # --- Output flag for conditional panels ---
  output$has_data <- reactive({
    gl <- gene_lists()
    length(gl) > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  # --- Gene Preview Module ---
  selected_names <- genePreviewServer("gene_preview", gene_lists)

  # --- Dynamic Venn panel (hide if > 5 selected) ---
  output$venn_panel <- renderUI({
    sel <- selected_names()
    if (length(sel) > 5) {
      div(
        class = "alert",
        style = "background: #e67e22; color: white; border-radius: 8px; padding: 20px; border: none; text-align: center;",
        icon("exclamation-triangle", style = "font-size: 24px;"),
        tags$br(), tags$br(),
        tags$strong(paste0(length(sel), " gene lists selected.")),
        tags$br(),
        "Venn diagrams support up to 5 sets. Please use the UpSet Plot tab for larger comparisons,",
        "or deselect some gene lists in the Preview tab."
      )
    } else if (length(sel) < 2) {
      div(
        class = "alert",
        style = "background: #3498db; color: white; border-radius: 8px; padding: 20px; border: none; text-align: center;",
        icon("info-circle", style = "font-size: 24px;"),
        tags$br(), tags$br(),
        "Please select at least 2 gene lists in the Preview tab to generate a Venn diagram."
      )
    } else {
      vennPlotUI("venn_plot")
    }
  })

  # --- Venn Plot Module ---
  venn_info <- vennPlotServer("venn_plot", gene_lists, selected_names)

  # --- UpSet Plot Module ---
  upset_info <- upsetPlotServer("upset_plot", gene_lists, selected_names)

  # --- Gene Search Module ---
  geneSearchServer("gene_search", gene_lists, selected_names)

  # --- Download Handler Module ---
  downloadHandlerServer("download_handler", gene_lists, selected_names,
                         venn_info, upset_info)
}
