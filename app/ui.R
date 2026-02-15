# ============================================================
# ui.R - User Interface definition
# ============================================================

ui <- dashboardPage(
  skin = "blue",

  # --- Header ---
  dashboardHeader(
    title = tags$span(
      icon("dna", style = "color: #ecf0f1;"),
      " Gene Set Venn",
      style = "font-weight: 600; font-size: 16px;"
    ),
    titleWidth = 250
  ),

  # --- Sidebar (20% width) ---
  dashboardSidebar(
    width = 250,
    div(
      style = "padding: 10px;",
      dataLoaderUI("data_loader")
    )
  ),

  # --- Body (80% width) ---
  dashboardBody(
    # Custom CSS for polished look
    tags$head(
      tags$style(HTML("
        /* Global font */
        body, .content-wrapper, .main-sidebar {
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }

        /* Sidebar styling */
        .main-sidebar {
          background-color: #2c3e50 !important;
        }
        .skin-blue .main-sidebar .sidebar {
          background-color: #2c3e50 !important;
        }
        .skin-blue .main-header .logo {
          background-color: #1a252f !important;
        }
        .skin-blue .main-header .navbar {
          background-color: #2c3e50 !important;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #1a252f !important;
        }

        /* Content area */
        .content-wrapper {
          background-color: #f5f6fa !important;
        }

        /* Box styling */
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 12px rgba(0,0,0,0.08);
          border-top: none;
        }
        .box-header {
          border-bottom: 1px solid #e9ecef;
        }

        /* Tab styling */
        .nav-tabs-custom > .nav-tabs > li.active > a {
          color: #2c3e50;
          font-weight: 600;
          border-top-color: #3498db;
        }

        /* File input styling */
        .form-control {
          border-radius: 6px;
        }

        /* Button styling */
        .btn-primary {
          background-color: #3498db !important;
          border-color: #2980b9 !important;
          border-radius: 6px;
        }
        .btn-primary:hover {
          background-color: #2980b9 !important;
        }

        /* Download button */
        .btn-block {
          margin-top: 10px;
        }

        /* DataTable styling */
        table.dataTable thead th {
          background-color: #2c3e50;
          color: white;
          font-weight: 600;
        }

        /* Radio buttons */
        .btn-group-justified .btn {
          border-radius: 6px !important;
          margin: 2px !important;
        }

        /* Alert custom */
        .alert-warning {
          border-radius: 6px;
        }

        /* Scrollbar */
        .sidebar::-webkit-scrollbar {
          width: 6px;
        }
        .sidebar::-webkit-scrollbar-thumb {
          background-color: #4a6785;
          border-radius: 3px;
        }

        /* Panel info text */
        .info-panel {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 30px;
          border-radius: 12px;
          text-align: center;
          margin: 20px 0;
        }

        /* Colour picker adjustments */
        .bootstrap-colorpicker .form-control {
          padding-left: 8px;
        }

        /* Slider coloring */
        .irs--shiny .irs-bar {
          background: #3498db;
          border-color: #3498db;
        }
        .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
          background-color: #3498db;
        }

        /* Tab content padding */
        .tab-pane {
          padding: 15px 5px;
        }
      "))
    ),

    # Welcome / info when no data loaded
    conditionalPanel(
      condition = "!output.has_data",
      div(
        class = "info-panel",
        h2(icon("dna"), " Gene Set Venn", style = "margin-top: 0;"),
        p("Visualize gene set overlaps with interactive Venn diagrams and UpSet plots.", style = "font-size: 16px; opacity: 0.9;"),
        hr(style = "border-color: rgba(255,255,255,0.3);"),
        p(icon("upload"), " Upload gene lists from the sidebar to get started.",
          style = "font-size: 14px;"),
        p(
          tags$small("Supports: txt/csv files, Excel (.xlsx), and RDS files",
                     style = "opacity: 0.7;")
        )
      )
    ),

    # Main content when data loaded
    conditionalPanel(
      condition = "output.has_data",
      tabBox(
        id = "main_tabs",
        width = 12,
        selected = "Preview",

        tabPanel(
          "Preview",
          icon = icon("table"),
          genePreviewUI("gene_preview")
        ),

        tabPanel(
          "Venn Diagram",
          icon = icon("circle-notch"),
          uiOutput("venn_panel")
        ),

        tabPanel(
          "UpSet Plot",
          icon = icon("chart-bar"),
          upsetPlotUI("upset_plot")
        ),

        tabPanel(
          "Search",
          icon = icon("search"),
          geneSearchUI("gene_search")
        )
      )
    )
  )
)
