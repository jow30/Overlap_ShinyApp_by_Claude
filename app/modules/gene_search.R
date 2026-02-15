# ============================================================
# gene_search.R - Module for searching genes across intersections
# ============================================================

geneSearchUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Gene Search", style = "color: #2c3e50; font-weight: 600;"),
    fluidRow(
      column(12,
        div(
          style = "margin-bottom: 15px;",
          selectizeInput(
            inputId = ns("search_gene"),
            label = NULL,
            choices = NULL,
            options = list(
              placeholder = "Type a gene name to search...",
              maxOptions = 20,
              create = FALSE
            ),
            width = "100%"
          )
        )
      )
    ),
    fluidRow(
      column(12,
        uiOutput(ns("search_results"))
      )
    ),
    # Download intersections section
    hr(style = "border-color: #e9ecef;"),
    div(
      style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
      fluidRow(
        column(4, h5(icon("download"), " Download Intersections",
                     style = "font-weight: 600; color: #2c3e50; margin-top: 5px;")),
        column(3, selectInput(ns("data_format"), NULL,
                    choices = c("Excel (.xlsx)" = "xlsx",
                                "CSV (zipped)" = "csv",
                                "Text (zipped)" = "txt"),
                    width = "100%")),
        column(3, downloadButton(ns("download_data"), "Download",
                    class = "btn-primary", style = "width: 100%; margin-top: 1px;")),
        column(2)
      )
    )
  )
}

geneSearchServer <- function(id, gene_lists, selected_names) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_lists <- reactive({
      gl <- gene_lists()
      sel <- selected_names()
      req(length(sel) >= 2)
      gl[sel]
    })

    # Build list of all unique genes across selected lists for autocomplete
    all_genes <- reactive({
      sl <- selected_lists()
      sort(unique(unlist(sl)))
    })

    # Update selectize choices when gene lists change
    observeEvent(all_genes(), {
      genes <- all_genes()
      updateSelectizeInput(
        session, "search_gene",
        choices = genes,
        selected = input$search_gene,
        server = TRUE
      )
    })

    # Compute all pairwise and higher-order intersections
    all_intersections <- reactive({
      sl <- selected_lists()
      compute_intersections(sl)
    })

    output$search_results <- renderUI({
      query <- input$search_gene
      if (is.null(query) || trimws(query) == "") {
        return(div(
          style = "color: #7f8c8d; font-style: italic; padding: 20px; text-align: center;",
          "Start typing a gene name above — matching candidates will appear as you type."
        ))
      }

      query <- trimws(query)
      sl <- selected_lists()
      intersections <- all_intersections()

      # Search in the original gene lists first
      in_sets <- names(sl)[sapply(sl, function(x) {
        any(toupper(x) == toupper(query))
      })]

      # Search in intersections
      in_intersections <- names(intersections)[sapply(intersections, function(x) {
        any(toupper(x) == toupper(query))
      })]

      if (length(in_sets) == 0) {
        return(div(
          class = "alert",
          style = "background: #e74c3c; color: white; border-radius: 8px; padding: 15px; border: none;",
          icon("times-circle"),
          tags$strong(paste0(" '", query, "'")),
          " was not found in any of the selected gene lists."
        ))
      }

      tagList(
        # Show which sets contain this gene
        div(
          class = "alert",
          style = "background: #27ae60; color: white; border-radius: 8px; padding: 15px; border: none; margin-bottom: 10px;",
          icon("check-circle"),
          tags$strong(paste0(" '", query, "'")),
          " was found in ",
          tags$strong(length(in_sets)),
          " gene list(s): ",
          tags$em(paste(in_sets, collapse = ", "))
        ),

        # Show which exclusive intersections contain this gene
        if (length(in_intersections) > 0) {
          div(
            style = "margin-top: 10px;",
            h5("Exclusive Intersection Membership:",
               style = "color: #2c3e50; font-weight: 600;"),
            tags$ul(
              style = "list-style: none; padding-left: 0;",
              lapply(in_intersections, function(intname) {
                tags$li(
                  style = "padding: 8px 12px; margin-bottom: 4px; background: #ecf0f1; border-radius: 6px; border-left: 4px solid #3498db;",
                  icon("layer-group", style = "color: #3498db;"),
                  " ",
                  tags$strong(intname),
                  tags$span(
                    style = "float: right; color: #7f8c8d;",
                    paste0(length(intersections[[intname]]), " genes")
                   )
                )
              })
            )
          )
        } else {
          div(
            style = "color: #7f8c8d; font-style: italic; margin-top: 10px;",
            "This gene does not belong to any exclusive intersection."
          )
        }
      )
    })

    # --- Download handler for intersection data ---
    output$download_data <- downloadHandler(
      filename = function() {
        if (input$data_format == "xlsx") {
          "intersections.xlsx"
        } else {
          "intersections.zip"
        }
      },
      content = function(file) {
        sl <- selected_lists()
        intersections <- compute_intersections(sl)

        if (length(intersections) == 0) {
          showNotification("No intersections found!", type = "warning")
          return(NULL)
        }

        if (input$data_format == "xlsx") {
          # Save each intersection as a sheet
          sheets <- lapply(names(intersections), function(name) {
            data.frame(genes = intersections[[name]], stringsAsFactors = FALSE)
          })
          # Clean sheet names for Excel compatibility
          clean_names <- gsub("[^A-Za-z0-9_ &,.-]", "_", names(intersections))
          clean_names <- substr(clean_names, 1, 31)
          clean_names <- make.unique(clean_names, sep = "_")
          names(sheets) <- clean_names

          # Set column headers to the combination name
          for (i in seq_along(sheets)) {
            colnames(sheets[[i]]) <- clean_names[i]
          }

          write_xlsx(sheets, path = file)

        } else {
          # Save each intersection to a separate file
          tmp_dir <- tempdir()
          out_dir <- file.path(tmp_dir, "intersections")
          # Clean up from previous runs
          if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
          dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

          ext <- input$data_format
          for (name in names(intersections)) {
            clean_name <- gsub("[^A-Za-z0-9_.-]", "_", name)
            file_path <- file.path(out_dir, paste0(clean_name, ".", ext))
            if (ext == "csv") {
              write.csv(
                data.frame(gene = intersections[[name]]),
                file = file_path, row.names = FALSE
              )
            } else {
              writeLines(intersections[[name]], con = file_path)
            }
          }

          # Zip the files
          old_wd <- setwd(out_dir)
          on.exit(setwd(old_wd))
          files_to_zip <- list.files(".", full.names = TRUE)
          zip(file, files = files_to_zip)
        }
      }
    )
  })
}
