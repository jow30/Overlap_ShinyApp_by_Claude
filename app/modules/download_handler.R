# ============================================================
# download_handler.R - Module for figure and data download
# ============================================================

#' Compute all intersections from a named list of gene sets
compute_intersections <- function(gene_lists) {
  if (length(gene_lists) < 2) return(list())

  set_names <- names(gene_lists)
  n <- length(set_names)
  intersections <- list()

  # Generate all possible combinations (1 to n sets at a time)
  for (k in 1:n) {
    combos <- combn(set_names, k, simplify = FALSE)
    for (combo in combos) {
      # Find genes in ALL sets of this combo
      genes_in_combo <- Reduce(intersect, gene_lists[combo])
      # Remove genes that are also in sets NOT in this combo
      other_sets <- setdiff(set_names, combo)
      if (length(other_sets) > 0) {
        genes_in_others <- unique(unlist(gene_lists[other_sets]))
        exclusive_genes <- setdiff(genes_in_combo, genes_in_others)
      } else {
        exclusive_genes <- genes_in_combo
      }

      if (length(exclusive_genes) > 0) {
        # Use " & " as separator — safe for filenames, Excel, and display
        combo_name <- paste(combo, collapse = " & ")
        intersections[[combo_name]] <- exclusive_genes
      }
    }
  }

  intersections
}

downloadHandlerUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Downloads", style = "color: #2c3e50; font-weight: 600;"),
    fluidRow(
      column(6,
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
          h5("Download Figures", style = "font-weight: 600; color: #2c3e50;"),
          selectInput(ns("fig_format"), "Format:",
                      choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg")),
          selectInput(ns("fig_type"), "Figure:",
                      choices = c("Venn Diagram" = "venn", "UpSet Plot" = "upset")),
          downloadButton(ns("download_figure"), "Download Figure",
                         class = "btn-primary btn-block")
        )
      ),
      column(6,
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
          h5("Download Intersections", style = "font-weight: 600; color: #2c3e50;"),
          selectInput(ns("data_format"), "Format:",
                      choices = c("Excel (.xlsx)" = "xlsx",
                                  "CSV (separate files)" = "csv",
                                  "Text (separate files)" = "txt")),
          downloadButton(ns("download_data"), "Download Intersections",
                         class = "btn-primary btn-block")
        )
      )
    )
  )
}

downloadHandlerServer <- function(id, gene_lists, selected_names,
                                   venn_info, upset_info) {
  moduleServer(id, function(input, output, session) {

    selected_lists <- reactive({
      gl <- gene_lists()
      sel <- selected_names()
      req(length(sel) >= 2)
      gl[sel]
    })

    # --- Figure download ---
    output$download_figure <- downloadHandler(
      filename = function() {
        paste0(input$fig_type, "_plot.", input$fig_format)
      },
      content = function(file) {
        info <- if (input$fig_type == "venn") venn_info() else upset_info()
        w <- info$width / 100
        h <- info$height / 100

        if (input$fig_format == "pdf") {
          pdf(file, width = w, height = h)
        } else if (input$fig_format == "png") {
          png(file, width = info$width, height = info$height, res = 150)
        } else {
          jpeg(file, width = info$width, height = info$height,
               quality = 95, res = 150)
        }

        tryCatch({
          info$plot_fn()
        }, finally = {
          dev.off()
        })
      }
    )

    # --- Data download ---
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
          # Clean sheet names for Excel compatibility:
          # Remove any characters not safe for sheet names
          clean_names <- gsub("[^A-Za-z0-9_ &,.-]", "_", names(intersections))
          clean_names <- substr(clean_names, 1, 31)
          # Ensure unique names
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
