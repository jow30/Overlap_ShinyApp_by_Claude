# ============================================================
# venn_plot.R - Module for Venn diagram visualization
# ============================================================

vennPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Venn Diagram", style = "color: #2c3e50; font-weight: 600;"),
    fluidRow(
      column(
        8,
        plotOutput(ns("venn_plot"), height = "auto"),
        # Download section embedded in the plot tab
        hr(style = "border-color: #e9ecef;"),
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
          fluidRow(
            column(4, h5(icon("download"), " Download Venn Diagram",
              style = "font-weight: 600; color: #2c3e50; margin-top: 5px;"
            )),
            column(3, selectInput(ns("fig_format"), NULL,
              choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg"),
              width = "100%"
            )),
            column(3, downloadButton(ns("download_figure"), "Download",
              class = "btn-primary", style = "width: 100%; margin-top: 1px;"
            )),
            column(2)
          )
        )
      ),
      column(
        4,
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
          h5("Plot Options", style = "color: #2c3e50; font-weight: 600; margin-top: 0;"),
          textInput(ns("title"), "Title:", value = "Gene Set Overlap"),
          sliderInput(ns("width"), "Width (px):", 300, 1000, 600, step = 50),
          sliderInput(ns("height"), "Height (px):", 300, 1000, 500, step = 50),
          sliderInput(ns("alpha"), "Transparency:", 0.1, 1.0, 0.5, step = 0.05),
          sliderInput(ns("label_size"), "Label Size:", 0.5, 3.0, 1.2, step = 0.1),
          sliderInput(ns("cat_label_size"), "Category Label Size:", 0.5, 3.0, 1.0, step = 0.1),
          numericInput(ns("margin"), "Margin:", value = 0.05, min = 0, max = 0.3, step = 0.01),
          checkboxInput(ns("show_percentage"), "Show Percentages", value = FALSE),
          hr(style = "border-color: #dee2e6;"),
          h6("Colors", style = "font-weight: 600; color: #2c3e50;"),
          selectInput(ns("palette"), "Color Palette:",
            choices = names(COLOR_PALETTES),
            selected = "Tableau 10"
          )
        )
      )
    )
  )
}

vennPlotServer <- function(id, gene_lists, selected_names) {
  moduleServer(id, function(input, output, session) {
    # Get selected gene lists
    selected_lists <- reactive({
      gl <- gene_lists()
      sel <- selected_names()
      req(length(sel) > 0, length(sel) <= 5)
      gl[sel]
    })

    # ---- Color management ----
    # Colors are derived directly from the selected palette.

    # Safe helper: get the active palette (never returns NULL)
    get_palette <- reactive({
      if (!is.null(input$palette) && input$palette %in% names(COLOR_PALETTES)) {
        COLOR_PALETTES[[input$palette]]
      } else {
        DEFAULT_COLORS
      }
    })

    # The actual colors used for plotting — derived from palette
    colors <- reactive({
      sel <- selected_names()
      req(length(sel) > 0, length(sel) <= 5)
      n <- length(sel)
      pal <- get_palette()
      sapply(seq_len(n), function(i) {
        pal[((i - 1) %% length(pal)) + 1]
      })
    })

    # Helper to make a venn diagram grob
    make_venn <- function(sl, cols) {
      n <- length(sl)
      venn.diagram(
        x = sl,
        category.names = names(sl),
        filename = NULL,
        output = TRUE,
        fill = cols[1:n],
        alpha = rep(input$alpha, n),
        cex = input$label_size,
        cat.cex = input$cat_label_size,
        margin = input$margin,
        main = input$title,
        main.cex = 1.5,
        main.fontface = "bold",
        cat.fontface = "bold",
        lwd = 2,
        lty = 1
      )
    }

    # Render the Venn diagram
    output$venn_plot <- renderPlot(
      {
        sl <- selected_lists()
        req(length(sl) >= 2, length(sl) <= 5)

        cols <- colors()
        venn_fn <- make_venn(sl, cols)

        # Clean background
        grid.newpage()
        pushViewport(viewport(
          width = unit(1, "npc"), height = unit(1, "npc"),
          gp = gpar(fill = "white", col = NA)
        ))
        grid.rect(gp = gpar(fill = "white", col = NA))
        grid.draw(venn_fn)
      },
      width = function() input$width,
      height = function() input$height
    )

    # --- Download handler for Venn figure ---
    output$download_figure <- downloadHandler(
      filename = function() {
        paste0("venn_diagram.", input$fig_format)
      },
      content = function(file) {
        w <- input$width
        h <- input$height

        if (input$fig_format == "pdf") {
          pdf(file, width = w / 100, height = h / 100)
        } else if (input$fig_format == "png") {
          png(file, width = w, height = h, res = 150)
        } else {
          jpeg(file, width = w, height = h, quality = 95, res = 150)
        }

        tryCatch({
          sl <- selected_lists()
          cols <- colors()
          venn_fn <- make_venn(sl, cols)
          grid.newpage()
          grid.rect(gp = gpar(fill = "white", col = NA))
          grid.draw(venn_fn)
        }, finally = {
          dev.off()
        })
      }
    )

    # Return the plot info for external use (kept for compatibility)
    reactive({
      list(
        plot_fn = function() {
          sl <- selected_lists()
          cols <- colors()
          venn_fn <- make_venn(sl, cols)
          grid.newpage()
          grid.rect(gp = gpar(fill = "white", col = NA))
          grid.draw(venn_fn)
        },
        width = input$width,
        height = input$height
      )
    })
  })
}
